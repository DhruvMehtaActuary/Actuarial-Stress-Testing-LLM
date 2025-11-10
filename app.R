# app.R - Shiny app with choice: automatic example files OR user uploads
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)

# ----------------------
# Helpers (same logic as before)
# ----------------------
map_score_to_shock <- function(score, sd_gdp_pct, sd_unemp_diff, sd_ffr_diff,
                               shock_strength = 1.0,
                               cap_gdp_pct = 5.0, cap_unemp = 3.0, cap_ffr = 2.0) {
  d_gdp_pct <- -score * sd_gdp_pct * shock_strength
  d_unemp_pct <- -score * sd_unemp_diff * shock_strength
  d_ffr_pct <- -score * sd_ffr_diff * shock_strength
  
  d_gdp_pct <- pmax(pmin(d_gdp_pct, cap_gdp_pct), -cap_gdp_pct)
  d_unemp_pct <- pmax(pmin(d_unemp_pct, cap_unemp), -cap_unemp)
  d_ffr_pct <- pmax(pmin(d_ffr_pct, cap_ffr), -cap_ffr)
  
  list(d_unemp = d_unemp_pct, d_gdp = d_gdp_pct, d_ffr = d_ffr_pct)
}

simulate_scenario_simple <- function(model, policies, score, text,
                                     n_sim = 1000, sev_meanlog = 8, sev_sdlog = 0.6,
                                     shock_strength = 1.0,
                                     sd_gdp_pct = 1.5, sd_unemp_diff = 0.5, sd_ffr_diff = 0.3) {
  shocks <- map_score_to_shock(score, sd_gdp_pct, sd_unemp_diff, sd_ffr_diff, shock_strength)
  
  policies_shocked <- policies %>%
    mutate(
      unemp = unemp + shocks$d_unemp,
      gdp = gdp * (1 + shocks$d_gdp / 100),
      ffr = ffr + shocks$d_ffr
    )
  
  lambda_hat <- tryCatch(
    predict(model, newdata = policies_shocked, type = "response"),
    error = function(e) rep(0, nrow(policies_shocked))
  )
  policies_shocked$lambda_hat <- lambda_hat
  n_policies <- nrow(policies_shocked)
  
  total_losses <- numeric(n_sim)
  for(sim in seq_len(n_sim)) {
    counts <- rpois(n_policies, lambda = policies_shocked$lambda_hat)
    total_claims <- sum(counts)
    if(total_claims == 0) {
      total_losses[sim] <- 0
    } else {
      sev_draws <- rlnorm(total_claims, meanlog = sev_meanlog, sdlog = sev_sdlog)
      total_losses[sim] <- sum(sev_draws)
    }
  }
  
  var_val <- as.numeric(quantile(total_losses, probs = 0.995, names = FALSE, type = 7))
  es_val <- if(sum(total_losses >= var_val) == 0) NA_real_ else mean(total_losses[total_losses >= var_val])
  
  list(
    scenario_text = text,
    score = score,
    d_unemp = shocks$d_unemp,
    d_gdp = shocks$d_gdp,
    d_ffr = shocks$d_ffr,
    losses = total_losses,
    mean_loss = mean(total_losses),
    sd_loss = sd(total_losses),
    VaR = var_val,
    ES = es_val
  )
}

# ----------------------
# UI
# ----------------------
ui <- fluidPage(
  titlePanel("Actuarial Stress Testing — Example or Upload"),
  # ---- Short summary panel (paste this right after titlePanel(...)) ----
  wellPanel(
    h4("Quick summary"),
    p("This app links macroeconomic data, short news signals (LLM), and actuarial loss simulations."),
    tags$ul(
      tags$li(strong("What it does:"), " Converts news headlines into a risk score, maps that to macro shocks, and simulates portfolio losses (Poisson frequency & Lognormal severity)."),
      tags$li(strong("How to use:"), " Choose 'Use example files' or 'Upload my own files', set Monte Carlo sims and shock strength, then click 'Run scenarios'."),
      tags$li(strong("Files you may upload:"), " policies CSV, news scores CSV, and a fitted Poisson model (.rds)."),
      tags$li(strong("For more instructions, refer to the README on the github/platform"))
    ),
    p(em("Tip: Use example files for a quick demo; upload your own files to test real portfolios."))
  ),
  # ----------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      radioButtons("file_choice", "Choose data source:",
                   choices = c("Use example files (auto-load)" = "example",
                               "Upload my own files" = "upload"),
                   selected = "example"),
      # upload inputs (shown only when upload selected)
      conditionalPanel(
        condition = "input.file_choice == 'upload'",
        fileInput("policies_file", "Upload policies_with_macro.csv", accept = ".csv"),
        fileInput("news_file", "Upload news_risk_scores.csv", accept = ".csv"),
        fileInput("model_file", "Upload poisson_macro_model.rds", accept = ".rds")
      ),
      hr(),
      numericInput("n_sim", "Monte Carlo sims per scenario", value = 1000, min = 100, step = 100),
      numericInput("shock_strength", "Shock strength (scale)", value = 1.0, step = 0.1),
      actionButton("run", "Run scenarios"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 br(),
                 h4("What this app does (simple)"),
                 p("This app demonstrates how macroeconomics, short news summaries (LLM), and actuarial models can be combined to stress-test an insurance portfolio."),
                 tags$ul(
                   tags$li(strong("Step 1 — Macro data:"), " We use economic series like GDP, CPI, unemployment and interest rates."),
                   tags$li(strong("Step 2 — LLM news signals:"), " Short economic headlines are converted into a numeric 'risk score' using a language model."),
                   tags$li(strong("Step 3 — Shock mapping:"), " The LLM risk score is translated into plausible changes (shocks) in macro variables."),
                   tags$li(strong("Step 4 — Actuarial simulation:"), " We update policy inputs with those shocks, predict claim counts, and run Monte Carlo simulations to get loss distributions.")
                 ),
                 br(),
                 h5("Quick guide to use"),
                 tags$ol(
                   tags$li("Choose whether to use the example files included in the project or upload your own files."),
                   tags$li("Set the number of Monte Carlo simulations (n_sim) and the shock strength (how strongly the LLM score affects macro variables)."),
                   tags$li("Click 'Run scenarios' and wait for the app to finish (progress bar will appear)."),
                   tags$li("Explore the summary table, VaR barplot, and histogram. Download results if needed.")
                 ),
                 br(),
                 p(em("Tip: If example files are missing, switch to 'Upload my own files' and upload CSV / RDS files."))),
        
        tabPanel("Methodology",
                 br(),
                 h4("Plain-language description of the maths"),
                 p("Below is a brief, non-technical explanation of how the simulation works. You do not need to understand every math symbol to use the app; this is just for context."),
                 tags$ul(
                   tags$li(strong("LLM → Risk Score:"), " A language model (sentence embedding) converts each news headline into a number that represents economic optimism vs pessimism."),
                   tags$li(strong("Map score to macro shock:"), " The risk score is scaled to move macro variables. For example, a negative score might decrease GDP growth by X% and increase unemployment by Y percentage points."),
                   tags$li(strong("Policy re-pricing:"), " We apply those shocked macro values to each policy's baseline macro columns (gdp, unemp, ffr)."),
                   tags$li(strong("Predict claim frequency:"), " A Poisson regression model (trained earlier) predicts the expected number of claims per policy under the shocked conditions."),
                   tags$li(strong("Simulate claim counts and sizes:"), " For each simulation round we draw claim counts from a Poisson distribution and claim severities from a Lognormal distribution, then sum claims to get total portfolio loss."),
                   tags$li(strong("Aggregate and summarize:"), " Repeat the simulation many times to form a loss distribution. From that distribution we compute metrics like VaR (Value at Risk) and ES (Expected Shortfall).")
                 ),
                 br(),
                 h5("Interpreting outputs"),
                 tags$ul(
                   tags$li(strong("Mean loss:"), " The average total loss across simulations."),
                   tags$li(strong("VaR (0.995):"), " The loss level which is exceeded only 0.5% of the time — think of it as a high-loss threshold."),
                   tags$li(strong("ES (Tail mean):"), " The average loss conditional on exceeding the VaR — shows tail severity.")
                 ),
                 br(),
                 h5("Caveats & assumptions (read before sharing)"),
                 tags$ul(
                   tags$li("The LLM risk score is a proxy and should be validated — it is not a perfect signal of future macro moves."),
                   tags$li("Shock mapping uses historical variability to keep moves plausible; you can change shock strength to test sensitivity."),
                   tags$li("Synthetic policy data may not reflect your real portfolio — results are illustrative unless you provide real, cleaned policy data.")
                 )),
        
        tabPanel("Summary table", DTOutput("summary_tbl")),
        tabPanel("VaR barplot", plotOutput("var_plot", height = "450px")),
        tabPanel("Histogram (first scenario)", plotOutput("hist_plot")),
        tabPanel("Download", downloadButton("download_summary", "Download summary CSV"))
      ),
      width = 9
    )
  )
)

# ----------------------
# Server
# ----------------------
server <- function(input, output, session) {
  # reactive holders
  policies_r <- reactiveVal(NULL)
  news_r <- reactiveVal(NULL)
  model_r <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  # helper to compute macro sd values
  compute_macro_sds <- function() {
    if(file.exists("data/macro.csv")) {
      macro <- read_csv("data/macro.csv", show_col_types = FALSE)
      macro2 <- macro %>% arrange(date) %>%
        mutate(gdp_pct = 100*(gdp/lag(gdp)-1), unemp_diff = unemp - lag(unemp), ffr_diff = ffr - lag(ffr)) %>%
        drop_na()
      sd_gdp_pct <- sd(macro2$gdp_pct, na.rm = TRUE)
      sd_unemp_diff <- sd(macro2$unemp_diff, na.rm = TRUE)
      sd_ffr_diff <- sd(macro2$ffr_diff, na.rm = TRUE)
    } else {
      # defaults if no macro.csv found
      sd_gdp_pct <- 1.5; sd_unemp_diff <- 0.5; sd_ffr_diff <- 0.3
    }
    list(sd_gdp_pct = sd_gdp_pct, sd_unemp_diff = sd_unemp_diff, sd_ffr_diff = sd_ffr_diff)
  }
  
  # If example chosen, load files immediately
  observe({
    if(input$file_choice == "example") {
      # Try to load example files; if missing, show message but allow upload option
      ok <- TRUE
      if(!file.exists("data/policies_with_macro.csv")) {
        showNotification("Example policies file not found: data/policies_with_macro.csv", type = "warning")
        ok <- FALSE
      } else {
        policies_r(read_csv("data/policies_with_macro.csv", show_col_types = FALSE))
      }
      if(!file.exists("data/embeddings/news_risk_scores.csv")) {
        showNotification("Example news scores not found: data/embeddings/news_risk_scores.csv", type = "warning")
        ok <- FALSE
      } else {
        news_r(read_csv("data/embeddings/news_risk_scores.csv", show_col_types = FALSE))
      }
      if(!file.exists("models/poisson_macro_model.rds")) {
        showNotification("Example model not found: models/poisson_macro_model.rds", type = "warning")
        ok <- FALSE
      } else {
        model_r(readRDS("models/poisson_macro_model.rds"))
      }
      # If any example file missing, we still let user switch to upload
    }
  })
  
  # If user uploads files, store them in reactives
  observeEvent(input$policies_file, {
    req(input$policies_file)
    policies_r(read_csv(input$policies_file$datapath, show_col_types = FALSE))
    showNotification("Policies file uploaded", type = "message")
  })
  observeEvent(input$news_file, {
    req(input$news_file)
    news_r(read_csv(input$news_file$datapath, show_col_types = FALSE))
    showNotification("News file uploaded", type = "message")
  })
  observeEvent(input$model_file, {
    req(input$model_file)
    model_r(readRDS(input$model_file$datapath))
    showNotification("Model file uploaded", type = "message")
  })
  
  # Run scenarios (button)
  observeEvent(input$run, {
    # get current data source
    pol <- policies_r()
    nws <- news_r()
    mdl <- model_r()
    
    # basic checks
    if(is.null(pol) || is.null(nws) || is.null(mdl)) {
      showModal(modalDialog(
        title = "Missing data",
        "Please ensure policies, news scores and model are available (either choose example or upload files).",
        easyClose = TRUE
      ))
      return()
    }
    if(!all(c("gdp","unemp","ffr") %in% names(pol))) {
      showModal(modalDialog(title = "Bad policies file", "policies must contain columns: gdp, unemp, ffr", easyClose = TRUE))
      return()
    }
    if(!all(c("text","risk_score") %in% names(nws))) {
      showModal(modalDialog(title = "Bad news file", "news scores file must contain columns: text, risk_score", easyClose = TRUE))
      return()
    }
    
    sds <- compute_macro_sds()
    sd_gdp_pct <- sds$sd_gdp_pct; sd_unemp_diff <- sds$sd_unemp_diff; sd_ffr_diff <- sds$sd_ffr_diff
    
    n_sim <- as.integer(input$n_sim)
    shock_strength <- as.numeric(input$shock_strength)
    res_list <- vector("list", nrow(nws))
    
    withProgress(message = "Running scenarios...", value = 0, {
      for(i in seq_len(nrow(nws))) {
        incProgress(1/nrow(nws), detail = paste("Scenario", i))
        row <- nws[i, ]
        out <- simulate_scenario_simple(
          model = mdl,
          policies = pol,
          score = as.numeric(row$risk_score),
          text = as.character(row$text),
          n_sim = n_sim,
          shock_strength = shock_strength,
          sd_gdp_pct = sd_gdp_pct,
          sd_unemp_diff = sd_unemp_diff,
          sd_ffr_diff = sd_ffr_diff
        )
        res_list[[i]] <- out
      }
    })
    
    df <- map_dfr(res_list, function(x) tibble(
      text = x$scenario_text,
      score = x$score,
      d_unemp = x$d_unemp,
      d_gdp = x$d_gdp,
      d_ffr = x$d_ffr,
      mean_loss = x$mean_loss,
      sd_loss = x$sd_loss,
      VaR = x$VaR,
      ES = x$ES
    ))
    results(list(raw = res_list, summary = df))
    showNotification("Simulations completed successfully", type = "message")
  })
  
  # Outputs
  output$summary_tbl <- renderDT({
    req(results())
    datatable(results()$summary, options = list(pageLength = 8))
  })
  
  output$var_plot <- renderPlot({
    req(results())
    results()$summary %>%
      mutate(short = ifelse(nchar(text) > 60, paste0(substr(text,1,57),"..."), text)) %>%
      ggplot(aes(x = reorder(short, VaR), y = VaR)) +
      geom_col() + coord_flip() +
      labs(x = "Scenario (headline)", y = "Portfolio VaR (0.995)") +
      theme_minimal()
  })
  
  output$hist_plot <- renderPlot({
    req(results())
    raw <- results()$raw
    if(length(raw) < 1) return(NULL)
    losses <- raw[[1]]$losses
    hist(losses, breaks = 60, main = results()$summary$text[1], xlab = "Total loss", ylab = "Frequency")
  })
  
  output$download_summary <- downloadHandler(
    filename = function() paste0("scenario_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(results())
      write_csv(results()$summary, file)
    }
  )
}

# ----------------------
# Run app
# ----------------------
shinyApp(ui, server)
