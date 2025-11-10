# 04_macro_llm_scenarios.R
# Build scenario-level portfolio loss distributions by mapping LLM risk scores to macro shocks,
# re-evaluating the Poisson frequency model, and Monte-Carlo simulating losses.
#
# Assumes the following files exist:
# - data/macro.csv
# - data/policies_with_macro.csv
# - models/poisson_macro_model.rds
# - data/embeddings/news_risk_scores.csv
# - (optional) data/claims.csv  --> used to estimate severity distribution
#
# Outputs:
# - output/scenario_losses_summary.csv
# - output/scenario_loss_distributions.rds (list of loss vectors)
# - output/plots/*.png

library(tidyverse)
library(fitdistrplus)
library(lubridate)

set.seed(2025)

# ---------------------------
# Parameters you can tweak
# ---------------------------
n_sim <- 2000           # Monte Carlo iterations per scenario
shock_strength <- 1.0   # scales how strongly LLM score moves macro vars
vaR_p <- 0.995          # VaR probability

# ---------------------------
# Helpers
# ---------------------------
safe_dir <- function(p) if(!dir.exists(p)) dir.create(p, recursive = TRUE)

# Cosine sign mapping: LLM score negative => worse macro (we use mapping below)
# ---------------------------

# ---------------------------
# 1) Load inputs
# ---------------------------
cat("Loading inputs...\n")
macro <- read_csv("data/macro.csv", show_col_types = FALSE)
policies <- read_csv("data/policies_with_macro.csv", show_col_types = FALSE)
model <- readRDS("models/poisson_macro_model.rds")
news_scores <- read_csv("data/embeddings/news_risk_scores.csv", show_col_types = FALSE)

# Optional: severity data (if exists)
severity_exists <- file.exists("data/claims.csv")
if(severity_exists){
  claims_df <- read_csv("data/claims.csv", show_col_types = FALSE)
  # Try to extract positive severities
  sev_sample <- claims_df$severity[which(!is.na(claims_df$severity) & claims_df$severity > 0)]
  # Fit lognormal on observed severities (on original dollar scale)
  fit_sev <- fitdist(sev_sample, "lnorm")
  sev_meanlog <- fit_sev$estimate["meanlog"]
  sev_sdlog <- fit_sev$estimate["sdlog"]
  cat("Fitted severity (lognormal) from data: meanlog=", round(sev_meanlog,3),
      " sdlog=", round(sev_sdlog,3), "\n")
} else {
  # fallback to the synthetic parameters used earlier
  sev_meanlog <- 8
  sev_sdlog <- 0.6
  cat("No claims.csv found — using fallback severity meanlog=", sev_meanlog,
      " sdlog=", sev_sdlog, "\n")
}

# ---------------------------
# 2) Compute macro variability (IMPROVED)
# ---------------------------
# Convert GDP level -> annual percent growth (or monthly pct if your series is monthly)
# We will compute pct change (100 * diff / lag)
macro2 <- read_csv("data/macro.csv", show_col_types = FALSE) %>%
  arrange(date) %>%
  mutate(
    gdp_pct = 100 * (gdp / lag(gdp) - 1),
    unemp_diff = unemp - lag(unemp),   # use changes for unemployment
    ffr_diff = ffr - lag(ffr)          # use changes for rates
  ) %>%
  drop_na()

sd_gdp_pct <- sd(macro2$gdp_pct, na.rm = TRUE)
sd_unemp_diff <- sd(macro2$unemp_diff, na.rm = TRUE)
sd_ffr_diff <- sd(macro2$ffr_diff, na.rm = TRUE)

cat("Macro SDs (improved): gdp_pct =", round(sd_gdp_pct,3),
    " unemp_diff =", round(sd_unemp_diff,3),
    " ffr_diff =", round(sd_ffr_diff,3), "\n")

# ---------------------------
# 3) Scenario mapping function (IMPROVED)
# ---------------------------
# Map LLM score to *plausible percent / point changes* using SD of growth/changes.
# We'll also cap shocks to avoid extreme moves.
map_score_to_shock <- function(score, shock_strength = 1.0,
                               cap_gdp_pct = 5.0,  # cap GDP pct change at +/-5%
                               cap_unemp = 3.0,    # cap unemployment change in percentage points
                               cap_ffr = 2.0) {    # cap rate change in percentage points
  # score: higher -> positive (good econ), lower -> negative (bad)
  # Map to changes (note the sign convention: negative score => worse => gdp down)
  d_gdp_pct <- -score * sd_gdp_pct * shock_strength
  d_unemp_pct <- -score * sd_unemp_diff * shock_strength   # negative score -> increase in unemp
  d_ffr_pct <- -score * sd_ffr_diff * shock_strength       # negative score -> upward pressure on rates
  
  # Cap the shocks to plausible ranges
  d_gdp_pct <- pmax(pmin(d_gdp_pct, cap_gdp_pct), -cap_gdp_pct)
  d_unemp_pct <- pmax(pmin(d_unemp_pct, cap_unemp), -cap_unemp)
  d_ffr_pct <- pmax(pmin(d_ffr_pct, cap_ffr), -cap_ffr)
  
  list(d_unemp = d_unemp_pct, d_gdp = d_gdp_pct, d_ffr = d_ffr_pct)
}


# ---------------------------
# 4) Scenario simulation loop
# ---------------------------
safe_dir("output")
safe_dir("output/plots")

scenario_results <- list()
loss_distributions <- list()

# We'll simulate for each news row (each headline / scenario)
for(i in seq_len(nrow(news_scores))){
  row <- news_scores[i, ]
  scenario_name <- paste0("scenario_", i)
  text <- row$text
  score <- as.numeric(row$risk_score)
  cat("Running scenario", i, "score=", round(score,4), "text: ", substr(text,1,80), "...\n")
  
  shocks <- map_score_to_shock(score, shock_strength = shock_strength)
  
  # Create shocked policy-level dataset
  policies_shocked <- policies %>%
    mutate(
      unemp = unemp + shocks$d_unemp,               # d_unemp in percentage points (works with level unemployment)
      gdp = gdp * (1 + shocks$d_gdp / 100),         # d_gdp is percent, so convert
      ffr = ffr + shocks$d_ffr                       # d_ffr in percentage points
    )
  
  
  # Predict new expected counts (lambda) using the fitted Poisson model
  # Important: predict uses the same covariate names as model formula
  lambda_hat <- predict(model, newdata = policies_shocked, type = "response")
  policies_shocked <- policies_shocked %>% mutate(lambda_hat = lambda_hat)
  
  # Monte Carlo simulate portfolio losses
  total_losses <- numeric(n_sim)
  n_policies <- nrow(policies_shocked)
  
  # Precompute mean severity expectation for speed (not used directly for simulation)
  mean_severity <- exp(sev_meanlog + 0.5 * sev_sdlog^2)
  
  for(sim in seq_len(n_sim)){
    # Simulate claim counts per policy
    counts <- rpois(n_policies, lambda = policies_shocked$lambda_hat)
    total_claims <- sum(counts)
    if(total_claims == 0){
      total_losses[sim] <- 0
    } else {
      # sample severity for each claim
      sev_draws <- rlnorm(total_claims, meanlog = sev_meanlog, sdlog = sev_sdlog)
      total_losses[sim] <- sum(sev_draws)
    }
  }
  
  # Compute VaR & ES
  var_val <- as.numeric(quantile(total_losses, probs = vaR_p, names = FALSE, type = 7))
  es_val <- mean(total_losses[total_losses >= var_val])
  mean_loss <- mean(total_losses)
  sd_loss <- sd(total_losses)
  
  scenario_results[[scenario_name]] <- tibble(
    scenario_id = i,
    text = text,
    score = score,
    d_unemp = shocks$d_unemp,
    d_gdp = shocks$d_gdp,
    d_ffr = shocks$d_ffr,
    mean_loss = mean_loss,
    sd_loss = sd_loss,
    VaR = var_val,
    ES = es_val
  )
  loss_distributions[[scenario_name]] <- total_losses
  
  # Save quick histogram plot
  png(filename = file.path("output/plots", paste0("loss_hist_scenario_", i, ".png")), width = 900, height = 600)
  hist(total_losses, breaks = 60, main = paste0("Loss Distribution - Scenario ", i),
       xlab = "Total Loss (portfolio)", ylab = "Frequency")
  abline(v = var_val, col = "red", lwd = 2)
  legend("topright", legend = c(paste0("VaR ", round(vaR_p*100,1), "% = ", round(var_val,2)),
                                paste0("ES = ", round(es_val,2))),
         col = c("red", "black"), lwd = c(2,1), bty = "n")
  dev.off()
  
  cat("  -> mean loss:", round(mean_loss,2), "VaR:", round(var_val,2), "ES:", round(es_val,2), "\n")
}

# ---------------------------
# 5) Save results
# ---------------------------
scenario_summary <- bind_rows(scenario_results)
write_csv(scenario_summary, "output/scenario_losses_summary.csv")
saveRDS(loss_distributions, "output/scenario_loss_distributions.rds")

cat("Saved scenario summary to output/scenario_losses_summary.csv\n")
cat("Saved loss distributions to output/scenario_loss_distributions.rds\n")
cat("Plots in output/plots/\n")

# Print summary table
print(scenario_summary)

# End of script
