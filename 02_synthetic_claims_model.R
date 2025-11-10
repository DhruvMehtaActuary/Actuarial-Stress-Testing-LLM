library(tidyverse)
library(lubridate)
library(MASS)
library(glmnet)

# Load macro data
macro <- read_csv("data/macro.csv") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    gdp = mean(gdp, na.rm = TRUE),
    cpi = mean(cpi, na.rm = TRUE),
    unemp = mean(unemp, na.rm = TRUE),
    ffr = mean(ffr, na.rm = TRUE)
  )


# Create synthetic policy exposure data (5,000 policies)
set.seed(123)
n <- 5000

policies <- tibble(
  policy_id = 1:n,
  start_date = sample(seq.Date(from = as.Date("2018-01-01"), to = as.Date("2022-12-31"), by = "month"), n, replace = TRUE)
) %>%
  mutate(
    year = year(start_date),
    exposure = runif(n, 0.5, 1),   # 6–12 months exposure
    age = sample(18:80, n, replace = TRUE),
    vehicle_value = rgamma(n, shape = 6, scale = 30000),  # approx INR 1.8L mean
    credit_score = rnorm(n, mean = 700, sd = 50)
  )

# Join macro data (match by year)
policies <- policies %>%
  left_join(macro, by = "year")

# Adjusted parameters to create realistic variation
beta0 <- -1.5
beta_age <- 0.01
beta_exposure <- 1.2
beta_unemp <- 0.08
beta_gdp <- -0.03
beta_rate <- -0.01

# Calculate lambda
policies <- policies %>%
  mutate(
    lambda = exp(beta0 +
                   beta_age * (age - mean(age)) / sd(age) +
                   beta_exposure * exposure +
                   beta_unemp * (unemp - mean(unemp)) / sd(unemp) +
                   beta_gdp * (gdp - mean(gdp)) / sd(gdp) +
                   beta_rate * (ffr - mean(ffr)) / sd(ffr))
  )


# Generate Poisson claims
policies <- policies %>%
  mutate(
    claims = rpois(n, lambda)
  )

# Fit base Poisson model
model <- glm(claims ~ age + exposure + unemp + gdp + ffr, 
             data = policies, family = poisson())

summary(model)

# Save
write_csv(policies, "data/policies_with_macro.csv")
dir.create("models", showWarnings = FALSE)
saveRDS(model, "models/poisson_macro_model.rds")

# Quick plot: unemployment vs avg claims
policies %>%
  group_by(unemp) %>%
  summarise(freq = mean(claims)) %>%
  ggplot(aes(unemp, freq)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Claims frequency rises in recessions (higher unemployment)")
