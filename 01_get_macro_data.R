install.packages(c("shiny","tidyverse","DT","lubridate","fitdistrplus"))
install.packages(c("future","future.apply","httr","jsonlite","MASS","glmnet","fitdistrplus"))
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

if(!dir.exists("data")) dir.create("data")

fred_key <- "33f55a9a675be65302b1115c9521c376"

fetch_fred <- function(series_id) {
  url <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?series_id=",
    series_id,
    "&api_key=", fred_key,
    "&file_type=json"
  )
  
  res <- fromJSON(url)$observations
  tibble(date = ymd(res$date), value = as.numeric(res$value))
}

cat("Downloading macroeconomic data...\n")

gdp  <- fetch_fred("GDPC1")  %>% rename(gdp = value)
cpi  <- fetch_fred("CPIAUCSL") %>% rename(cpi = value)
unemp <- fetch_fred("UNRATE") %>% rename(unemp = value)
ffr  <- fetch_fred("DFF") %>% rename(ffr = value)

macro <- reduce(list(gdp, cpi, unemp, ffr), left_join, by = "date") %>% drop_na()

write_csv(macro, "data/macro.csv")

macro %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  labs(title = "Macro Indicators from FRED", x = "", y = "")
