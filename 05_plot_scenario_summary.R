library(tidyverse)
summary <- read_csv("output/scenario_losses_summary.csv")

summary %>%
  ggplot(aes(x = reorder(text, VaR), y = VaR)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Scenario VaR Comparison",
    x = "Headline Scenario",
    y = "Portfolio VaR (stress outcome)"
  )
