library(tidyverse)
library(reticulate)

# Activate Python env
use_virtualenv("C:\\Users\\mehta\\OneDrive\\Desktop\\LLM Proj\\venv", required = TRUE)

# Import SentenceTransformer
sentence_transformers <- import("sentence_transformers")
np <- import("numpy")

model <- sentence_transformers$SentenceTransformer('all-MiniLM-L6-v2')

# Example economic news headlines (you can add more later)
news_text <- c(
  "Central bank signals potential interest rate cuts due to slowing inflation.",
  "Unemployment rate rises unexpectedly as job market weakens.",
  "GDP growth forecasts revised downward amid global demand slowdown.",
  "Energy prices surge following geopolitical tensions.",
  "Consumer confidence improves as financial conditions ease.",
  "Corporate bankruptcy filings increase as borrowing costs remain elevated.",
  "Manufacturing output contracts for third straight month.",
  "Stock markets rally as recession fears decline."
)

# Convert to embeddings
emb_matrix <- model$encode(news_text)
emb <- as.matrix(emb_matrix)

# Compute sentiment-style macro index (cosine similarity to positive anchor)
anchor_pos <- model$encode("economic stability and growth")
anchor_neg <- model$encode("recession, inflation and financial stress")

cos_sim <- function(x, y) sum(x*y) / (sqrt(sum(x*x))*sqrt(sum(y*y)))

macro_score <- sapply(1:nrow(emb), function(i){
  pos <- cos_sim(emb[i,], anchor_pos)
  neg <- cos_sim(emb[i,], anchor_neg)
  pos - neg   # net optimism index
})

results <- tibble(
  text = news_text,
  risk_score = macro_score
)

# save embeddings + scores
dir.create("data/embeddings", showWarnings = FALSE)
write_csv(as_tibble(emb), "data/embeddings/news_embeddings.csv")
write_csv(results, "data/embeddings/news_risk_scores.csv")

print("✅ Embeddings + risk scores saved")
print(results)
