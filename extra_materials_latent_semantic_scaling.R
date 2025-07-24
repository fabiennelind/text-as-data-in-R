# Latent Semantic Scaling (LSS) in R
# June 2025

# script based on: https://koheiw.github.io/LSX/articles/pkgdown/basic.html

# All the details
# Watanabe, K. (2021). Latent semantic scaling: A semisupervised text analysis technique for new domains and languages. Communication Methods and Measures, 15(2), 81-102.

# The model builds on ideas from word embeddings and dictionary-based sentiment analysis

library(LSX)
library(quanteda)

articles_en <- read.csv("https://raw.githubusercontent.com/fabiennelind/text-as-data-in-R/refs/heads/main/data/news_migration.csv")
corp <- corpus(articles_en, text_field = "headline")

toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, 
               remove_numbers = TRUE, remove_url = TRUE, tolower = TRUE)
dfmt <- dfm(toks) |> 
  dfm_remove(stopwords("en"))

# build-in seed words in quanteda
seed <- as.seedwords(data_dictionary_sentiment)
print(seed) 

# computes the polarity scores of all the words in the corpus based on their semantic similarity to the seed words
lss <- textmodel_lss(dfmt, seeds = seed, k = 300, cache = TRUE, # size of word vectors k = 300
                     include_data = TRUE, group_data = TRUE) # word meanings and distances are learned entirely from your corpus

# Visualization of seed words + random words
library(ggplot2)
textplot_terms(lss, highlighted = NULL) #highlighted = NULL, it randomly samples 50 words and highlights them

# Visualization of seed words + specified words
ukip <- featnames(dfm_select(dfmt, "ukip", valuetype = "regex"))
textplot_terms(lss, highlighted = c(refugee, names(seed)))

#Predict the polarity of documents
dat <- docvars(lss$data)
dat$lss <- predict(lss)
print(nrow(dat))

dat$publication_date <-as.Date(dat$publication_date)

#smooth polarity scores
smo <- smooth_lss(dat, lss_var = "lss", date_var = "publication_date")

#Plot over time

ggplot(smo, aes(x = date, y = fit)) + 
  geom_line() +
  theme_minimal() +
  geom_ribbon(aes(ymin = fit - se.fit * 1.96, ymax = fit + se.fit * 1.96), alpha = 0.1) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dotted") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(title = "Polarity in UK Refugee/Asylum Coverage", x = "Date", y = "Polarity")

# Excercise: Apply the model to your own dataset or to another dataset we used in class




