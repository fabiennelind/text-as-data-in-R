# How to classify texts with Ollama models
# Inspiration, more information: # https://jbgruber.github.io/rollama/articles/annotation.html#example-using-a-dataframe


library(rollama)
library(tidyverse) 

# set default model
options(rollama_model = "llama3.2:3b-instruct-q8_0") 
pull_model()

# optimized for multilingual dialogue
# Supported Languages: English, German, French, Italian, Portuguese, Hindi, Spanish, and Thai are officially supported. 
# Llama 3.2 has been trained on a broader collection of languages than these 8 supported languages.


library(tibble)
library(purrr)

# Test with small toy dataframe

# Create an example dataframe with 5 movie reviews
movie_reviews <- tibble::tibble(
  review_id = 1:5,
  review = c("A stunning visual spectacle with a gripping storyline  forest protection.",
             "The plot was predictable, but the acting was superb for the environment.",
             "An overrated film with underwhelming performances",
             "A beautiful tale of love and adventure, beautifully shot and sustainable energy.",
             "The movie lacked depth, but the special effects were incredible.")
)

# Process each review using make_query
queries <- make_query(
  text = movie_reviews$review,
  prompt = "Categories: climate action/environment protection, Other", # 
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the topic of the text. Answer with just one of the correct category.",
  prefix = "Text to classify: "
)

# Process and annotate the movie reviews
movie_reviews$annotation <- query(queries, screen = FALSE, output = "text")

# it works but is quite unstable (Check out, what happens when you re-run)

#################
# with other data: News Articles with Manual Labels (Climate Change: 1 = Yes, 0 = No)
##################

df_coded = read.csv("https://raw.githubusercontent.com/fabiennelind/text-as-data-in-R/refs/heads/main/data/data_climate.csv")

colnames(df_coded)


# Use half of the annotated data to develop the set-up (df_test), use the other to validate the final set-up

set.seed(42)  # optional for reproducibility
idx_test <- sample(nrow(df_coded), size = 25, replace = FALSE)

df_test <- df_coded[idx_test, , drop = FALSE]
df_val  <- df_coded[-idx_test, , drop = FALSE]  


# Process each article using make_query
queries <- make_query(
  text = df_test$text,
  prompt = "Categories: climate change, other", # 
  template = "{prefix}{text}\n{prompt}",
  system = "You are a strict text classifier. Your task is to determine if the given text is about climate change. 
Respond with exactly one of the following two labels (case-sensitive): 
- climate change
- Other

Do not add any explanation or extra text.",
  
  prefix = "Text to classify: "
  
)

# Process and annotate the movie reviews
df_test$annotation <- query(queries, screen = FALSE, output = "text")

# Recode the output to calculate metrics

df_test <- df_test %>%
  mutate(
    annotation_rec = recode(as.character(annotation),
                        "climate change" = 1L,
                        "Other" = 0L,
                        .default = NA_integer_)
  )

# Calculate recall precision and F1 for the positve class (1)

# 1) Keep only rows with non-missing labels
df_eval <- df_test[!is.na(df_test$climate_change_human) & !is.na(df_test$annotation_rec), , drop = FALSE]

# 2) Coerce to integers (if factors/characters)
gold <- as.integer(df_eval$climate_change_human)
pred <- as.integer(df_eval$annotation_rec)

# 3) Compute confusion matrix terms for positive class = 1
TP <- sum(pred == 1 & gold == 1)
FP <- sum(pred == 1 & gold == 0)
FN <- sum(pred == 0 & gold == 1)
TN <- sum(pred == 0 & gold == 0)  # optional

# 4) Metrics (guard against division by zero)
precision_pos <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
recall_pos    <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
f1_pos        <- if (is.finite(precision_pos) && is.finite(recall_pos) &&
                     (precision_pos + recall_pos) > 0) {
  2 * precision_pos * recall_pos / (precision_pos + recall_pos)
} else NA_real_

list(
  TP = TP, FP = FP, FN = FN, TN = TN,
  precision_pos = precision_pos,
  recall_pos = recall_pos,
  f1_pos = f1_pos
)

## Next:
# Consider if you want to improve the prompt, switch the model, etc.
# If yes: Prepare the new set up here and repeat the classification and evaluation until satisfied
# if satisfied already, employ the set-up also to df_val 




