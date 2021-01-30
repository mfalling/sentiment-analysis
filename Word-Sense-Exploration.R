# Library -----------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(syuzhet)

# Load Data ---------------------------------------------------------------

# Load all csv files from the target folder into the same dataframe.
# Dataset is related to mask wearing during the COVID-19 pandemic.
df <- list.files(path = "data/100k_RAND_FILES",
                 pattern = "*.csv",
                 full.names = TRUE) %>%
  map_df(~ read_csv(.))

# The wordsense.csv file contains the synonyms/sense of the words.
# This file is from the NRC EmoLex researchers.
wordsense <- read.csv("data/wordsense.csv", fileEncoding = "UTF-8-BOM")

# Capture NRC dictionary terms.
nrcdict <- get_sentiment_dictionary("nrc") %>%
  select(word, sentiment)

# Word Frequencies --------------------------------------------------------

# Filter the word-frequency pairings to words that exist in the NRC EmoLex.
wordFreq <- df %>%
  pull(complete_texts) %>%
  as_tibble() %>%
  unnest_tokens(word, value) %>%
  group_by(word) %>%
  tally(name = "freq") %>%
  filter(word %in% nrcdict$word)

# Compare Words with Sense ------------------------------------------------

# Values with "0" indicate that there is no sentiment attached.
x <- wordsense %>%
  select(word, sense, score) %>%
  distinct() %>%
  group_by(word, sense) %>%
  tally(score) %>%
  filter(word %in% wordFreq$word) %>%
  inner_join(wordFreq) %>%
  arrange(desc(freq))

# The following word-sense pairings in the NRC EmoLex do not match
# the word-sense pairings seen in our dataset (as determined through
# manual investigation): 
#       Trump
#       Wear
#       Nose
#       Cover
