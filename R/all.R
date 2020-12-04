library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
#library(crfsuite)
library(dendextend)
library(tibble)
library(textdata)
library(lattice)
library(tidyverse)
library(textmineR)
library(plotrix)

# Clean with tm
tm_clean <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "sephora", "ulta", "company"))
  return(corpus)
}

# Define bigram tokenizer
tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

setwd("E:/Sentiment_analysis_Assignment")
sephora<- readr::read_csv(file = 'consumeraffairplustrustpilot_sephora.csv')
ulta<- readr::read_csv(file = 'consumeraffairplustrustpilot_ulta.csv')

str(sephora)
str(ulta)

# create a data frame of just the positive revies
# ulta is longer by one so here I simply add a null to sephora reviews to get same length
all <- data.frame(
  sephora = c(as.character(sephora, 'NULL')),
  ulta = as.character(ulta),
  stringsAsFactors = F
)

# Create a corpus
all_corp <- VCorpus(VectorSource(all))
all_corp <- tm_clean(all_corp)

# Create a all_corp tdm 
all_tdm <- TermDocumentMatrix(all_corp)

# Create all_m
all_m <- as.matrix(all_tdm)

# Make commonalitiy cloud
commonality.cloud(all_m, 
                  colors = "steelblue1",
                  max.words = 100)

# Name the columns of all_tdm
colnames(all_tdm) <- c("Sephora", "Ulta")

# Create all_m_compare
all_m_compare <- as.matrix(all_tdm)

# Create comparison cloud
comparison.cloud(all_m_compare,
                 colors = c("red", "blue"),
                 max.words = 100)

# Identify terms shared by both documents
common_words <- subset(
  all_m,
  all_m[, 1] > 0 & all_m[, 2] > 0
)

head(common_words)

# calc common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))

# The plotrix package has been loaded
# Make pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 90,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Sephora",
                            "Words",
                            "Ulta"))

 #######
all_corpus <- VCorpus(VectorSource(sephora_ulta[,1:2]))
str(all_corpus)

# Clean corpus
all_corpus <- tm_clean(all_corpus)

# Create a term document matrix for all_corpus
all_tdm <- TermDocumentMatrix(all_corpus)

# Name the columns of all_tdm
colnames(all_tdm) <- c("Sephora", "Ulta")

# Create all_m
all_m <- as.matrix(all_tdm)
head(all_m)
comparison.cloud(all_m,
                 colors = c("#F44336", "#2196f3"),
                 max.words = 100)
