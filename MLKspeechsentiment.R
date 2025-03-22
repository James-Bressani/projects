# Load libraries
library(XML)
library(tm)
library(ggplot2)

# Load sentiment word lists
positive <- scan("data/positive-words.txt", what = "character", comment.char = ";")
negative <- scan("data/negative-words.txt", what = "character", comment.char = ";")

# Read and parse MLK's speech
mlk_url <- "http://www.analytictech.com/mb021/mlk.htm"
doc.html <- htmlTreeParse(mlk_url, useInternal = TRUE)
doc.text <- unlist(xpathApply(doc.html, "//p", xmlValue))
doc.text <- gsub('\\n|\\r', ' ', doc.text)

# Clean and preprocess text
mlk_corpus <- Corpus(VectorSource(doc.text))
mlk_corpus <- tm_map(mlk_corpus, content_transformer(tolower))
mlk_corpus <- tm_map(mlk_corpus, removePunctuation)
mlk_corpus <- tm_map(mlk_corpus, removeNumbers)
mlk_corpus <- tm_map(mlk_corpus, removeWords, stopwords("en"))
mlk_corpus <- tm_map(mlk_corpus, stripWhitespace)

# Create Term-Document Matrix
tdm <- TermDocumentMatrix(mlk_corpus)
wordCounts <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)

# Compute positive & negative sentiment ratios
positiveRatio <- sum(wordCounts[names(wordCounts) %in% positive]) / sum(wordCounts)
negativeRatio <- sum(wordCounts[names(wordCounts) %in% negative]) / sum(wordCounts)

# Split text into quartiles
wordsCorpus <- unlist(strsplit(unlist(mlk_corpus), " "))
wordsCorpus <- wordsCorpus[wordsCorpus != ""]
cutpoint <- round(length(wordsCorpus) / 4)

# Function to calculate sentiment ratios
calculate_sentiment <- function(words) {
  tdm <- TermDocumentMatrix(Corpus(VectorSource(words)))
  wordCounts <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  totalWords <- sum(wordCounts)
  positiveRatio <- sum(wordCounts[names(wordCounts) %in% positive]) / totalWords
  negativeRatio <- sum(wordCounts[names(wordCounts) %in% negative]) / totalWords
  return(c(positiveRatio, negativeRatio))
}

# Compute sentiment for each quartile
sentiments <- sapply(1:4, function(i) {
  start <- (i - 1) * cutpoint + 1
  end <- ifelse(i == 4, length(wordsCorpus), i * cutpoint)
  calculate_sentiment(wordsCorpus[start:end])
})

# Prepare data for visualization
quartiles <- c("Q1", "Q2", "Q3", "Q4")
positiveDF <- data.frame(Quartile = quartiles, Ratio = sentiments[1, ])
negativeDF <- data.frame(Quartile = quartiles, Ratio = sentiments[2, ])

# Plot sentiment ratios
ggplot(positiveDF, aes(x = Quartile, y = Ratio)) +
  geom_bar(stat = "identity") +
  ggtitle("Positive Sentiment Ratio")

ggplot(negativeDF, aes(x = Quartile, y = Ratio)) +
  geom_bar(stat = "identity") +
  ggtitle("Negative Sentiment Ratio")
