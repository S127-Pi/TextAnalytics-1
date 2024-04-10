setwd("C:/Users/thoma/OneDrive/Documenten/ESE/Text Analytics/Assignment 1")
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(syuzhet)
library(SnowballC)
library(sentimentr)
library(qdap)
library(caret)


# Loading in the complete reviews
complete_reviews <- read.csv("complete_reviews.csv")


### Perform a sentiment analysis on the reviews

# Get the sentiment of each review using the polarity function
complete_reviews$sentiment <- sentiment_by(sentimentr::get_sentences(complete_reviews[,"Review"]))$ave_sentiment

# Plot the results, showing the distribution of the sentiment scores for positive and negative reviews
ggplot(complete_reviews, aes(x = sentiment, fill = factor(isPositive, labels = c("Negative", "Positive")))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Rating")

# Additionally: see how accurately it predicts positive and negative reviews (if sentiment is positive, review is positive, and vice versa)
confusionMatrix(factor(complete_reviews$isPositive), 
                factor(complete_reviews$sentiment >= 0))


### Make adjustments to the settings 

# Get the sentiment, with a amplifier weight and adversative weight set to zero
complete_reviews$sentiment_changes <- sentiment_by(sentimentr::get_sentences(complete_reviews[,"Review"]), 
                                                   amplifier.weight = 0, 
                                                   adversative.weight = 0,
                                                   )$ave_sentiment

# Plot the sentiment for positive and negative reviews with the adjusted sentiment
ggplot(complete_reviews, aes(x = sentiment_changes, fill = factor(isPositive, labels = c("Negative","Positive")))) +
  geom_density(alpha = 0.5) +
  labs(x = "sentiment", fill = "Rating")

# Again, additionally: see the accuracy of the sentiment analysis
confusionMatrix(factor(complete_reviews$isPositive), 
                factor(complete_reviews$sentiment_changes >= 0))


# Average sentiment no changes polarity
median(complete_reviews$sentiment)
median(complete_reviews$sentiment[complete_reviews$isPositive == TRUE])
median(complete_reviews$sentiment[complete_reviews$isPositive == FALSE])

# Percentage of people that have a positive sentiment, but a negative review
sum(complete_reviews$sentiment[complete_reviews$isPositive == FALSE] > 0)/sum(complete_reviews$isPositive == FALSE)

# Average sentiment, amplifier to 0
median(complete_reviews$sentiment_changes)
median(complete_reviews$sentiment_changes[complete_reviews$isPositive == TRUE])
median(complete_reviews$sentiment_changes[complete_reviews$isPositive == FALSE])



### Extra

# Get the sentiment, with a amplifier weight to zero, and adversative weight to -1
complete_reviews$sentiment_changes2 <- sentiment_by(sentimentr::get_sentences(complete_reviews[,"Review"]), 
                                                    amplifier.weight = 1, 
                                                    adversative.weight = -1,
)$ave_sentiment

# Plot the sentiment for positive and negative reviews with the adjusted sentiment
ggplot(complete_reviews, aes(x = sentiment_changes2, fill = isPositive)) +
  geom_density(alpha = 0.5)

# Average sentiment, amplifier to 0, adversative to -1
median(complete_reviews$sentiment_changes2)
median(complete_reviews$sentiment_changes2[complete_reviews$isPositive == TRUE])
median(complete_reviews$sentiment_changes2[complete_reviews$isPositive == FALSE])












