install.packages(c("dplyr", "ggplot2", "wordcloud", "tm", "syuzhet")) 
library(dplyr) 
library(ggplot2)
library(wordcloud) 
library(tm)
library(syuzhet)

file_path <- "C:/Users/lenovo/Downloads/prodigy/task4/twitter_training.csv"
df <- read.csv(file_path, sep = ",", stringsAsFactors = FALSE)
colnames(df) <- c("ID", "city", "state", "comment") 
df$sentiment_score <- get_sentiment(df$state, method = "afinn")

df$sentiment_class <- ifelse(df$sentiment_score > 0, "positive", ifelse(df$sentiment_score < 0, "negative", "neutral"))

ggplot(df, aes(x = sentiment_class)) + geom_bar(fill = c("green", "red", "blue")) + labs(title = "Sentiment Distribution", x = "Sentiment", y = "Frequency")





positive_comments <- df %>% filter(sentiment_class == "positive") %>% select(comment)


corpus <- Corpus(VectorSource(positive_comments$comment))


corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%  # Convert to lowercase
  tm_map(removePunctuation) %>%              # Remove punctuation
  tm_map(removeNumbers) %>%                  # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # Remove stopwords
  tm_map(stripWhitespace)                    # Remove extra whitespace


wordcloud(corpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))




# Group by city and sentiment class
city_sentiment <- df %>%
  group_by(city, sentiment_class) %>%
  summarise(count = n()) %>%
  ungroup()

# Summarize the top 10 cities by total sentiment count
top_cities <- city_sentiment %>%
  group_by(city) %>%
  summarise(total_count = sum(count)) %>%
  top_n(10, total_count)

# Filter the sentiment data for the top 10 cities
top_city_sentiment <- city_sentiment %>%
  filter(city %in% top_cities$city)

# Plot the top 10 cities by sentiment
ggplot(top_city_sentiment, aes(x = city, y = count, fill = sentiment_class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Cities by Sentiment", x = "City", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_manual(values = c("green", "red", "blue", "orange"))
