library(tidyverse)
library(rtweet)
library(dotenv)


auth <- create_token(
  app = 'toxicity_detection',
  consumer_key = Sys.getenv('api_key'),
  consumer_secret = Sys.getenv('api_secret_key'),
  access_token = Sys.getenv('access_token'),
  access_secret = Sys.getenv('access_secret')
)

# Pseudorandom sample of tweets that contain this token
# This function includes RTs
covid_19_tweets <- search_tweets('coronavirus', n=4000, retryonratelimit = TRUE)


potus_tweets <- get_timeline('POTUS', n=50000, retryonratelimit=TRUE, verbose=TRUE)

# Plot results over time
biden_tweet_freqs <- ts_plot(potus, "weeks", tz='US/Eastern') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from @POTUS",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


tidy_potus <- potus_tweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

# Remove stopwords and count most common
data("stop_words")

top_words<-
  tidy_potus %>%
  anti_join(stop_words) %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  count(word) %>%
  arrange(desc(n))

top_words %>%
  slice(1:20) %>%
  #Reorder words by decreasing frequency
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x =
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title =
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Biden Tweets")+
  guides(fill=FALSE)



# Count # positive/negative tokens for each tweet
biden_tweet_sentiment <- tidy_potus %>%
  left_join(get_sentiments("bing")) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neither', sentiment),
         date = as.Date(created_at, format="%Y-%m-%d %x")) %>%
  count(date, sentiment) %>%
  filter(sentiment != 'neither')

biden_sentiment_plot <- ggplot(biden_tweet_sentiment, aes(x=date, y=n, color=sentiment))+
  # geom_line(color="red", size=.5)+
  geom_smooth() +
  theme_minimal()+
  ylab("# Negative and Positive Words")+
  xlab("")+
  ggtitle("Sentiment Trends in Tweets from @POTUS account")

ggsave('images/biden-tweet-freqs.png', biden_tweet_freqs)
ggsave('images/sentiment-analysis-trend.png', biden_sentiment_plot)
