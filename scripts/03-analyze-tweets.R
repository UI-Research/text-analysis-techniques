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
hotd_tweets <- search_tweets('#HOTD', n=50000, include_rts=FALSE, retryonratelimit = TRUE)



# Plot results over time
ts_plot(hotd_tweets, "hours", tz='US/Eastern') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::geom_vline(xintercept=as.POSIXct('2022-10-23 21:00:00', tz='US/Eastern'),
                      linetype='dashed', color='red') +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about House of the Dragon",
    subtitle = "Tweet counts aggregated by hour",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
