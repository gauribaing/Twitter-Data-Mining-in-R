install.packages("twitteR")
library(twitteR)
library(rtweet)


consumer_key <- "*****"
consumer_secret <- "*****"
access_token <- "*****"
access_secret <- "*****"

setup_twitter_oauth(consumer_key, consumer_secret, 
                    access_token, access_secret)

#Search Tweets
climate <- search_tweets("climate", n=1000, include_rts=FALSE, lang="en")
climate

Gates <- get_timeline("BillGates", n= 3200)
Gates

# Remove retweets
Gates_tweets_organic <- Gates[Gates$is_retweet==FALSE, ] 
# Remove replies
Gates_tweets_organic <- subset(Gates_tweets_organic, is.na(Gates_tweets_organic$reply_to_status_id))

Gates_tweets_organic <- Gates_tweets_organic %>% arrange(-favorite_count)
Gates_tweets_organic[1,5]

Gates_tweets_organic <- Gates_tweets_organic %>% arrange(-retweet_count)
Gates_tweets_organic[1,5]

# Keeping only the retweets
Gates_retweets <- Gates[Gates$is_retweet==TRUE,]
# Keeping only the replies
Gates_replies <- subset(Gates, !is.na(Gates$reply_to_status_id))

# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856, 192, 120)
)

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))# Rounding the data to two decimal points
data <- round_df(data, digit=2)# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

colnames(Gates)[colnames(Gates)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(Gates, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Bill Gates",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

Gates_app <- Gates %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
Gates_app <- subset(Gates_app, count > 11)

data <- data.frame(
  category=Gates_app$source,
  count=Gates_app$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

Gates_tweets_organic$text <-  gsub("https\\S*", "", Gates_tweets_organic$text)
Gates_tweets_organic$text <-  gsub("@\\S*", "", Gates_tweets_organic$text) 
Gates_tweets_organic$text  <-  gsub("amp", "", Gates_tweets_organic$text) 
Gates_tweets_organic$text  <-  gsub("[\r\n]", "", Gates_tweets_organic$text)
Gates_tweets_organic$text  <-  gsub("[[:punct:]]", "", Gates_tweets_organic$text)

tweets <- Gates_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Bill Gates",
       subtitle = "Stop words removed from the list")

Gates_tweets_organic$hashtags <- as.character(Gates_tweets_organic$hashtags)
Gates_tweets_organic$hashtags <- gsub("c\\(", "", Gates_tweets_organic$hashtags)
set.seed(1234)
wordcloud(Gates_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(1234)
wordcloud(Gates_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

install.packages("syuzhet")
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(Gates, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",Gates)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",Gates)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

