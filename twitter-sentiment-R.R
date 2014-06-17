# Sentiment analysis based on the following two papers:
# Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
#        Proceedings of the ACM SIGKDD International Conference on Knowledge 
#        Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
#        Washington, USA, 
#    Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing 
#        and Comparing Opinions on the Web." Proceedings of the 14th 
#        International World Wide Web conference (WWW-2005), May 10-14, 
#        2005, Chiba, Japan.
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(wordcloud)

query <- 'e3' # The word we want to analyze. Change this
maxTweets <- 10000 # The maximum number of tweets to search
startDate <- '2014-01-01'

consumer.key <- 'v5b3mm9hRjZ7kL7gcOUvxwr4m'
consumer.secret <- 't2NZr2T18jOEFcX8h2B1jS8k8JH5jwD2eVUZBFyGAdEVI22GT6'
access.token <- '1149944478-vXoBfwZ718WW12m6xFjoGHEvjQk73H1qP1g2q0Z'
access.token.secret <- 'muJVicR46pW7dlMqxDkyAakJMDtijFyPeMCzcETU5ohf0'
setup_twitter_oauth(consumer_key=consumer.key, consumer_secret=consumer.secret, access_token=access.token, access_secret=access.token.secret)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none') {
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        sentence <- gsub('[[:punct:]]', '', sentence)
        sentence <- gsub('[[:cntrl:]]', '', sentence)
        sentence <- gsub('\\d+', '', sentence)
        sentence <- tolower(sentence)
        word.list <- str_split(sentence, '\\s+')
        words <- unlist(word.list)
        pos.matches <- match(words, pos.words)
        neg.matches <- match(words, neg.words)
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)
        score <- sum(pos.matches) - sum(neg.matches)
        return(score)
    }, pos.words, neg.words, .progress=.progress)
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
}

# Retrieve tweets based on query
tweets <- searchTwitter(query, n=maxTweets, since=startDate)
tweets.df <- twListToDF(tweets)
write.csv(tweets.df, file='data.csv', row.names=FALSE)

pos.words <- scan('positive-words.txt', what='character', comment.char=';')
neg.words <- scan('negative-words.txt', what='character', comment.char=';')
pos.words <- c(pos.words, 'upgrade')
neg.words <- c(neg.words, 'wtf', 'fail', 'epicfail')

tweets.df <- read.csv('data.csv')
tweets.df$text <- as.factor(tweets.df$text)
sentiment.scores <- score.sentiment(tweets.df$text, pos.words, neg.words, .progress='text')
write.csv(sentiment.scores, file='scores.csv', row.names=TRUE)

# Uncomment these for a histogram of general sentiment
# hist.plot <- ggplot() + geom_histogram(data=sentiment.scores, aes(x=score))
# hist.plot + theme_economist() + scale_colour_economist()

# tweet.corpus <- Corpus(DataframeSource(data.frame(sentiment.scores[ , 2])))
# tweet.corpus <- tm_map(tweet.corpus, removePunctuation)
# tweet.corpus <- tm_map(tweet.corpus, tolower)
# tweet.corpus <- tm_
# 
# require(XML)
# require(tm)
# require(wordcloud)
# require(RColorBrewer)
# u = "http://cran.r-project.org/web/packages/available_packages_by_date.html"
# t = readHTMLTable(u)[[1]]
# ap.corpus <- Corpus(DataframeSource(data.frame(as.character(t[,3]))))
# ap.corpus <- tm_map(ap.corpus, removePunctuation)
# ap.corpus <- tm_map(ap.corpus, tolower)
# ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
# ap.tdm <- TermDocumentMatrix(ap.corpus)
# ap.m <- as.matrix(ap.tdm)
# ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
# ap.d <- data.frame(word = names(ap.v),freq=ap.v)
# table(ap.d$freq)
# pal2 <- brewer.pal(8,"Dark2")
# png("wordcloud_packages.png", width=1280,height=800)
# wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
#           max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
# dev.off()
