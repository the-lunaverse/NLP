### DS 316 Final Project ###

##Upload Libraries
library(tidyverse)
library(RColorBrewer)
library(tm)
library(NLP)
library(twitteR)
library(stringi)
library(syuzhet)
library(topicmodels)
library(tsne)
library(Rtsne)
library(SentimentAnalysis)
library(cluster)
library(fviz_nbclust)


##### Twitter API Scrape ###### 

api_key <- "YOUR OWN KEY "
api_secret <- "YOUR OWN KEY "
access_token <- "YOUR OWN KEY "
access_token_secret <-  "YOUR OWN KEY "

twitter_token <- create_token(
  app = "YOUR APP",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret
)

get_token()

first_query <- search_tweets("Covid-19",
                             n=1500, include_rts = FALSE, lang="en")
head(first_query)




##Import dataset 
twitter_hi_cov <- read_csv("Your path to your scraped data!")
head(twitter_hi_cov)
view(twitter_hi_cov)

##Subset dataset for the months June, July, & August 2021
summer_2021 <- twitter_hi_cov[twitter_hi_cov$data.created_at >= "2021-05-01" & twitter_hi_cov$data.created_at <= "2021-07-31", ]
view(summer_2021)

# Dropping rows that have NA by indices (the first three rows have NA values for all columns)
summer_21 <- summer_2021[-c(1:26, 1587:1590, 2579:2582, 2822:3052), ]
summer_21


# Subset and remove NA values of each month 
may_2021 <- summer_21[summer_21$data.created_at >= "2021-05-01" & summer_21$data.created_at <= "2021-05-31", ]
may_2021_clean <- may_2021[-c(1:4, 621:624, 838:863), ]
june_2021 <- summer_21[summer_21$data.created_at >= "2021-06-01" & summer_21$data.created_at <= "2021-06-30", ]
june_2021_clean <- june_2021[-c(746:749, 1118:1147), ]
july_2021 <- summer_21[summer_21$data.created_at >= "2021-07-01" & summer_21$data.created_at <= "2021-07-31", ]
july_2021_clean <- july_2021[-c(838:871), ]

view(may_2021_clean)
view(june_2021_clean)
view(july_2021_clean)

#Pallete Function - Color WordCloud
pal=brewer.pal(8, "Dark2")


#### WORD CLOUD MAY #####
may_wordcloud1 <- Corpus(VectorSource(may_2021_clean$data.text))
may_wordcloud1

wordcloud(may_wordcloud1, max.words = 500, random.order = FALSE, colors=pal)

## Adding conditions to wordcloud
may_wordcloud1 <- tm_map(may_wordcloud1, removePunctuation)
may_wordcloud2 <- tm_map(may_wordcloud1, tolower)  
may_wordcloud3 <- tm_map(may_wordcloud2, stripWhitespace)  
may_wordcloud4 <- tm_map(may_wordcloud3, removeWords, c(stopwords('english'),"may","amp", "covid19", "covid-19", "pandemic", "coronavirus", "hawaii","cases","covid","khonnews"))

wordcloud(may_wordcloud4, max.words =500, random.order = FALSE, colors=pal)


##### JUNE Word Cloud #######
june_wordcloud1 <- Corpus(VectorSource(june_2021_clean$data.text))
june_wordcloud1

wordcloud(june_wordcloud1, max.words = 500, random.order = FALSE, colors=pal)

## Adding conditions to wordcloud
june_wordcloud1 <- tm_map(june_wordcloud1, removePunctuation)
june_wordcloud2 <- tm_map(june_wordcloud1, tolower)  
june_wordcloud3 <- tm_map(june_wordcloud2, stripWhitespace)  
june_wordcloud4 <- tm_map(june_wordcloud3, removeWords, c(stopwords('english'), "covid19", "covid-19", "pandemic", "coronavirus", "hawaii","cases","covid","khonnews"))

wordcloud(june_wordcloud4, max.words =500, random.order = FALSE, colors=pal)


##### JULY Word Cloud #######
july_wordcloud1 <- Corpus(VectorSource(july_2021_clean$data.text))
july_wordcloud1

wordcloud(july_wordcloud1, max.words = 500, random.order = FALSE, colors=pal)

## Adding conditions to wordcloud
july_wordcloud1 <- tm_map(july_wordcloud1, removePunctuation)
july_wordcloud2 <- tm_map(july_wordcloud1, tolower)  
july_wordcloud3 <- tm_map(july_wordcloud2, stripWhitespace)  
july_wordcloud4 <- tm_map(july_wordcloud3, removeWords, c(stopwords('english'), "covid19", "covid-19", "pandemic", "coronavirus", "hawaii","cases","covid","khonnews"))

wordcloud(july_wordcloud4, max.words =500, random.order = FALSE, colors=pal)


#### Tidytext - Clean data ####
data_df <- data.frame(text=april$text)
data_df$text <- as.character(data_df$text)

Encoding(data_df$text) <- "latin1" 


data_df$text <- gsub('http\\S+\\s*', '', data_df$text)
data_df$text <- gsub('\\b+RT', '', data_df$text)
data_df$text <- gsub('#\\S+', '', data_df$text)
data_df$text<- gsub('@\\S+', '', data_df$text)
data_df$text <- gsub('[[:cntrl:]]', '', data_df$text)
data_df$text <- gsub('\\d', '', data_df$text)

tryToLower <- function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e)e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

custom_stopwords <- c(stopwords('english'), 'amp', 'covid', 'covid19', 'covid-19',
                      'coronavirus', 'pandemic','hawaii',"cases")

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryToLower))
  corpus <- tm_map(corpus, removeWords,custom_stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

corpus <-Corpus(VectorSource(data_df$text))
corpus <- clean_corpus(corpus) 

tdm <- TermDocumentMatrix(corpus) 
tdm

m <- as.matrix(tdm)
m

###############################
# Tidytext - Create data set tweets_td
###############################

dtm <- DocumentTermMatrix(corpus)
str(dtm)

tweets_td <- tidy(dtm)
tweets_td

###############################
# Tidytext - Word frequency
###############################
tweets_td %>% 
  count(term, sort=T)%>%
  filter(n>=20)%>%
  mutate(word=reorder(term,n))%>%
  ggplot(aes(word, n))+
  geom_col(fill="yellow", col="black")+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Word frequencies >=20 in tweets")+
  theme(plot.title=element_text(size=10, face="bold"))+
  theme_bw()



##### Sentiment Analysis MAY, JUNE, JULY  ######

### Sentiment Analysis text for May ###
may_hicovid19_text <-  as.character(may_2021_clean$data.text)
may_hicovid19_text

### Sentiment Analysis text for june ###
june_hicovid19_text <-  as.character(june_2021_clean$data.text)
june_hicovid19_text

### Sentiment Analysis text for july ###
july_hicovid19_text <-  as.character(july_2021_clean$data.text)
july_hicovid19_text

nrc_lexicon <-  get_sentiments("nrc")
nrc_lexicon

## May sentiment 
may_hicovid19_sentiment <-  get_nrc_sentiment(may_hicovid19_text)
may_hicovid19_sentiment
## June sentiment
june_hicovid19_sentiment <-  get_nrc_sentiment(june_hicovid19_text)
june_hicovid19_sentiment
## July sentiment
july_hicovid19_sentiment <-  get_nrc_sentiment(july_hicovid19_text)
july_hicovid19_sentiment

## May sentiment score
may_hicovid19_sentiment_score <- data.frame(colSums(may_hicovid19_sentiment[,]))
may_hicovid19_sentiment_score
names(may_hicovid19_sentiment_score) <- "Score"
names(may_hicovid19_sentiment_score)

may_hicovid19_sentiment_score <- cbind("sentiment"=rownames(may_hicovid19_sentiment_score),
                                       may_hicovid19_sentiment_score)

rownames(may_hicovid19_sentiment_score) <-  NULL

## June sentiment score
june_hicovid19_sentiment_score <- data.frame(colSums(june_hicovid19_sentiment[,]))
june_hicovid19_sentiment_score
names(june_hicovid19_sentiment_score) <- "Score"
names(june_hicovid19_sentiment_score)

june_hicovid19_sentiment_score <- cbind("sentiment"=rownames(june_hicovid19_sentiment_score),
                                       june_hicovid19_sentiment_score)

rownames(june_hicovid19_sentiment_score) <-  NULL

## July Sentiment score
july_hicovid19_sentiment_score <- data.frame(colSums(july_hicovid19_sentiment[,]))
july_hicovid19_sentiment_score
names(july_hicovid19_sentiment_score) <- "Score"
names(july_hicovid19_sentiment_score)

july_hicovid19_sentiment_score <- cbind("sentiment"=rownames(july_hicovid19_sentiment_score),
                                        july_hicovid19_sentiment_score)

rownames(july_hicovid19_sentiment_score) <-  NULL

##Remove Positive and Negative Sentiment Columns
# Remove May columns
may_hicovid19_sentiment_score[,]
may_hicovid19_sentiment_score2 <-  may_hicovid19_sentiment_score[1:8,]
## Remove June columns
june_hicovid19_sentiment_score[,]
june_hicovid19_sentiment_score2 <-  june_hicovid19_sentiment_score[1:8,]
## Remove July columns
july_hicovid19_sentiment_score[,]
july_hicovid19_sentiment_score2 <-  july_hicovid19_sentiment_score[1:8,]

#### Plot Sentiment Analysis ####

## Plot May sentiment Analysis
ggplot(data=may_hicovid19_sentiment_score2, aes(x=sentiment, y=Score))+
  geom_bar(aes(fill=sentiment), stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+
  ylab("Scores")+
  ggtitle("Sentiments of People Behind the Tweets in Hawaii During May 2021")

## Plot June sentiment Analysis
ggplot(data=june_hicovid19_sentiment_score2, aes(x=sentiment, y=Score))+
  geom_bar(aes(fill=sentiment), stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+
  ylab("Scores")+
  ggtitle("Sentiments of People Behind the Tweets in Hawaii During June 2021")

## Plot July sentiment Analysis
ggplot(data=july_hicovid19_sentiment_score2, aes(x=sentiment, y=Score))+
  geom_bar(aes(fill=sentiment), stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+
  ylab("Scores")+
  ggtitle("Sentiments of People Behind the Tweets in Hawaii During July 2021")



##### Clara Cluster Analysis ####


#### Tidytext - Clean data ####
data_df <- data.frame(text=may_2021_clean$data.text)
data_df$text <- as.character(data_df$text)

data_df$text <- gsub('http\\S+\\s*', '', data_df$text)
data_df$text <- gsub('\\b+RT', '', data_df$text)
data_df$text <- gsub('#\\S+', '', data_df$text)
data_df$text<- gsub('@\\S+', '', data_df$text)
data_df$text <- gsub('[[:cntrl:]]', '', data_df$text)
data_df$text <- gsub('\\d', '', data_df$text)

## Converting dataframe into corpus 
corpus <- Corpus(VectorSource(data_df$text))
corpus <- clean_corpus(corpus)

tdm <- TermDocumentMatrix(corpus)
tdm

m <-  as.matrix(tdm)
m

########## Unsupervised Machine Learning #############3
#Grouping - qords with t-sne algorithm
# t-distributed stochastic neighbor embedding (t-sne)

tsne_out <- Rtsne(m, dims=2, check_duplicates = F)

tsne_out


##See tsne visualization plotting. It is not necessary, but prvides a visual

str(tsne_out)
plot(tsne_out$Y, t="n", main="t-sne visulation of words in document" ,
     cex.main=1)
text(tsne_out$Y, labels = rownames(m), cex=0.35)


##Create Frequency Data Frame 
term_freq <- rowSums(m)
term_freq

freq_df <-  data.frame(word=names(term_freq),frequency=term_freq)
freq_df <- freq_df[order(freq_df[,2], decreasing = T),]
freq_df 

##Max Clusters
x1 <- tsne_out$Y[,1]
x2 <- tsne_out$Y[,2]
df <- data.frame(x1,x2)
df

##Machine informs us number of optimal clusters we should use from the themes of the text data (dataset)
fviz_nbclust(df, clara, method="silhouette")+ theme_classic()

## Cluster Analysis Version 1 - no lables or names
clara_res <-  clara(df, 4, samples = 75, pamLike = T)

p <- fviz_cluster(clara_res,
                  palette=c("red","green","blue","grey","yellow"),
                  eclispe.type ="t",
                  geom = "point",
                  pointsize = 0.5,
                  ggtheme = theme_classic())
p

clara_res$data

## Cluster Analysis Version 2- with labels and using highest frequency of words
clara_res2 <- cbind(clara_res$data, clara_res$clustering)
clara_res2

label <- rownames(m)
D <- cbind(clara_res2, label, freq_df$frequency)
D 
DX2 <- as.data.frame(D)
DX2$x1 <- as.numeric(as.character(DX2$x1))
DX2$x2 <- as.numeric(as.character(DX2$x2))
DX2$V5 <- as.numeric(as.character(DX2$V5))
DX2

names(DX2) <- c("x1","x2","cluster","label","frequency")
DX3 <- subset(DX2, DX2$frequency >= 10)
DX3

names(DX3) <- c("x1","x2","cluster","label","frequency")

p <- ggplot(DX2, aes(x=x1, y=x2, color=cluster), ggrepel.max.overlaps=Inf)+
  geom_point(size=0.8)+
  geom_encircle()
p

q <- p+geom_text_repel(data=DX3, aes(x=x1, y=x2, label=label, color=cluster),
                       size=4, max.overlaps=Inf)+
  ggtitle("Cluster Analysis of T-sne & Labeling of High Frequency Terms in Tweets During May 2021")
q














