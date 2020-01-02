install.packages('tm')
install.packages("wordcloud") 
library(wordcloud) 
library(tm)
library(cluster)    
library(factoextra) 
setwd('C:/Users/carlo/Desktop/Data science/ASDM/textmining')
bbchealth <-read.csv("bbchealth.csv", header= TRUE)
cnnhealth <-read.csv("cnnhealth.csv", header= TRUE)
foxhealth <-read.csv("foxnewshealth.csv", header= TRUE)
#Inspect the dataset 
head(bbchealth)
head(cnnhealth)
head(foxhealth)
#Inspect the tweet column in the datasets
head(bbchealth$tweet)
head(cnnhealth$tweet)
head(foxhealth$tweet)

#create text vectors
bbchealth_tweet<-bbchealth$tweet
cnnhealth_tweet<-cnnhealth$tweet
foxhealth_tweet<-foxhealth$tweet

#convert all text to lower case
bbchealth_tweet<-tolower(bbchealth_tweet)
cnnhealth_tweet<-tolower(cnnhealth_tweet)
foxhealth_tweet<-tolower(foxhealth_tweet)

#Replace blank space ("rt")
bbchealth_tweet <-gsub("rt", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("rt", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("rt", "", foxhealth_tweet)

#Replace tweeter @UserName
bbchealth_tweet <-gsub("@\\w+", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("@\\w+", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("@\\w+", "", foxhealth_tweet)

#Replace links in the tweets
bbchealth_tweet <-gsub("http\\S+\\s*", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("http\\S+\\s*", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("http\\S+\\s*", "", foxhealth_tweet)

#Remove punctuation
bbchealth_tweet <-gsub("[[:punct:]]", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("[[:punct:]]", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("[[:punct:]]", "", foxhealth_tweet)

#Remove tabs
bbchealth_tweet <-gsub("[ |\t]{2,}", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("[ |\t]{2,}", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("[ |\t]{2,}", "", foxhealth_tweet)

#Remove "video" word in the tweets
bbchealth_tweet <-gsub("video", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("video", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("video", "", foxhealth_tweet)

#Remove blank spaces at the beginning

bbchealth_tweet <-gsub("^ ", "", bbchealth_tweet)
cnnhealth_tweet <-gsub("^ ", "", cnnhealth_tweet)
foxhealth_tweet <-gsub("^ ", "", foxhealth_tweet)

#Remove blank spaces at the end

bbchealth_tweet <-gsub(" $", "", bbchealth_tweet)
cnnhealth_tweet <-gsub(" $", "", cnnhealth_tweet)
foxhealth_tweet <-gsub(" $", "", foxhealth_tweet)

#Inspect the vectors after cleaning
head(bbchealth_tweet)
head(cnnhealth_tweet)
head(foxhealth_tweet)

#converting the text vectors to corpus 
bbchealth_corpus <-Corpus(VectorSource(bbchealth_tweet))
bbchealth_corpus
cnnhealth_corpus <-Corpus(VectorSource(cnnhealth_tweet))
cnnhealth_corpus
foxhealth_corpus <-Corpus(VectorSource(foxhealth_tweet))
foxhealth_corpus

# clean up corpus by removing stop words, number and Whitespace
bbchealth_corpus  <-tm_map(bbchealth_corpus,removeWords,stopwords("english"))
bbchealth_corpus  <-tm_map(bbchealth_corpus,removeNumbers)
bbchealth_corpus  <-tm_map(bbchealth_corpus,stripWhitespace)
inspect(bbchealth_corpus )

cnnhealth_corpus <-tm_map(cnnhealth_corpus, removeWords,stopwords("english"))
cnnhealth_corpus <-tm_map(cnnhealth_corpus, removeNumbers)
cnnhealth_corpus <-tm_map(cnnhealth_corpus, stripWhitespace)
inspect(cnnhealth_corpus )

foxhealth_corpus  <-tm_map(foxhealth_corpus , removeWords,stopwords("english"))
foxhealth_corpus  <-tm_map(foxhealth_corpus , removeNumbers)
foxhealth_corpus  <-tm_map(foxhealth_corpus , stripWhitespace)
inspect(foxhealth_corpus )

#wordcloud 
wordcloud(bbchealth_corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 100)
wordcloud(cnnhealth_corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 100)
wordcloud(foxhealth_corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 100)

#Create document-term matrix
bbchealth_dtm <-DocumentTermMatrix(bbchealth_corpus, control = list(minWordLength=c(3,Inf),bounds = list(global = c(40, Inf))))
bbchealth_dtm
cnnhealth_dtm <-DocumentTermMatrix(cnnhealth_corpus, control = list(minWordLength=c(3,Inf),bounds = list(global = c(40, Inf))))
cnnhealth_dtm
foxhealth_dtm <-DocumentTermMatrix(foxhealth_corpus, control = list(minWordLength=c(3,Inf),bounds = list(global = c(40, Inf))))
foxhealth_dtm

bbchealth_dtm2  <-as.matrix(bbchealth_dtm)
cnnhealth_dtm2  <-as.matrix(cnnhealth_dtm)
foxhealth_dtm2  <-as.matrix(foxhealth_dtm)

#K-means clustering 

head(bbchealth_dtm2)
bbc_dist <-dist(t(bbchealth_dtm2), method="euclidian")
kfit <-kmeans(bbc_dist, 3) 
bbc_dist
kfit
fviz_cluster(kfit,bbc_dist)

head(cnnhealth_dtm2)
cnn_dist <-dist(t(cnnhealth_dtm2), method="euclidian")
kfit <-kmeans(cnn_dist, 3)  
cnn_dist
kfit
fviz_cluster(kfit,cnn_dist)

head(foxhealth_dtm2)
fox_dist <-dist(t(foxhealth_dtm2), method="euclidian")   
kfit <-kmeans(fox_dist, 3)
fox_dist 
kfit
fviz_cluster(kfit,fox_dist)