#Load all the libraries
library(RSentiment)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(readxl)
#library(dplyr)#In Sentiment turn this to comment status
library(stringr)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(igraph)
#library(servr)#In Sentiment turn this to comment status
library(scales)
library(reshape2)
#library(lubridate) #In Sentiment turn this to comment status
#library(twitteR) #In Sentiment turn this to comment status
library(RCurl)
library(wordcloud2)
library(tidyverse)
library(tidytext)
library(lda)
library(topicmodels)
library(textdata)
library(tidyr)
############################
#Set working directory and load the data
setwd("F:\\PSITE6\\PSITE6(2)\\final")
GreenComp<-read_excel("Green-Computing.xlsx")
docs<-iconv(GreenComp$Text, to = "utf-8")
############################
#Build a Corpus
docs<-Corpus(VectorSource(docs))
docs<-tm_map(docs,content_transformer(tolower))
############################
# Data Cleaning
removeNumPunct<-function(x) gsub("[^[:alpha:][:space:][:alnum:][^a-zA-Z0-9]]*","",x)
docs<-tm_map(docs, content_transformer(removeNumPunct))
docs<-tm_map(docs, stripWhitespace) #To remove extra spaces
docs<-tm_map(docs, removeWords, stopwords("english"))
docs<-tm_map(docs, removeNumbers)




docs<-tm_map(docs, gsub, pattern="country<e3><e2><e2>s",replace="country")
docs<-tm_map(docs, gsub, pattern="region<e3><e2><e2>s",replace="region")
docs<-tm_map(docs, gsub, pattern="government<e3><e2><e2>s",replace="government")
docs<-tm_map(docs, gsub, pattern="people<e3><e2><e2>s",replace="people")
docs<-tm_map(docs, gsub, pattern="month<e3><e2><e2>s",replace="month")
docs<-tm_map(docs, gsub, pattern="visayas<e3><e2><e2>",replace="visayas")
docs<-tm_map(docs, gsub, pattern="communities<e3><e2><e2>",replace="communities")
docs<-tm_map(docs, gsub, pattern="leyte<e3><e2><e2>s",replace="leyte")
docs<-tm_map(docs, gsub, pattern="growth<e3><e2><e2>",replace="growth")
docs<-tm_map(docs, gsub, pattern="sell<e3><e2><e2>it",replace="sell")
docs<-tm_map(docs, gsub, pattern="towns<e3><e2><e2>",replace="towns")
docs<-tm_map(docs, gsub, pattern="water<e3><e2><e2>",replace="water")
docs<-tm_map(docs, gsub, pattern="<e3><e2><e5>extreme",replace="extreme")
docs<-tm_map(docs, gsub, pattern="<e3><e2><e5>imperial",replace="imperial")
docs<-tm_map(docs, gsub, pattern="<e3><e2><e5>tubig",replace="water")
docs<-tm_map(docs, gsub, pattern="seaman<e3><e2><e2>",replace="seaman")
docs<-tm_map(docs, gsub, pattern="samar<e3><e2><e5>basta",replace="samar")
docs<-tm_map(docs, gsub, pattern="manila<e3><e2><e2>",replace="manila")
docs<-tm_map(docs, gsub, pattern="challenge<e3><e2><e2>",replace="challenge")
docs<-tm_map(docs, gsub, pattern="<e3><e2><e5>tubig",replace="water")
docs<-tm_map(docs, gsub, pattern="<e3><e2><e5>tubig",replace="water")
docs<-tm_map(docs, gsub, pattern="students<e3><e2><e2>",replace="student")
docs<-tm_map(docs, gsub, pattern="<e3><e2><e2>",replace="0")


docs<-tm_map(docs, removeWords,c( "<e3><e2><e2>",
                                  "<e3><e2><e2>",
                                  "<e3><e2><e5>we",
                                 "also",
                                 "<e3><e2><e5>this",
                                 "<e3><e2><e5>minsan",
                                 "challenge",
                                 "kami<e3><e2><e2>",
                                 "<e3><e2><e5>sana",
                                 "doj<e3><e2><e2>",
                                 "can<e3><e2><e2>t",
                                 "<e3><e2><e5>basta",
                                 "<e3><e2><e5>",
                                 "will",
                                 "green",
                                 "<e3><e2><e2>",
                                 "computing",
                                 "study",
                                 "said",
                                 "among",
                                 "<e3><e2><eb>green<e3><e2><e2>",
                                 "first",
                                 "will",
                                 "green",
                                 "computing",
                                 "study",
                                 "said",
                                 "among",
                                 "first",
                                 "we<e3><e2><e2>d")) #used to remove specific word

#########################
# Create a term document matrix
tdm<-TermDocumentMatrix(docs)
tdm<-as.matrix(tdm)

###############################
# Text Mining
tdm<-TermDocumentMatrix(docs)
m<-as.matrix(tdm)
v<-sort(rowSums(m), decreasing = TRUE)
d<-data.frame(word = names(v), freq=v)
head(d, 10)

findFreqTerms(tdm, 200)
corr_p<-findAssocs(tdm, terms = "GreenComp", corlimit = 0.6)
corr_p
write.csv(corr_p,"corr_p.csv")
corr_per<-findAssocs(tdm, terms = "percent", corlimit = 0.6)
#########################
# Sentiment Analysis
GreenComp<-read_excel("Green-Computing.xlsx")
tw<-iconv(GreenComp$Text, to = "utf-8")

s<-get_nrc_sentiment(tw)
write.csv(s, "GCSenti.csv")
head(s)
# Examine text
tw[4]
get_nrc_sentiment("percent")
get_nrc_sentiment("GreenComp")
# Sentiment Bar Plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = "Frequency",
        main = "Student Sentiment Scores")

###############################
#Topic modeling
dtm<-DocumentTermMatrix(docs)
rownames(dtm)<-GreenComp$Text
freq<-colSums(as.matrix(dtm))
length(freq)

# Set Paramenters for Gibbs Sampling
burnin<-4000
iter<-2000
thin<-500
seed<-list(2003,5,63,100001,765)
nstart<-5
best<-TRUE

#Number of Topics
k<-50
#Run LDA using Gibbs Sampling
ldaOut<-LDA(dtm,k,method = 'Gibbs', control = list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))

#documents to topics
ldaOut.topics<-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics, file = paste("LDAGibbs", k, "DocsToTopics.csv"))

#Top 20 terms in eac topic
ldaOut.terms<-as.matrix(terms(ldaOut, 20))
write.csv(ldaOut.terms,file = paste("LDAGibbs", k, "TopicsToTerms.csv"))

#Probabilities of the topics per docs
topicProb<-as.data.frame(ldaOut@gamma)
write.csv(topicProb, file = paste("LDAGibbs",k,"TopicProbabilities.csv"))

