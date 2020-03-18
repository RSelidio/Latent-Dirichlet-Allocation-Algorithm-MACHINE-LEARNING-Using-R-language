library(RSentiment)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(igraph)
library(servr)
library(scales)
library(reshape2)
library(lubridate)
library(twitteR)
library(RCurl)
library(wordcloud2)
library(tidyverse)
library(tidytext)
library(lda)
library(topicmodels)
library(textdata)
library(tidyr)

###set working dir###

setwd("F:\\PSITE6\\PSITE6(2)\\final")
GreenComp<-read_excel("Green-Computing.xlsx")
docs<-iconv(GreenComp$Text, to = "utf-8")

###BUILD the corpus###
docs<-Corpus(VectorSource(docs))
docs<-tm_map(docs,content_transformer(tolower))

###CLEAN the data###
removeNumPunct<-function(x) gsub("[^[:alpha:][:space:][:alnum:][^a-zA-Z0-9]]*","",x)
docs<-tm_map(docs, content_transformer(removeNumPunct))
docs<-tm_map(docs, stripWhitespace)
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
docs<-tm_map(docs, gsub, pattern="manila<e3><e2><e2>",replace="manila")
docs<-tm_map(docs, gsub, pattern="challenge<e3><e2><e2>",replace="challenge")


docs<-tm_map(docs, removeWords,c("<e3><e2><e5>we",
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
                                 "said",
                                               ./ "among",
                                 "first",
                                 "we<e3><e2><e2>d"))

###create a Term DOcument Matrix###
tdm<-TermDocumentMatrix(docs)
tdm<-as.matrix(tdm)

# Fitting Labels
par(las=2) # make label text perpendicular to axis
par(mar=c(5.5,8,2,2)) # increase y-axis margin.


#### Create Bar Plot/Graph###
rs<-rowSums(tdm)
rs<-sort(rowSums(tdm), decreasing = TRUE)
write.csv(rs,"GreenCompTDM.csv")
rs<-subset(rs, rs>35)         


x<-barplot(rs,
           las = 2,
           col = rainbow(10),
           beside = TRUE,
           legend.text = TRUE,
           args.legend = list(bty="n", horiz=FALSE),
           ylim = c(0, 500),
           ylab = "Frequency",
           main = "Top Words")

y<-as.matrix(rs)
text(x,y+(-18), labels = as.character(y))


###Sentiment Bar Plot###
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = "Frequency",
        main = "Sentiment Scores")
