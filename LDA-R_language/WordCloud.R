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

setwd("C:\\Users\\Happy Friends\\Desktop\\NEW")
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
                                 "green",
                                 "computing",
                                 "study",
                                 "said",
                                 "among",
                                 "first",
                                 "we<e3><e2><e2>d"))

###create a Term DOcument Matrix###
tdm<-TermDocumentMatrix(docs)
tdm<-as.matrix(tdm)

###word Cloud###
rs<-sort(rowSums(tdm), decreasing = TRUE)
set.seed(1234)
wordcloud(words = names(rs),freq = rs,
          random.order = F,
          max.words = 100, ##maximum number of words to be plotted
          min.freq = 15, ## words with frequency below min.freq will not be plotted
          colors = rainbow(10),
          rot.per = .0) ##proportion words with 90 degree rotation



# Comparison WordCloud
rs<- tibble(phrases = GreenComp$Text)
rs$ID<- row.names(rs)

tidy_docs <- rs %>% unnest_tokens(word, phrases)

tidy_docs %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = rainbow(10),
                   title.size=1, max.words = 1000, scale=c(3, .3))




#############################################################################
# More comparison cloud
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(wordcloud)
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  ###CLEAN the data###
  removeNumPunct<-function(x) gsub("[^[:alpha:][:space:][:alnum:][^a-zA-Z0-9]]*","",x)
  docs<-tm_map(docs, content_transformer(removeNumPunct))
  docs<-tm_map(docs, stripWhitespace)
  docs<-tm_map(docs, removeWords, stopwords("english"))
  docs<-tm_map(docs, removeNumbers)
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
                                   "green",
                                   "computing",
                                   "study",
                                   "said",
                                   "among",
                                   "first",
                                   "we<e3><e2><e2>d"))
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}
key <- "0699354273022f599ccfae0415c33d23"
getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  ##########################################
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  ###################################
  return(list(sentiment=sentiment))
}
#Set the working directory
setwd("C:\\Users\\Happy Friends\\Desktop\\NEW")
GreenComp<-read_excel("Green-Computing.xlsx")
docs<-iconv(GreenComp$Text, to = "utf-8")

# clean text
tweet_clean = clean.text(docs)
tweet_num = length(tweet_clean)

#Create a data frame
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

# apply function getSentiment
sentiment = rep(0, tweet_num)
for (i in 1:tweet_num)
{
  tmp = getSentiment(tweet_clean[i], key)
  
  tweet_df$sentiment[i] = tmp$sentiment
  
  print(paste(i," of ", tweet_num))
}

# delete rows with no sentiment
tweet_df <- tweet_df[tweet_df$sentiment!="",]


#separate text by sentiment
sents = levels(factor(tweet_df$sentiment))

# get the labels and percents

labels <- lapply(sents, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"))

#Then we create the so called docs for each category and add the tweet texts to these categories:
nemo = length(sents)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = tweet_df[tweet_df$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("german"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.9), random.order = FALSE, title.size = 1)

