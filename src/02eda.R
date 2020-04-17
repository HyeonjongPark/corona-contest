rm(list=ls())

library(lubridate)
library(dplyr)
library(tidyverse)
library(data.table)
library(devtools)
#devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(KoNLP)
library(tm)
library(SnowballC)

library(ggplot2)
library(ggthemes)

setwd("E:/data-analysis/contest/corona-contest/")

news = fread("./data/공모전데이터/파생데이터/newslist.csv")
Encoding(names(news)) = "UTF-8"
names(news)
names(news) = c("fileName", "title", "date", "virusName", "category", "article")

news$date = ymd_hms(news$date)

news %>% str()
news$virusName %>% table() %>% sort()


art = news %>% group_by(virusName, date) %>% summarise(art_count = n())

influ = art %>% filter(virusName == "Influenza") %>% arrange(date) 
influ %>% head 
influ %>% ggplot(aes(x = date, y = art_count)) + geom_bar(stat = "identity", color ="red")


covid = art %>% filter(virusName == "COVID-19") %>% arrange(date) 
covid = covid[-c(1,2,3,4),]
covid %>% head 
covid %>% ggplot(aes(x = date, y = art_count)) + geom_bar(stat = "identity", color ="red")

covid %>% head()
covid %>% tail()

covid201912 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2019-12-01" & date < "2020-01-01")
covid202001 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-01-01" & date < "2020-02-01")
covid202002 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-02-01" & date < "2020-03-01")
covid202003 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-03-01" & date < "2020-04-01")

covid201912 = paste(covid201912$article, collapse = " ")
covid202001 = paste(covid202001$article, collapse = " ")
covid202002 = paste(covid202002$article, collapse = " ")
covid202003 = paste(covid202003$article, collapse = " ")


wordcloud_function = function(dat) {
  corpus <- VCorpus(VectorSource(dat))
  ##Removing Punctuation
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  ##Removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  ##Converting to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  ##Removing stop words
  corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
  ##Stemming
  corpus <- tm_map(corpus, stemDocument)
  ##Whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  
  # Create Document Term Matrix
  dtm <- DocumentTermMatrix(corpus)
  # Removing all terms whose sparsity is greater than 95% 
  corpus <- removeSparseTerms(dtm, 0.95)
  
  colS <- colSums(as.matrix(corpus))
  doc_features <- data.table(name = attributes(colS)$names, count = colS)
  doc_features = doc_features %>% arrange(desc(count))
  doc_features = as.data.table(doc_features)
  
  names(doc_features) = c("noun", "Freq")
  wordcount = doc_features
  wordcount = as.data.frame(wordcount)
  wordcount = wordcount %>% head(100)
  
  wordcloud2(data=wordcount,fontFamily = '나눔바른고딕', color = "random-light", backgroundColor = "grey")
  
}

wordcloud_function(covid202001)


corpus <- VCorpus(VectorSource(covid202001))
##Removing Punctuation
corpus <- tm_map(corpus, content_transformer(removePunctuation))
##Removing numbers
corpus <- tm_map(corpus, removeNumbers)
##Converting to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
##Removing stop words
corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
##Stemming
corpus <- tm_map(corpus, stemDocument)
##Whitespace
corpus <- tm_map(corpus, stripWhitespace)


# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
# Removing all terms whose sparsity is greater than 95% 
corpus <- removeSparseTerms(dtm, 0.95)


colS <- colSums(as.matrix(corpus))
doc_features <- data.table(name = attributes(colS)$names, count = colS)
doc_features = doc_features %>% arrange(desc(count))
doc_features %>% head()
doc_features = as.data.table(doc_features)
ggplot(doc_features[count>15],aes(name, count)) + geom_bar(stat = "identity",fill='lightblue',color='black')+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme_economist()+ scale_color_economist()



names(doc_features) = c("noun", "Freq")
wordcount = doc_features
wordcount = as.data.frame(wordcount)
wordcount = wordcount %>% head(100)

wordcloud2(data=wordcount,fontFamily = '나눔바른고딕', color = "random-light", backgroundColor = "grey")



covid201912_2 = sapply(covid201912, extractNoun, USE.NAMES = F)
covid201912_3 = unlist(covid201912_2)

covid201912_4 <- gsub('\\d+','',covid201912_3)
covid201912_4 <- gsub('-','',covid201912_4)
covid201912_4 <- gsub('""','',covid201912_4)
covid201912_4 <- gsub('//.','',covid201912_4)


covid201912_4 %>% head(200)

wordcount <- table(covid201912_4)
wordcount <- head(sort(wordcount,decreasing=T),200)
wordcount

