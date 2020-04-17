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

covid20200131 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-01-31" & date < "2020-02-01")
covid20200229 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-02-29" & date < "2020-03-01")
covid20200315 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-03-15" & date < "2020-03-16")
covid20200331 = news %>% filter(virusName == "COVID-19") %>% filter(date >= "2020-03-31" & date < "2020-04-01")


covid201912 = paste(covid201912$article, collapse = " ")
covid202001 = paste(covid202001$article, collapse = " ")
covid202002 = paste(covid202002$article, collapse = " ")
covid202003 = paste(covid202003$article, collapse = " ")

covid20200131 = paste(covid20200131$article, collapse = " ")
covid20200229 = paste(covid20200229$article, collapse = " ")
covid20200315 = paste(covid20200315$article, collapse = " ")
covid20200331 = paste(covid20200331$article, collapse = " ")


wordcloud_function = function(news, virus_name, start, end) {
  
  vir = news %>% filter(virusName == virus_name) %>% filter(date >= start & date < end)
  vir = paste(vir$article, collapse = " ")
  
  corpus <- VCorpus(VectorSource(vir))
  ##Removing Punctuation
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  ##Removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  ##Converting to lowercase
  #corpus <- tm_map(corpus, content_transformer(tolower))
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

wordcloud_function(news, "COVID-19", "2020-03-15", "2020-03-31")








## 낱개 코드


##
corpus <- VCorpus(VectorSource(covid20200331))
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

wordcount
wordcloud2(data=wordcount,fontFamily = '나눔바른고딕', color = "random-light", backgroundColor = "grey")



