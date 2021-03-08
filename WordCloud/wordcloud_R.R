
if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(wordcloud2)){install.packages("wordcloud2")} 
if(!require(grDevices)){install.packages("grDevices")} 
if(!require(dplyr)){install.packages("dplyr")} 
if(!require(lubridate)){install.packages("lubridate")} 
if(!require(stringr)){install.packages("stringr")} 
if(!require(jiebaR)){install.packages("jiebaR")} 


library(grDevices)
library(wordcloud2)
library(dplyr)
library(lubridate)
library(stringr)
library(jiebaR)
library(wordcloud) 

tsai<-read.csv("G:/E_Business/Homework/wordcloud/tsai.csv")
han<-read.csv("G:/E_Business/Homework/wordcloud/han.csv")
cutter<-worker(user="G:/E_Business/Homework/wordcloud/newwords.txt",stop_word="G:/E_Business/Homework/wordcloud/stopwords.txt",bylines=T)

###################### tsai's wordcloud
content_tsai<-tsai[1,1]

for(i in c(2:nrow(tsai)-1))
{
  temp_text<-as.character(tsai[i,1])
  content_tsai<-paste(content_tsai,temp_text)
}
content_tsai
content_tsai<-str_remove_all(content_tsai,"[0-9a-zA-Z]+?")

content_tsai_tokenized<-segment(content_tsai,cutter)
content_tsai_tokenized[1]

tsai_freq<-freq(content_tsai_tokenized[[1]])
tsai_freq<-arrange(tsai_freq,desc(freq))

tsai_highfreq<-subset(tsai_freq,freq>50)
tsai_highfreq
wordcloud2(tsai_highfreq,fontFamily="Microsoft YaHei",size=1)

######################## han's wordcloud
content_han<-han[1,1]

for(i in c(2:nrow(han)-1))
{
  temp_text<-as.character(han[i,1])
  content_han<-paste(content_han,temp_text)
}
content_han
content_han<-str_remove_all(content_han,"[0-9a-zA-Z]+?")

content_han_tokenized<-segment(content_han,cutter)
content_han_tokenized[1]

han_freq<-freq(content_han_tokenized[[1]])
han_freq<-arrange(han_freq,desc(freq))

han_highfreq<-subset(han_freq,freq>50)
han_highfreq
wordcloud2(han_highfreq,fontFamily="Microsoft YaHei",size=1)
