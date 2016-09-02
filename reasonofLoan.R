# emplty environment 
rm(list= ls())

# set working directory 
setwd("~/R programming/loan data study/")

# import data set
dat <- read.csv("Data for Cleaning & Modeling.csv")

# import libraries 
library(stringr)
library(tm)
library(wordcloud)
library(slam)

# extract variable which is important
loan_reason <- as.data.frame((str_trim(dat$X16)))
names(loan_reason) <- 'Reason for loan'

#convert variable to character 
loan_reason$`Reason for loan` <- as.character(loan_reason$`Reason for loan`)

word <- c("years","one","trying","start","use","great","currently",
          "using","business","year","cardsbr","paid","card","able","now","can","allow",
          "make","used","get","years","small","loanbr","start","always","high","take",
          "monthly","plan","current","like","back","going","thank","well","last","much",
          "now","can")

#convert names into corpus
postcorpus <- Corpus(VectorSource(loan_reason$`Reason for loan`))

# convert all words to lowercase 
corpusClean <- tm_map(postcorpus, tolower)
#remove stopwords and words with high frequency which are not related to output
corpusClean <- tm_map(corpusClean, removeWords,c(stopwords('english'),word))
#remove numbers
corpusClean <- tm_map(corpusClean, removeNumbers)
#remove punctuations
corpusClean <- tm_map(corpusClean, removePunctuation)
#remove white spaces 
corpusClean <- tm_map(corpusClean, stripWhitespace)
#convert document to plain text format
corpusClean <- tm_map(corpusClean, PlainTextDocument)
corpusClean <- Corpus(VectorSource(corpusClean))

#build  wordcloud
pal2 = brewer.pal(7,"Dark2")
wordcloud(corpusClean,min.freq = 150,random.order = F,max.words = 100 , colors = pal2)      
