# remove all data from environment
rm(list= ls())

# set working directory
setwd("~/R programming/loan data study/")

# import data set
dat <- read.csv("Data for Cleaning & Modeling.csv")

#import libraries 
library(stringr)
library(tm)
library(wordcloud)
library(slam)

# extract target varible
job_title <- as.data.frame((str_trim(dat1$X10)))
names(job_title) <- 'Job Title'

#convert variable to character
job_title$`Job Title` <- as.character(job_title$`Job Title`)


#convert names into corpus
postcorpus <- Corpus(VectorSource(job_title$`Job Title`))

# convert all words to lowercase 
corpusClean <- tm_map(postcorpus, tolower)
#remove stopwords 
corpusClean <- tm_map(corpusClean, removeWords,c(stopwords('english')))
#remove numbers
corpusClean <- tm_map(corpusClean, removeNumbers)
#remove punctuations
corpusClean <- tm_map(corpusClean, removePunctuation)
#remove white spaces 
corpusClean <- tm_map(corpusClean, stripWhitespace)
#convert document to plain text format
corpusClean <- tm_map(corpusClean, PlainTextDocument)
corpusClean <- Corpus(VectorSource(corpusClean))

# build wordcloud
pal2 = brewer.pal(8,"Dark2")
wordcloud(corpusClean,min.freq = 150,random.order = F,max.words = 150 , colors = pal2)       
