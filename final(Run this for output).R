# PROJECT -2 , TO FIND INTERST ON LOAN.
rm(list= ls())

# set working directory 
local_directory <- setwd("~/R programming/loan data study")

#Load all Libraries 
library(lubridate)
library(clusterSim)
library(corrplot)
library(rpart)
library(rpart.plot)

# import data set
dat <- read.csv("data/Data for Cleaning & Modeling.csv", header = T, 
                na.strings = c(""," ","NA"),stringsAsFactors = F)

# understand data set
# Understand the structure of data set 
str(dat)

# Descriptive statistics of data set
summary(dat)

# Pre-Processing data 
# Removing un-necessary data from data set 
# no. of missing variables
na_df <- data.frame(apply(is.na(dat), 2, sum))
na_df <- t(na_df)
row.names(na_df) <- "no. of missing values"

# Removing data which is not important for intrest rate calculation
dat <- dat[, c(-2, -3, -9, -10, -16, -18, -19, -20, -25, -26)]

# Removing data points with missing values
dat <- dat[!is.na(dat$X1),]
dat <- dat[!is.na(dat$X8),]
dat <- dat[!is.na(dat$X13),]
dat <- dat[!is.na(dat$X30),]

# Formating data for analysis
# convert percentage to numeric fractions
dat$X1 <- (as.numeric(sub("%", "", dat$X1)))/100

# remove "$" & "," from data and convert them to numeric
dat$X4 <- as.numeric(gsub("[[:punct:]]", "", dat$X4))
dat$X5 <- as.numeric(gsub("[[:punct:]]", "", dat$X5))
dat$X6 <- as.numeric(gsub("[[:punct:]]", "", dat$X6))

# convert character varibales to factor and store them in terms of numbers as numbers take less space than characters
# "36 months" = 0 & "60 months" = 1
dat$X7 <- as.factor(ifelse(dat$X7 == " 36 months", 0, 1))

# A = 1, B = 2 ...F = 6 & G = 7
dat$X8 <- as.factor(ifelse(dat$X8 == "A", 1,
                            ifelse(dat$X8 == "B", 2,
                                   ifelse(dat$X8 == "C", 3,
                                          ifelse(dat$X8 == "D", 4,
                                                 ifelse(dat$X8 == "E", 5,
                                                        ifelse(dat$X8 == "F", 6, 7)))))))

# "<1 year" = 0, "1 year" = 1 ... "9 years" = 9, "10+ years" = 10
dat$X11 <- as.factor(ifelse(dat$X11 == "< 1 year", 0,
                            ifelse(dat$X11 == "1 year", 1,
                                   ifelse(dat$X11 == "2 years", 2,
                                          ifelse(dat$X11 == "3 years", 3,
                                                 ifelse(dat$X11 == "4 years", 4,
                                                        ifelse(dat$X11 == "5 years", 5,
                                                               ifelse(dat$X11 == "6 years", 6,
                                                                      ifelse(dat$X11 == "7 years", 7,
                                                                             ifelse(dat$X11 == "8 years", 8,
                                                                                    ifelse(dat$X11 == "9 years", 9, 10)))))))))))

# "OTHER","ANY",NA = 0,"NONE" = 1,"OWN" = 2,"RENT" = 3, "MORTGAGE" = 4
dat$X12 <- as.factor(ifelse(dat$X12 %in% c("OTHER", "ANY", NA), 0,
                            ifelse(dat$X12 == "NONE", 1,
                                   ifelse(dat$X12 == "OWN", 2,
                                          ifelse(dat$X12 == "RENT", 3, 4)))))

# "VERIFIED - income" = 0,"VERIFIED- income source" = 1, "not-verified" = 2
dat$X14 <- as.factor(ifelse(dat$X14 == "VERIFIED - income" , 0,
                            ifelse(dat$X14 == "VERIFIED - income source" , 1, 2)))

# convert into date variable, since date requires all date,month and year adding "1-" to all data points for date
dat$X15 <- as.Date(paste("01",dat$X15,sep = "-"), "%d-%b-%y")
dat$X23 <- as.Date(paste("01",dat$X23,sep = "-"), "%d-%b-%y")

# check summary of date varibales
summary(dat[, c(12, 18)])

# as.Date converts all years above 69 in 20th century and below it 21st century ,
#  to compansate for that create a function
library(lubridate)
year(dat$X23) <- ifelse(year(dat$X23) > 2016 , year(dat$X23) - 100, year(dat$X23) )
dat$X34 <- year(dat$X23)
dat <- dat[, -15]

# convert from character to factor
dat$X17 <- as.factor(ifelse(dat$X17 %in% c("car", "house", "major_purchse"), 0,
                            ifelse(dat$X17 %in% c("small_business","renewable_energy"), 1,
                                   ifelse(dat$X17 %in% c("credit_card","debt_consolidation"), 2,
                                          ifelse(dat$X17 %in% c("medical", "vacation", "moving", "wedding",
                                                                "home_improvement", "education"), 3, 4)))))
dat$X32 <- as.factor(dat$X32)

# convert percentage to fraction
dat$X30 <- (as.numeric(sub("%", "", dat$X30)))/100

#  creating a new variable :- fraction of loan funded by investor
dat$X33 <- dat$X6/dat$X5


#remove outliers
#create a function to convert outliers into NA 
outlier <- function(x, n){
  a <- quantile(x, 0.75) 
  b <- quantile(x, 0.25)
  c <- a - b
  IQR <- n * c
  mi <- b - IQR
  ma <- a + IQR
  x <- ifelse(x < mi, NA,
              ifelse(x > ma, NA, x))
  return(x)
}

# convert extreme outliers to NA
dat$X13 <- outlier(dat$X13, 3)
dat$X27 <- outlier(dat$X27, 3)
dat$X29 <- outlier(dat$X29, 3)
dat$X31 <- outlier(dat$X31, 3)

# Convert faulty data to NA 
quantile(dat$X22, 0.99)
dat$X22 <- ifelse(dat$X22 > 4, NA, dat$X22)
quantile(dat$X24, 0.99)
dat$X24 <- ifelse(dat$X24 > 4, NA, dat$X24)
dat$X30 <- ifelse(dat$X30 > 1, NA, dat$X30)

# Remove NA
dat <- na.omit(dat)

# Normalise the data by min-max method
nums <- sapply(dat, is.numeric)
dat[, nums] <- data.Normalization(dat[, nums], type = "n4") 

#Build correlation plot between nnumeric variables
corrplot(cor(dat[, nums]), method = "number", order = "hclust")

# Remove highly correlated data and check again for correlation
dat <- dat[,c(-2,-4,-20)]
nums <- sapply(dat, is.numeric)
corrplot(cor(dat[,nums]),method = "number",order = "hclust")

# Build Regression Models
# divide data into train and test
dat <- dat[order(runif(232252)),]
train <- dat[1:170000,]
test1 <- dat[170001:232252,]

# MODEL-1 :- linear regression
model1 <- lm(X1 ~., data = train)
pred1 <- predict(model1,test1[,-1])
cor(pred1,test1$X1)

#Model-2 :- Regression Tree Model,R.part
model2 <- rpart(X1~., data = train)
pred2 <- predict(model2,test1[,-1])
cor(pred2,test1$X1)

#visualizing rules
rpart.plot(model2,digits = 3, type = 2)

#check Mean Absolute Error (MAE)
MAE <- function(pred, actual){
  mean(abs(pred-actual))
}
MAE(pred1,test1$X1)
MAE(pred2,test1$X1)

# save linear regression model
save(model1,file = "output/linear_regression.rda")
save(model2,file = "output/regression_trees.rda")

# save output of model
out1 <- capture.output(summary(model1))
cat("Regression Model test-1",out1,file = "output/model_summary.txt",sep = "\n",append = T)
out2 <- capture.output(summary(model2))
cat("Regression Model test-2",out2,file = "output/model_summary.txt",sep = "\n",append = T)


# run pre-processing on test data
source(paste(local_directory,"R/test.R",sep = "/"))

# Predict target variable 
pred_1 <- predict(model1,test)
pred_2 <- predict(model2,test)

