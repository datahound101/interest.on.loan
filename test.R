# import data set
test <- read.csv("Holdout for Testing.csv", header = T, 
                na.strings = c("", " ", NA),stringsAsFactors = F)

# Removing data which is not important for intrest rate calculation
test <- test[,c(-1, -3, -10, -16, -18, -19, -23, -25, -26)]

# Formating data for analysis
# remove "$" & "," from data and convert them to numeric
test$X4 <- as.numeric(gsub("[[:punct:]]","",test$X4))
test$X5 <- as.numeric(gsub("[[:punct:]]","",test$X5))
test$X6 <- as.numeric(gsub("[[:punct:]]","",test$X6))

#convert character varibales to factor and store them in terms of numbers as numbers take less space than characters
# "36 months" = 0 & "60 months" = 1
test$X7 <- as.factor(ifelse(test$X7 == " 36 months",0,1))

# A = 1, B = 2 ...F = 6 & G = 7
test$X8 <- as.factor(ifelse(test$X8 == "A",1,
                           ifelse(test$X8 == "B",2,
                                  ifelse(test$X8 == "C",3,
                                         ifelse(test$X8 == "D",4,
                                                ifelse(test$X8 == "E",5,
                                                       ifelse(test$X8 == "F",6,7)))))))

# "<1 year" = 0, "1 year" = 1 ... "9 years" = 9, "10+ years" = 10
test$X11 <- as.factor(ifelse(test$X11 == "< 1 year",0,
                            ifelse(test$X11 == "1 year",1,
                                   ifelse(test$X11 == "2 years",2,
                                          ifelse(test$X11 == "3 years",3,
                                                 ifelse(test$X11 == "4 years",4,
                                                        ifelse(test$X11 == "5 years",5,
                                                               ifelse(test$X11 == "6 years",6,
                                                                      ifelse(test$X11 == "7 years",7,
                                                                             ifelse(test$X11 == "8 years",8,
                                                                                    ifelse(test$X11 == "9 years",9,10)))))))))))

#"OTHER","ANY",NA = 0,"NONE" = 1,"OWN" = 2,"RENT" = 3, "MORTGAGE" = 4
test$X12 <- as.factor(ifelse(test$X12 %in% c("OTHER", "ANY", NA), 0,
                            ifelse(test$X12 == "NONE", 1,
                                   ifelse(test$X12 == "OWN", 2,
                                          ifelse(test$X12 == "RENT", 3, 4)))))

#"VERIFIED - income" = 0,"VERIFIED- income source" = 1, "not-verified" = 2
test$X14 <- as.factor(ifelse(test$X14 == "VERIFIED - income" ,0,
                            ifelse(test$X14 == "VERIFIED - income source" ,1,2)))


#
test$X15m = as.factor(gsub("[0-9 -]", "", test$X15))
test$X15 = NULL

#convert from character to factor
test$X17 <- as.factor(ifelse(test$X17 %in% c("car","house","major_purchse"),0,
                            ifelse(test$X17 %in% c("small_business","renewable_energy"),1,
                                   ifelse(test$X17 %in% c("credit_card","debt_consolidation"),2,
                                          ifelse(test$X17 %in% c("medical","vacation","moving","wedding",
                                                                "home_improvement","education"),3,4)))))
test$X32 <- as.factor(test$X32)

#convert percentage to fraction
test$X30 <- (as.numeric(sub("%","",test$X30)))/100

#  creating a new variable :- fraction of loan funded by investor
test$X33 <- test$X6/test$X5
test$X5 = NULL
test$X6 = NULL
test$X2 = as.character(test$X2)
#remove outliers
#create a function to convert outliers into NA 
outlier <- function(x, n = 1.5){
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


#convert extreme outliers to NA
test$X13 <- outlier(test$X13,3)
test$X27 <- outlier(test$X27,3)
test$X29 <- outlier(test$X29,3)
test$X31 <- outlier(test$X31,3)

# Convert faulty data to NA 
quantile(test$X220.99)
test$X22 <- ifelse(test$X22 > 4, NA, test$X22)
quantile(test$X24,0.99)
test$X24 <- ifelse(test$X24 > 4, NA, test$X24)
test$X30 <- ifelse(test$X30 > 1 ,NA, test$X30)

#Remove NA
test <- na.omit(test)

# Normalise the data by min-max method
nums <- sapply(test, is.numeric)
test[,nums] <- data.Normalization(test[,nums], type = "n4") 
test_id = test$X2
test$X2 = NULL



