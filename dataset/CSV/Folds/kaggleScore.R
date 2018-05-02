library(FFTrees)
library(tidyverse)
library(rhandsontable)
require(gridExtra)
setwd('/Users/ravirane/Desktop/GMU/CS584/project/creditRisk/dataset/CSV/Folds')
# Function to create Fast and Frugal Tree for given data and algo
fast.frugal.tree <- function(trainFile, testFile, algo, info) {
  print(info)
  loan.train <- read.csv(file=trainFile, header=TRUE, sep=",")
  loan.test <- read.csv(file=testFile, header=TRUE, sep=",")
  loan.fft <- FFTrees(formula = good_bad_flag ~ .,
                      data = loan.train,
                      data.test = loan.test,
                      algorithm = algo,
                      main = "Loan Data",
                      do.comp = FALSE,
                      decision.labels = c("Good", "Bad"))
  print(loan.fft)
  loan.fft
}
# Final model for train -
fold1.ifan.fft <- fast.frugal.tree("fold1_train.csv", "seq_test.csv", "ifan", 'Train Model')
plot(fold1.ifan.fft, data = "test")

testData <- read.csv(file="processed_testdata.csv", header=TRUE, sep=",")
a <- predict(fold1.ifan.fft, 
        data = testData)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
## True - 0 false - 1
func2 <- function(vector) {
  vr <- c() ### an empty vector 
  for (i in 1:length(vector)) { 
    x <- trim(vector[i])
    if(x == 'TRUE'){
      vr[i] <- 0
    } else {
      vr[i] <- 1
    }
  }
  vr
}
x <- func2(a)
## Creating submission file
MyData <-data.frame(customerid=testData['customerid'], Good_Bad_flag=x)
write.csv(MyData, file = "kaggle-test.csv" ,row.names=FALSE)


