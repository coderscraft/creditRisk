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
# Creating model on fold
fold1.ifan.fft <- fast.frugal.tree("fold1_train.csv", "fold1_test.csv", "ifan", 'Fold 1 FFT - Algo: ifan')
fold2.ifan.fft <- fast.frugal.tree("fold2_train.csv", "fold2_test.csv", "ifan", 'Fold 2 FFT - Algo: ifan')
fold3.ifan.fft <- fast.frugal.tree("fold3_train.csv", "fold3_test.csv", "ifan", 'Fold 3 FFT - Algo: ifan')
fold1.dfan.fft <- fast.frugal.tree("fold1_train.csv", "fold1_test.csv", "dfan", 'Fold 1 FFT - Algo: dfan')
fold2.dfan.fft <- fast.frugal.tree("fold2_train.csv", "fold2_test.csv", "dfan", 'Fold 2 FFT - Algo: dfan')
fold3.dfan.fft <- fast.frugal.tree("fold3_train.csv", "fold3_test.csv", "dfan", 'Fold 3 FFT - Algo: dfan')
# Plotting fold model tree
plot(fold1.ifan.fft, data = "test")
loan.test <- read.csv(file='fold1_test.csv', header=TRUE, sep=",")
loan.test.csv <- read.csv(file='test.csv', header=TRUE, sep=",")
a <- predict(fold1.ifan.fft, data=loan.test.csv )
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
get <- function(x) {
  x <- trim(x)
  print(paste0("Current working dir: ", x))
  if(x){
    y <- 1
  } else {
    y <- 0
  }
  y
}

func2 <- function(vector) {
  vr <- c() ### an empty vector 
  for (i in 1:length(vector)) { 
    x <- trim(vector[i])
    print(paste0("Current working dir: ", x))
    if(x == 'TRUE'){
      vr[i] <- 0
    } else {
      vr[i] <- 1
    }
  }
  vr
}
a
x <- func2(a)

MyData <-data.frame(loan.test.csv['customerid'], x)
MyData
write.csv(MyData, file = "MyData.csv" ,row.names=FALSE,col.names=FALSE)

plot(fold2.ifan.fft, data = "test")
plot(fold3.ifan.fft, data = "test")
plot(fold1.dfan.fft, data = "test")
plot(fold2.dfan.fft, data = "test")
plot(fold3.dfan.fft, data = "test")


folds <- c('Fold1', 'Fold2', 'Fold3')
#confusion matrix for fold_ifan
tp_ifan <- c(0, 36, 141 )
fp_ifan <- c(103, 24, 0)
tn_ifan <- c(97, 76, 0)
fn_ifan <- c(0, 64, 59)

cm_fold_ifan = tibble(FOLD= folds, TP = tp_ifan, TN = tn_ifan, FP = fp_ifan, FN = fn_ifan)
cm_fold_ifan$accuracy <- round((cm_fold_ifan$TP + cm_fold_ifan$TN)/(cm_fold_ifan$TP + cm_fold_ifan$TN + cm_fold_ifan$FP + cm_fold_ifan$FN), digits = 2)
cm_fold_ifan$precision <- round(cm_fold_ifan$TP/(cm_fold_ifan$TP + cm_fold_ifan$FP), digits = 2)
cm_fold_ifan$recall <- round(cm_fold_ifan$TP/(cm_fold_ifan$TP + cm_fold_ifan$FN), digits = 2)
cm_fold_ifan$f <- round(2*cm_fold_ifan$recall*cm_fold_ifan$precision/(cm_fold_ifan$precision + cm_fold_ifan$recall), digits = 2)
cm_fold_ifan$sensitivity <- c(0.48, 0.76, NaN)
cm_fold_ifan$specificity <- c(NaN, 0.36, 0.70)
rhandsontable(cm_fold_ifan, rowHeaders = NULL)


#confusion matrix for fold_dfan
tp_dfan <- c(0, 73, 164)
fp_dfan <- c(88, 72, 0)
tn_dfan <- c(112, 28, 0)
fn_dfan <- c(0, 27, 36)
cm_fold_dfan = tibble(FOLD= folds, TP = tp_dfan, TN = tn_dfan, FP = fp_dfan, FN = fn_dfan)
cm_fold_dfan$accuracy <- round((cm_fold_dfan$TP + cm_fold_dfan$TN)/(cm_fold_dfan$TP + cm_fold_dfan$TN + cm_fold_dfan$FP + cm_fold_dfan$FN), digits = 2)
cm_fold_dfan$precision <- round(cm_fold_dfan$TP/(cm_fold_dfan$TP + cm_fold_dfan$FP), digits = 2)
cm_fold_dfan$recall <- round(cm_fold_dfan$TP/(cm_fold_dfan$TP + cm_fold_dfan$FN), digits = 2)
cm_fold_dfan$f <- round(2*cm_fold_dfan$recall*cm_fold_dfan$precision/(cm_fold_dfan$precision + cm_fold_dfan$recall), digits = 2)
cm_fold_dfan$sensitivity <- c(0.56, 0.28, NaN)
cm_fold_dfan$specificity <- c(NaN, 0.73, 0.82)
rhandsontable(cm_fold_dfan, rowHeaders = NULL)

## For sequester set -

loan.s1 <- read.csv(file="S1.csv", header=TRUE, sep=",")

s1.dfan.fft <- fast.frugal.tree("fold3_train.csv", "S1.csv", "dfan", 'Sequester 1 - Algo: dfan')
s2.dfan.fft <- fast.frugal.tree("fold3_train.csv", "S2.csv", "dfan", 'Sequester 2 - Algo: dfan')
s3.dfan.fft <- fast.frugal.tree("fold3_train.csv", "S3.csv", "dfan", 'Sequester 3 - Algo: dfan')
s4.dfan.fft <- fast.frugal.tree("fold3_train.csv", "S4.csv", "dfan", 'Sequester 4 - Algo: dfan')

plot(s1.dfan.fft, data = "test")
plot(s2.dfan.fft, data = "test")
plot(s3.dfan.fft, data = "test")
plot(s4.dfan.fft, data = "test")


#confusion matrix for sequester data
seqs <- c('Seq1', 'Seq2', 'Seq3', 'Seq4')
tp_dfan <- c(90, 78, 82, 84)
fp_dfan <- c(67, 72, 67, 63)
tn_dfan <- c(33, 28, 33, 38)
fn_dfan <- c(10, 22, 18, 15)
cm_fold_dfan = tibble(FOLD= seqs, TP = tp_dfan, TN = tn_dfan, FP = fp_dfan, FN = fn_dfan)
cm_fold_dfan$accuracy <- round((cm_fold_dfan$TP + cm_fold_dfan$TN)/(cm_fold_dfan$TP + cm_fold_dfan$TN + cm_fold_dfan$FP + cm_fold_dfan$FN), digits = 2)
cm_fold_dfan$precision <- round(cm_fold_dfan$TP/(cm_fold_dfan$TP + cm_fold_dfan$FP), digits = 2)
cm_fold_dfan$recall <- round(cm_fold_dfan$TP/(cm_fold_dfan$TP + cm_fold_dfan$FN), digits = 2)
cm_fold_dfan$f <- round(2*cm_fold_dfan$recall*cm_fold_dfan$precision/(cm_fold_dfan$precision + cm_fold_dfan$recall), digits = 2)
cm_fold_dfan$sensitivity <- c(0.33, 0.28, 0.33, 0.38)
cm_fold_dfan$specificity <- c(0.9, 0.78, 0.82, 0.85)
rhandsontable(cm_fold_dfan, rowHeaders = NULL)



## Sequester
seq_accuracy = c(0.62, 0.53, 0.57, 0.61)
seq_precision = c(0.57, 0.52, 0.55, 0.57)
seq_recall = c(0.9, 0.78, 0.82, 0.85)
seq_f = c(0.7, 0.62, 0.66, 0.68)
seq_sensitivity = c(0.33, 0.28, 0.33, 0.38)
seq_specificity = c(0.9, 0.78, 0.82, 0.85)


data.accuracy <- bind_rows(tibble(Accuracy = 'Accuracy',Range = seq_accuracy),
                           tibble(Accuracy = 'Precision',Range = seq_precision),
                           tibble(Accuracy = 'Recall',Range = seq_recall),
                           tibble(Accuracy = 'F',Range = seq_f),
                           tibble(Accuracy = 'Sensitivity',Range = seq_sensitivity),
                           tibble(Accuracy = 'Specificity',Range = seq_specificity))


ggplot(data.accuracy,aes(x=Accuracy,y=Range))+
  geom_boxplot(fill='orange')
ggplot(data.precision,aes(x=Precision,y=Range))+
  geom_boxplot(fill='orange')
ggplot(data.recall,aes(x=Recall,y=Range))+
  geom_boxplot(fill='orange')
ggplot(data.f,aes(x=F,y=Range))+
  geom_boxplot(fill='orange')
ggplot(data.sensitivity,aes(x=Sensitivity,y=Range))+
  geom_boxplot(fill='orange')
ggplot(data.specificity,aes(x=Specificity,y=Range))+
  geom_boxplot(fill='orange')
