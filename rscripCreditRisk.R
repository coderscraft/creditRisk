library(FFTrees)
library(tidyverse)
library(rhandsontable)
require(gridExtra)
setwd('/Users/ravirane/Desktop/GMU/CS584/myWork/assignment1/data/fold')
# Function to create Fast and Frugal Tree for given data and algo
fast.frugal.tree <- function(trainFile, testFile, algo, info) {
  print(info)
  adult.train <- read.csv(file=trainFile, header=TRUE, sep=",")
  adult.test <- read.csv(file=testFile, header=TRUE, sep=",")
  adult.fft <- FFTrees(formula = class ~ .,
                       data = adult.train,
                       data.test = adult.test,
                       algorithm = algo,
                       main = "Adult data",
                       do.comp = FALSE,
                       decision.labels = c("<=50", ">50"))
  print(adult.fft)
  adult.fft
}
# Creating model on fold
fold1.ifan.fft <- fast.frugal.tree("fold1_train.csv", "fold1_test.csv", "ifan", 'Fold 1 FFT - Algo: ifan')
fold2.ifan.fft <- fast.frugal.tree("fold2_train.csv", "fold2_test.csv", "ifan", 'Fold 2 FFT - Algo: ifan')
fold3.ifan.fft <- fast.frugal.tree("fold3_train.csv", "fold3_test.csv", "ifan", 'Fold 3 FFT - Algo: ifan')
fold4.ifan.fft <- fast.frugal.tree("fold4_train.csv", "fold4_test.csv", "ifan", 'Fold 4 FFT - Algo: ifan')
fold5.ifan.fft <- fast.frugal.tree("fold5_train.csv", "fold5_test.csv", "ifan", 'Fold 5 FFT - Algo: ifan')
fold1.dfan.fft <- fast.frugal.tree("fold1_train.csv", "fold1_test.csv", "dfan", 'Fold 1 FFT - Algo: dfan')
fold2.dfan.fft <- fast.frugal.tree("fold2_train.csv", "fold2_test.csv", "dfan", 'Fold 2 FFT - Algo: dfan')
fold3.dfan.fft <- fast.frugal.tree("fold3_train.csv", "fold3_test.csv", "dfan", 'Fold 3 FFT - Algo: dfan')
fold4.dfan.fft <- fast.frugal.tree("fold4_train.csv", "fold4_test.csv", "dfan", 'Fold 4 FFT - Algo: dfan')
fold5.dfan.fft <- fast.frugal.tree("fold5_train.csv", "fold5_test.csv", "dfan", 'Fold 5 FFT - Algo: dfan')
# Plotting fold model tree
plot(fold1.ifan.fft, data = "test")
plot(fold2.ifan.fft, data = "test")
plot(fold3.ifan.fft, data = "test")
plot(fold4.ifan.fft, data = "test")
plot(fold5.ifan.fft, data = "test")
plot(fold1.dfan.fft, data = "test")
plot(fold2.dfan.fft, data = "test")
plot(fold3.dfan.fft, data = "test")
plot(fold4.dfan.fft, data = "test")
plot(fold5.dfan.fft, data = "test")

folds <- c('Fold1', 'Fold2', 'Fold3', 'Fold4', 'Fold5' )
#confusion matrix for fold_ifan
tp_ifan <- c(49, 53, 46, 48,55 )
fp_ifan <- c(12, 14, 20, 21, 17)
tn_ifan <- c(46, 46, 44, 41, 43)
fn_ifan <- c(13, 7, 10, 10, 5)

cm_fold_ifan = tibble(FOLD= folds, TP = tp_ifan, TN = tn_ifan, FP = fp_ifan, FN = fn_ifan)
cm_fold_ifan$accuracy <- round((cm_fold_ifan$TP + cm_fold_ifan$TN)/(cm_fold_ifan$TP + cm_fold_ifan$TN + cm_fold_ifan$FP + cm_fold_ifan$FN), digits = 2)
cm_fold_ifan$precision <- round(cm_fold_ifan$TP/(cm_fold_ifan$TP + cm_fold_ifan$FP), digits = 2)
cm_fold_ifan$recall <- round(cm_fold_ifan$TP/(cm_fold_ifan$TP + cm_fold_ifan$FN), digits = 2)
cm_fold_ifan$f <- round(2*cm_fold_ifan$recall*cm_fold_ifan$precision/(cm_fold_ifan$precision + cm_fold_ifan$recall), digits = 2)
cm_fold_ifan$sensitivity <- c(0.79, 0.88, 0.82, 0.83, 0.92 )
cm_fold_ifan$specificity <- c(0.79, 0.77, 0.69, 0.66, 0.72)
rhandsontable(cm_fold_ifan, rowHeaders = NULL)


#confusion matrix for fold_dfan
tp_dfan <- c(49, 54, 50, 48,55)
fp_dfan <- c(12, 14, 21, 21, 17)
tn_dfan <- c(46, 46, 43, 41, 43)
fn_dfan <- c(13, 6, 6, 10, 5)
cm_fold_dfan = tibble(FOLD= folds, TP = tp_dfan, TN = tn_dfan, FP = fp_dfan, FN = fn_dfan)
cm_fold_dfan$accuracy <- round((cm_fold_dfan$TP + cm_fold_dfan$TN)/(cm_fold_dfan$TP + cm_fold_dfan$TN + cm_fold_dfan$FP + cm_fold_dfan$FN), digits = 2)
cm_fold_dfan$precision <- round(cm_fold_dfan$TP/(cm_fold_dfan$TP + cm_fold_dfan$FP), digits = 2)
cm_fold_dfan$recall <- round(cm_fold_dfan$TP/(cm_fold_dfan$TP + cm_fold_dfan$FN), digits = 2)
cm_fold_dfan$f <- round(2*cm_fold_dfan$recall*cm_fold_dfan$precision/(cm_fold_dfan$precision + cm_fold_dfan$recall), digits = 2)
cm_fold_dfan$sensitivity <- c(0.79, 0.90, 0.89, 0.83, 0.92)
cm_fold_dfan$specificity <- c(0.79, 0.77, 0.67, 0.66, 0.72)
rhandsontable(cm_fold_dfan, rowHeaders = NULL)


## Gini
gini_fold_accuracy = c(0.8, 0.85, 0.78, 0.78, 0.82)
gini_fold_precision = c(0.81, 0.84, 0.73, 0.75, 0.81)
gini_fold_recall = c(0.81, 0.87, 0.86, 0.81, 0.83)
gini_fold_f = c(0.81, 0.85, 0.79, 0.78, 0.82)
gini_fold_sensitivity = c(0.81, 0.87, 0.86, 0.81, 0.83)
gini_fold_specificity = c(0.79, 0.83, 0.72, 0.74, 0.8)

## Entropy
entropy_fold_accuracy = c(0.8, 0.87, 0.78, 0.78, 0.81)
entropy_fold_precision = c(0.81, 0.84, 0.73, 0.75, 0.8)
entropy_fold_recall = c(0.81, 0.9, 0.86, 0.81, 0.82)
entropy_fold_f = c(0.81, 0.87, 0.79, 0.78, 0.81)
entropy_fold_sensitivity = c(0.81, 0.9, 0.86, 0.81, 0.82)
entropy_fold_specificity = c(0.79, 0.83, 0.72, 0.74, 0.8)

## Info gain

infog_fold_accuracy = c(0.62, 0.83, 0.86, 0.80, 0.81)
infog_fold_precision = c(0.78, 0.85, 0.86, 1, 1)
infog_fold_recall = c(0.61, 0.83, 0.86, 0.80, 0.81)
infog_fold_f = c(0.55, 0.84, 0.86, 0.89, 0.89)
infog_fold_sensitivity = c(1, 0.25, 0.33, 0, 0)
infog_fold_specificity = c(0.56, 0.92, 0.92, 1, 1)

## Gain Ratio

gainr_fold_accuracy = c(0.61, 0.83, 0.86, 0.80, 0.82)
gainr_fold_precision = c(0.78, 0.85, 0.86, 0.80, 0.82)
gainr_fold_recall = c(0.61, 0.83, 0.86, 0.89, 0.90)
gainr_fold_f = c(0.55, 0.84, 0.86, 0.89, 0.90)
gainr_fold_sensitivity = c(1, 0.25, 0.33, 0, 0)
gainr_fold_specificity = c(0.56, 0.92, 0.92, 1, 1)

## Unpruned

unpruned_fold_accuracy = c(0.65, 0.75, 0.85, 0.81, 0.86)
unpruned_fold_precision = c(0.77, 0.87, 0.89, 1, 1)
unpruned_fold_recall = c(0.65, 0.75, 0.85, 0.81, 0.86)
unpruned_fold_f = c(0.60, 0.8, 0.87, 0.89, 0.92)
unpruned_fold_sensitivity = c(0.95, 0.22, 0.36, 0, 0)
unpruned_fold_specificity = c(0.59, 0.94, 0.95, 1, 1)

## Pruned

pruned_fold_accuracy = c(0.6, 0.85, 0.86, 0.80, 0.91)
pruned_fold_precision = c(0.78, 0.86, 0.86, 1, 1)
pruned_fold_recall = c(0.6, 0.85, 0.86, 0.88, 0.91)
pruned_fold_f = c(0.34, 0.85, 0.86, 0.88, 0.95)
pruned_fold_sensitivity = c(1, 0.28, 0.33, 0, 0)
pruned_fold_specificity = c(0.56, 0.92, 0.92, 1, 1)

data.accuracy <- bind_rows(tibble(Accuracy = 'gini-Accuracy',Range = gini_fold_accuracy),
                           tibble(Accuracy = 'entropy-Accuracy',Range = entropy_fold_accuracy),
                           tibble(Accuracy = 'infogain-Accuracy',Range = infog_fold_accuracy),
                           tibble(Accuracy = 'gainr-Accuracy',Range = gainr_fold_accuracy),
                           tibble(Accuracy = 'pruned-Accuracy',Range = pruned_fold_accuracy),
                           tibble(Accuracy = 'unpruned-Accuracy',Range = unpruned_fold_accuracy),
                           tibble(Accuracy = 'ffifan-Accuracy',Range = cm_fold_ifan$accuracy),
                           tibble(Accuracy = 'dfan-Accuracy',Range = cm_fold_dfan$accuracy))
data.precision <- bind_rows(tibble(Precision = 'gini-Precision',Range = gini_fold_precision),
                            tibble(Precision = 'entropy-Precision',Range = entropy_fold_precision),
                            tibble(Precision = 'infogain-Precision',Range = infog_fold_precision),
                            tibble(Precision = 'gainr-Precision',Range = gainr_fold_precision),
                            tibble(Precision = 'pruned-Precision',Range = pruned_fold_precision),
                            tibble(Precision = 'unpruned-Precision',Range = unpruned_fold_precision),
                            tibble(Precision = 'ifan-Precision',Range = cm_fold_ifan$precision),
                            tibble(Precision = 'dfan-Precision',Range = cm_fold_dfan$precision))
data.recall <- bind_rows(tibble(Recall = 'gini-Recall',Range = gini_fold_recall),
                         tibble(Recall = 'entropy-Recall',Range = entropy_fold_recall),
                         tibble(Recall = 'infogain-Recall',Range = infog_fold_recall),
                         tibble(Recall = 'gainr-Recall',Range = gainr_fold_recall),
                         tibble(Recall = 'pruned-Recall',Range = pruned_fold_recall),
                         tibble(Recall = 'unpruned-Recall',Range = unpruned_fold_recall),
                         tibble(Recall = 'ifan-Recall',Range = cm_fold_ifan$recall),
                         tibble(Recall = 'dfan-Recall',Range = cm_fold_dfan$recall))
data.f <- bind_rows(tibble(F = 'gini-F',Range = gini_fold_f),
                    tibble(F = 'entropy-F',Range = entropy_fold_f),
                    tibble(F = 'infogain-F',Range = infog_fold_f),
                    tibble(F = 'gnainr-F',Range = gainr_fold_f),
                    tibble(F = 'pruned-F',Range = pruned_fold_f),
                    tibble(F = 'unpruned-F',Range = unpruned_fold_f),
                    tibble(F = 'ifan-F',Range = cm_fold_ifan$f),
                    tibble(F = 'dfan-F',Range = cm_fold_dfan$f))
data.sensitivity <- bind_rows(tibble(Sensitivity = 'gini-Sensitivity',Range = gini_fold_sensitivity),
                              tibble(Sensitivity = 'entropy-Sensitivity',Range = entropy_fold_sensitivity),
                              tibble(Sensitivity = 'infogain-Sensitivity',Range = infog_fold_sensitivity),
                              tibble(Sensitivity = 'gnainr-Sensitivity',Range = gainr_fold_sensitivity),
                              tibble(Sensitivity = 'pruned-Sensitivity',Range = pruned_fold_sensitivity),
                              tibble(Sensitivity = 'unpruned-Sensitivity',Range = unpruned_fold_sensitivity),
                              tibble(Sensitivity = 'ifan-Sensitivity',Range = cm_fold_ifan$sensitivity),
                              tibble(Sensitivity = 'dfan-Sensitivity',Range = cm_fold_dfan$sensitivity))
data.specificity <- bind_rows(tibble(Specificity = 'gini-Specificity',Range = gini_fold_specificity),
                              tibble(Specificity = 'entropy-Specificity',Range = entropy_fold_specificity),
                              tibble(Specificity = 'infogain-Specificity',Range = infog_fold_specificity),
                              tibble(Specificity = 'gainr-Specificity',Range = gainr_fold_specificity),
                              tibble(Specificity = 'pruned-Specificity',Range = pruned_fold_specificity),
                              tibble(Specificity = 'unpruned-Specificity',Range = unpruned_fold_specificity),
                              tibble(Specificity = 'dfan-ifan-Specificity',Range = cm_fold_ifan$specificity),
                              tibble(Specificity = 'dfan-Specificity',Range = cm_fold_dfan$specificity))



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
