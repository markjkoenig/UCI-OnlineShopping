#---
#title: "knn_model"
#author: "Nitya Rajendran"
#---

library(class)
library(gmodels)
suppressPackageStartupMessages(library(tidyverse))

dataOnlineShopper <- read_csv("osd_reg_allOHE.csv")

attach(dataOnlineShopper)
str(dataOnlineShopper)
table(dataOnlineShopper$Revenue)
dataOnlineShopper <- as.data.frame(dataOnlineShopper)

dim(dataOnlineShopper)

set.seed(1)

dataOnlineShopper <- dataOnlineShopper[sample(1:nrow(dataOnlineShopper),length(1:nrow(dataOnlineShopper))),1:ncol(dataOnlineShopper)]

n <- floor(0.90 * nrow(dataOnlineShopper))
in_rows <- sample(c(1:nrow(dataOnlineShopper)), size = n, replace = FALSE)
train <- dataOnlineShopper[in_rows,]
test <- dataOnlineShopper[-in_rows,]


train_sub <- train %>% 
  select(Administrative, Administrative_Duration, Informational, Informational_Duration, ProductRelated, ProductRelated_Duration, BounceRates, ExitRates, PageValues, MonthDec, MonthMar, MonthMay, MonthNov, MonthOct, OperatingSystems2, OperatingSystems3, TrafficType13, VisitorTypeReturning_Visitor, Revenue)

test_sub <- test %>% 
  select(Administrative, Administrative_Duration, Informational, Informational_Duration, ProductRelated, ProductRelated_Duration, BounceRates, ExitRates, PageValues, MonthDec, MonthMar, MonthMay, MonthNov, MonthOct, OperatingSystems2, OperatingSystems3, TrafficType13, VisitorTypeReturning_Visitor, Revenue)


# separate features and labels
train_labels <- train_sub['Revenue']
test_labels <- test_sub['Revenue']
head(train_sub)
train_x <- train_sub[ , -which(names(train_sub) %in% c("Revenue"))]
test_x <- test_sub[ , -which(names(test_sub) %in% c("Revenue"))]

# define metrics
get_rates <- function(res){
  tp = res[2,2]
  tn = res[1,1]
  fn = res[1,2]
  fp = res[2,1]
  print(sprintf('Accuracy: %.3f', (tp+tn)/(tp+fp+tn+fn)))
  print(sprintf('TPR: %.3f', (tp)/(tp+fn)))
  print(sprintf('TNR: %.3f', (tn)/(fp+tn)))
  print(sprintf('FPR: %.3f', (fp)/(fp+tn)))
  print(sprintf('FNR: %.3f', (fn)/(tp+fn)))
  print(sprintf('Precision: %.3f', (tp)/(tp+fp)))
  print(sprintf('Recall: %.3f', (tp)/(tp+fn)))
  print(sprintf('F1: %.3f', (tp)/(tp+(0.5*(fp+fn)))))
}

# metrics per k-value

k_vals <- c(1, 5, 10, 15, 25, 30, 50, 75, 100)
for (k in k_vals) {
  print(k)
  knn.pred = knn(train_x, test_x, train_labels[,1], k = k)
  res = table(Predictions = knn.pred, TrueLabels = test_labels[,1])
  get_rates(res)
}