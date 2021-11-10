#---
#title: "knn_model"
#author: "Nitya Rajendran"
#---

library(class)
library(gmodels)
suppressPackageStartupMessages(library(tidyverse))


dataOnlineShopper <- read_csv("osd_ov_allOHE.csv")

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
  select(Administrative, Administrative_Duration, Informational, Informational_Duration, ProductRelated, ProductRelated_Duration, BounceRates, ExitRates, PageValues, SpecialDay, MonthDec, MonthFeb, MonthJul, MonthJune, MonthMar, MonthMay, MonthNov, MonthOct, MonthSep, OperatingSystems2, OperatingSystems3, OperatingSystems4, OperatingSystems6, OperatingSystems7, OperatingSystems8, Browser2, Browser3, Browser4, Browser5, Browser6, Browser7, Browser8, Browser10, Browser12, Browser13, Region2, Region3, Region4, Region5, Region6, Region7, Region8, Region9, TrafficType2, TrafficType3, TrafficType4, TrafficType5, TrafficType6, TrafficType7, TrafficType8, TrafficType9, TrafficType10, TrafficType11, TrafficType13, TrafficType15, TrafficType16, TrafficType20, VisitorTypeOther, VisitorTypeReturning_Visitor, WeekendTRUE, Revenue)

test_sub <- test %>% 
  select(Administrative, Administrative_Duration, Informational, Informational_Duration, ProductRelated, ProductRelated_Duration, BounceRates, ExitRates, PageValues, SpecialDay, MonthDec, MonthFeb, MonthJul, MonthJune, MonthMar, MonthMay, MonthNov, MonthOct, MonthSep, OperatingSystems2, OperatingSystems3, OperatingSystems4, OperatingSystems6, OperatingSystems7, OperatingSystems8, Browser2, Browser3, Browser4, Browser5, Browser6, Browser7, Browser8, Browser10, Browser12, Browser13, Region2, Region3, Region4, Region5, Region6, Region7, Region8, Region9, TrafficType2, TrafficType3, TrafficType4, TrafficType5, TrafficType6, TrafficType7, TrafficType8, TrafficType9, TrafficType10, TrafficType11, TrafficType13, TrafficType15, TrafficType16, TrafficType20, VisitorTypeOther, VisitorTypeReturning_Visitor, WeekendTRUE, Revenue)


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