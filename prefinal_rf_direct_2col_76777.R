library(readr)
library(dplyr)
library(randomForest)
library(caret)


F1 <- function(data, lev = NULL, model = NULL) {
  uni<-unique(data$obs)
  k<-length(uni)
  
  b<-0
  for  (i in 1:k)
  {
    a<-MLmetrics::F1_Score (data$obs, (data$pred), positive=uni[i])
    if (is.na(a)) a<-0
    b<-b+a
  }
  c(F1=b/i)
  
}

set.seed(123)
ctrl <- trainControl(method = "cv", summaryFunction = F1,#mnLogLoss,
                     number = 10)

setwd("E:/R/_cp2022/altay")
train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")

train<-train%>%select(Код_группы, Основания, Статус)
train$Основания<-as.numeric(as.factor(train$Основания))
train$Статус<-(as.factor(train$Статус))

set.seed(123)
mod<-train(Статус~., data=train, method="xgbTree",  metric="F1",verbosity=0,
           tuneGrid = expand.grid(nrounds=c(1000,1500), 
                                  max_depth=c(4,5), 
                                  eta=0.4, 
                                  gamma=0, 
                                  colsample_bytree=0.8, 
                                  min_child_weight=1, 
                                  subsample=1),
           trControl = ctrl)
mod
max(mod$results$F1)



test$Основания[test$Основания=="ЛН"]<-"ОО"
test$Основания<-as.numeric(as.factor(test$Основания))

pred<-predict(mod,test)
sample<-read_csv("sample_submission.csv")
sample$Статус<-pred
table(sample$Статус)

write_csv(sample, "rf_xgb_tune2_direct_2c_numnum_1609.csv")
