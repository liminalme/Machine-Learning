stevens=read.csv("stevens.csv")
str(stevens)
library("caTools")
set.seed(3000)
split=sample.split(stevens$Reverse, SplitRatio = 0.7)
train= subset(stevens, split==TRUE)
train
test= subset(stevens, split==FALSE)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
stevensTree= rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,method = "class",control = rpart.control(minbucket = 25))
prp(stevensTree)
predictCART=predict(stevensTree, test,type="class")
table(test$Reverse, predictCART)
accuracy= (41+71)/(41+36+22+71)
accuracy

table(stevens$Reverse)
baseline=309/(257+309)
baseline

library(ROCR)
predictROC=predict(stevensTree,test)
predictROC
pred=prediction(predictROC[,2],test$Reverse)
perf=performance(pred,"tpr","fpr")
plot(perf)


-----------------------------------------------------------
#Random Forests
  
install.packages("randomForest")
library(randomForest)
StevensForest= randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,nodesize=25, ntree=200)
train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)
StevensForest= randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,nodesize=25, ntree=200)
table(test$Reverse,PredictForest)
accuracy=(40+74)/(40+37+19+74)
accuracy
