#Read the data
stevens=read.csv("stevens.csv")
#Structure of stevens data
str(stevens)
#install and load caTools package
library("caTools")
#
set.seed(3000)
#Create the split
split=sample.split(stevens$Reverse, SplitRatio = 0.7)
#Split the data into training and test set
train= subset(stevens, split==TRUE)
train
#Split the data into training and test set
test= subset(stevens, split==FALSE)
#Install and load rpart package for CART
install.packages("rpart")
library(rpart)
#Install and load rpart plot for plotting CART
install.packages("rpart.plot")
library(rpart.plot)
# Create the CART model using rpart function
# Class tells rpart to build a classification tree since we have a binary outcome
#minbucket size limits the tree so it does not overfit to our training set
stevensTree= rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,method = "class",control = rpart.control(minbucket = 25))
#Plot the tree using a prp function
prp(stevensTree)
#Predicting cases in the test set using the CART model
#class will take each test set value and classify into 0 or 1
predictCART=predict(stevensTree, test,type="class")
#confusion matrix
table(test$Reverse, predictCART)
#accuracy of the model
accuracy= (41+71)/(41+36+22+71)
accuracy
#accuracy of baseline model
table(stevens$Reverse)
baseline=309/(257+309)
baseline
#Reciever operator characteristics
library(ROCR)
predictROC=predict(stevensTree,test)
predictROC
pred=prediction(predictROC[,2],test$Reverse)
perf=performance(pred,"tpr","fpr")
plot(perf)


-----------------------------------------------------------
#Random Forests- Designed to improve the prediction accuracy of CART
  
# install and load random forest package
install.packages("randomForest")
library(randomForest)
#Build the random forest model
#node size is same as min bucket
#ntree is no of trees to build
StevensForest= randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,nodesize=25, ntree=200)
# we receive a warning in the previous line of code since we did not specify the method=class
#Since random forest does not have method, we need to factor the outcome variable
train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)
#Build the model again
StevensForest= randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,nodesize=25, ntree=200)
#confusion matrix
table(test$Reverse,PredictForest)
#accuracy of the model
accuracy=(40+74)/(40+37+19+74)
accuracy


#RESULTS
#For the 68 cases in October 2002:
#Overall case predictions:
#--> Model accuracy: 75%
#--> Experts accuracy: 59%

# Individual justice predictions:
#--> Model Accuracy: 67%
#--> Experts Accuracy: 68%