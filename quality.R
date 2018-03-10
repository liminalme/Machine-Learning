quality= read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
baseline = 98/131
baseline

install.packages("caTools")
library(caTools)

set.seed(88)
split= sample.split(quality$PoorCare, SplitRatio = 0.75)
train= subset(quality,split==TRUE)
nrow(train)
test= subset(quality, split==FALSE)
nrow(test)

qualitylog= glm(PoorCare~ OfficeVisits+ Narcotics, data=train, family=binomial())
summary(qualitylog)

predictTrain= predict(qualitylog, type="response")
predictTrain
summary(predictTrain)

tapply(predictTrain, train$PoorCare, mean)

table(train$PoorCare,predictTrain>0.5)
sen= 10/25
sen
spe=70/74
spe

table(train$PoorCare, predictTrain >0.7)
sen=8/(8+17)
sen
spe= 73/74
spe

table(train$PoorCare, predictTrain >0.2)
sen=16/(9+16)
sen
spe= 54/74
spe

install.packages("ROCR")
library(ROCR)
ROCRpred= prediction(predictTrain, train$PoorCare)
ROCRperf=performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
predictTest= predict(qualitylog, type="response",test)
predictTest

table(test$PoorCare,predictTest >=0.3)
accuracy=(19+6)/32
accuracy


