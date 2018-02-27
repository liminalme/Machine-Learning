wine= read.csv("wine.csv")
str(wine)
summary(wine)
model1= lm(Price ~ AGST, wine)
summary(model1)
model1$residuals
SSE= sum(model1$residuals^2)
SSE
model2= lm(Price ~ AGST+ HarvestRain, wine)
summary(model2)
SSE= sum(model2$residuals^2)
SSE
model3= lm(Price ~ AGST+HarvestRain + WinterRain+ Age+FrancePop, wine)
summary(model3)
SSE= sum(model3$residuals^2)
SSE
model4 =lm(Price ~AGST +HarvestRain+WinterRain+Age, wine)
summary(model4)

cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

model5 =lm(Price ~AGST +HarvestRain+WinterRain, wine)
summary(model5)


wineTest= read.csv("wine_test.csv")
str(wineTest)
predictTest= predict(model4, newdata = wineTest)
predictTest
SSE= sum((wineTest$Price-predictTest)^2)
SST=sum((wineTest$Price-mean(wine$Price))^2)
SSE
SST
1-SSE/SST