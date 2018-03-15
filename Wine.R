
# Read the data
wine= read.csv("wine.csv")
#Structure of wine data
str(wine)
#Summary of wine date
summary(wine)

# Model1 using Avg growing season temp as independent variable
model1= lm(Price ~ AGST, wine)
summary(model1)
model1$residuals
# Sum of squared error for Model1 which was 5.74
SSE= sum(model1$residuals^2)
SSE

#Model2 using Avg growing season temp and Harvest Rain as 
#independent variable
model2= lm(Price ~ AGST+ HarvestRain, wine)
summary(model2)
#Sum of squared error for Model2 which was 2.98
SSE= sum(model2$residuals^2)
SSE

#Model3 using all independent variables
model3= lm(Price ~ AGST+HarvestRain + WinterRain+ Age+FrancePop, wine)
summary(model3)
#Sum of squared error for Model3 which was 1.73
SSE= sum(model3$residuals^2)
SSE

# From Model3 summary we determine that Age and France pop do not significantly 
#contribute. So for Model4 we remove France pop from the independent variables
model4 =lm(Price ~AGST +HarvestRain+WinterRain+Age, wine)
summary(model4)
#Sum of squared error for Model4 which was 1.74
SSE= sum(model4$residuals^2)
SSE
#We see that in Model4, age shows to be significant as opposed to Model3. This is due to
#multicollinearity. Age and France pop are highly correlated.
cor(wine$Age, wine$FrancePop)
cor(wine)

#If we try to remove Age and France pop at the same time and build model5 we determine that our
#SSE increased as compared to model4 and we would have lost a significant variable Age
model5 =lm(Price ~AGST +HarvestRain+WinterRain, wine)
summary(model5)
SSE= sum(model5$residuals^2)
SSE

#We will be using Model4 for predictions since our R^2 for Model4 was 0.83

#Read test data
wineTest= read.csv("wine_test.csv")
#Structure of test data
str(wineTest)
#Predict price for wines in test data using Model4
predictTest= predict(model4, newdata = wineTest)
predictTest
# Calculate sum of squared errors for test set
SSE= sum((wineTest$Price-predictTest)^2)
SSE
#Calculate sum of squared errors for baseline model
SST=sum((wineTest$Price-mean(wine$Price))^2)
SST
#Calculate the R^2 for the test set or out of sample accuracy of our model
1-SSE/SST

#RESULTS
#Wine Experts claimed 1986 wine will be very good or exceptional
#Models predicted 1986 will be mediocre and 1989 will be "wine of the century"
#In wine auctions, 1989 sold for more than twice the price of 1986 
#A linear regression model with only few variables can predict wine prices well and 
#in many cases outperforms wine experts