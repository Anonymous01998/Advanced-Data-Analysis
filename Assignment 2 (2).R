# PROBLEM 1

df <- mtcars
head(df)
# Selecting dependent variable
Y <- df$mpg
Y
Z = subset(df, select=c("cyl", "disp", "hp", "wt", "carb"))
head(Z)
Z = cbind('count'=1, Z)
head(Z)
Z = as.matrix(Z)
head(Z)
# Calculating beta value
covariance = t(Z) %*% Z
covariance
inverse = solve(covariance)
deter = det(inverse)
x = inverse * deter
cal = x %*% t(Z)
cal2 = cal / deter
beta = cal2 %*% Y
beta
# Fitting the linear regression model
Z = as.data.frame(Z)
Y = as.matrix(Y)
fit = lm(Y~., data = Z)
summary(fit)

# PROBLEM 2

housingTrain = read.csv("housingTrain.csv")    
housingTest = read.csv("housingTest.csv")
head(housingTrain)
olsFit = lm(CRIM ~ ., data=housingTrain)
summary(olsFit)
rmseOlsTrain = sqrt(mean(olsFit$residuals^2))
rmseOlsTrain
library(glmnet)
library(ggplot2)
# Separating and x and y variables
xTrain = as.matrix(housingTrain[, -1])   
yTrain = as.matrix(housingTrain[, 1]) 
xTest = as.matrix(housingTest[, -1])   
yTest = as.matrix(housingTest[, 1]) 
# Fitting the Ridge Regression model
fitRidge = cv.glmnet(xTrain, yTrain, alpha=0, nfolds=10)
fitRidge$lambda.min   
fitRidge$lambda.1se
plot(fitRidge)
fitRidge
y_predicted <- predict(fitRidge, s = "lambda.1se", newx = xTrain)
sst <- sum((yTrain - mean(yTrain))^2)
sse <- sum((y_predicted - yTrain)^2)
rsq <- 1 - sse / sst    # Calculating R-square
rsq
ridgePred = predict(fitRidge, xTest, s="lambda.1se")
rmseRidge = sqrt(mean((ridgePred - yTest)^2))
rmseRidge
mseRidge.min <- fitRidge$cvm[fitRidge$lambda == fitRidge$lambda.1se]
diffRmse = abs(mseRidge.min - LassoRmse)
diffRmsey_predicted <- predict(fitLasso, s = "lambda.1se", newx = xTrain)
sst <- sum((yTrain - mean(yTrain))^2)
sse <- sum((y_predicted - yTrain)^2)
rsq <- 1 - sse / sst    # Calculating R-square
rsq
LassoPred = predict(fitLasso, xTest, s="lambda.1se")
LassoRmse = sqrt(mean((LassoPred - yTest)^2))
LassoRmse
diffRmseLasoo = abs(mse.min - LassoRmse)
diffRmseLasoo
# ------------------ Lasso Regression ----------------------
fitLasso = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=10)
fitLasso$lambda.min   
fitLasso$lambda.1se
plot(fitLasso)
fitLasso
mse.min <- fitLasso$cvm[fitLasso$lambda == fitLasso$lambda.1se]


# PROBLEM 3
library(ggplot2)
library(glmnet)
insurTrain = read.csv("insurTest.csv")
insurTest = read.csv("insurTest.csv")
head(insurTrain)
head(insurTest)
XTrain <- subset(insurTrain, select=c("pctmin","fires","thefts","pctold","income"))
XTest <- subset(insurTest, select=c("pctmin","fires","thefts","pctold","income"))
XTrain
YTrain <- insurTrain$newpol
YTest <- insurTest$newpol
# Fitting a full multiple regression model
fit1 <- lm(YTrain ~., data=XTrain)
summary(fit1)
rmseTrain = sqrt(mean(fit1$residuals^2))
rmseTrain
# Fitting elastic net regression model with alpha = 0
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0, nfolds=7)
fitElastic$lambda.min
fitElastic$lambda.1se
plot(fitElastic)
fitElastic 
# Determining RMSE for testing set for alpha = 0
elasticPred = predict(fitElastic, xTest, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTest)^2))
rmseElastic
# Determining RMSE for training set alpha = 0
elasticPred = predict(fitElastic, xTrain, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTrain)^2))
rmseElastic
# Determining R-squared for training set alpha = 0
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0, nfolds=7)
fitElastic
# Fitting elastic net regression model with alpha = 0.25
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.25, nfolds=7)
fitElastic$lambda.min
fitElastic$lambda.1se
plot(fitElastic)
fitElastic 
# Determining RMSE for testing set alpha = 0.25
elasticPred = predict(fitElastic, xTest, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTest)^2))
rmseElastic
# Determining RMSE for training set alpha = 0.25
elasticPred = predict(fitElastic, xTrain, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTrain)^2))
rmseElastic
# Determining R-squared for training set alpha = 0.25
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.25, nfolds=7)
fitElastic
# Fitting elastic net regression model with alpha = 0.5
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.5, nfolds=7)
fitElastic$lambda.min
fitElastic$lambda.1se
plot(fitElastic)
fitElastic 
# Determining the RMSE for Testing set alpha = 0.5
elasticPred = predict(fitElastic, xTest, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTest)^2))
rmseElastic
# Determining the RMSE for Testing set alpha = 0.5
elasticPred = predict(fitElastic, xTrain, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTrain)^2))
rmseElastic
# Determining R-squared for training set alpha = 0.5
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.5, nfolds=7)
fitElastic
# Fitting elastic net regression model with alpha = 0.75
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.75, nfolds=7)
fitElastic$lambda.min
fitElastic$lambda.1se
plot(fitElastic)
fitElastic 
# Determining the RMSE for testing set alpha = 0.75
elasticPred = predict(fitElastic, xTest, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTest)^2))
rmseElastic
# Determining the RMSE for training set alpha = 0.75
elasticPred = predict(fitElastic, xTrain, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTrain)^2))
rmseElastic
# Determining the R-squared for training set alpha = 0.75
fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.75, nfolds=7)
fitElastic
# Fitting elastic net regression model with alpha = 1
fitElastic = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=7)
fitElastic$lambda.min
fitElastic$lambda.1se
plot(fitElastic)
fitElastic 
# Determining the RMSE for testing set alpha = 1
elasticPred = predict(fitElastic, xTest, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTest)^2))
rmseElastic
# Determining the RMSE for training set alpha = 1
elasticPred = predict(fitElastic, xTrain, s="lambda.1se")
rmseElastic = sqrt(mean((elasticPred - yTrain)^2))
rmseElastic
# Determining the R-squared for training set alpha = 1
fitElastic = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=7)
fitElastic

