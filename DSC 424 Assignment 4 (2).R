### PROBLEM 2

# Installing Packages
library(corrplot)
library(psych)
library(lavaan)
library(ggplot2)
library(vcd)
library(ca)
library(factoextra)

# Reading the StoresandAges File
SAA <- read.csv("StoresAndAges.csv")
head(SAA)
rownames(SAA) <- SAA[,1]
SAA_new <- SAA[,-1]
SAA_new

# Creating the Mosaic Plot
mosaic(as.matrix(SAA_new), shade = T, highlighting_direction = "right") 

# Plotting the Corresponding Analysis
c = ca(SAA_new)
c$N
c$rowcoord
plot(c)
eigenvals = get_eigenvalue(c)
eigenvals

# This plot puts arrows to the letters so that we can compare
# Their relative frequencies to the texts
plot(c, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))


### PROBLEM 3

# Installing Packages
library(MASS)

# Reading the BondRating File and Splitting it into Training and Testing Set
BRV <- read.csv("BondRating_ValidationSet.csv")
BRV
BRT <- read.csv("BondRating_TrainingSet.csv")
head(BRT)

# Performance of classifier on Training set
fit = lda(RATING ~ ., data=BRT[,-c(1,3)])
print(fit)
pred1 = predict(fit, BRV[,-c(1,3)])
pred1
BRV$RATING
print(fit$scaling[order(fit$scaling[, 1]), ])
fit.values = predict(fit, BRT[,-c(1,3)])

# Creating the BoxPlots for the Training Set
par(mar=c(1, 1, 1, 1))
ldahist(data=fit.values$x[, 1], g=BRT$RATING)
ldahist(data=fit.values$x[, 2], g=BRT$RATING)
ldahist(data=fit.values$x[, 3], g=BRT$RATING)
ldahist(data=fit.values$x[, 4], g=BRT$RATING)
ldahist(data=fit.values$x[, 5], g=BRT$RATING)
ldahist(data=fit.values$x[, 6], g=BRT$RATING)

# Creating the ScatterPlot for Training Set
BRT$RATING = as.factor(BRT$RATING)
plot(fit.values$x[, 1],
     fit.values$x[, 2], 
     col=BRT$RATING,
     pch=16)
table(BRT$RATING, fit.values$class)

# Confusion Matrix
source("Confusion.R")
confusion(fit.values$class, BRT$RATING)

# Creating the BoxPlots for the Validation Set
fit.values = predict(fit, BRV[,-c(1,3)])
par(mar=c(1, 1, 1, 1))
ldahist(data=fit.values$x[, 1], g=BRV$RATING)
ldahist(data=fit.values$x[, 2], g=BRV$RATING)
ldahist(data=fit.values$x[, 3], g=BRV$RATING)
ldahist(data=fit.values$x[, 4], g=BRV$RATING)
ldahist(data=fit.values$x[, 5], g=BRV$RATING)
ldahist(data=fit.values$x[, 6], g=BRV$RATING)

# Creating the ScatterPlot for the Testing Set
BRV$RATING = as.factor(BRV$RATING)
plot(fit.values$x[, 3], 
     fit.values$x[, 4],
     col=BRV$RATING,
     pch=16)
table(BRV$RATING, fit.values$class)

# Confusion Matrix
source("Confusion.R")
confusion(fit.values$class, BRV$RATING) 

