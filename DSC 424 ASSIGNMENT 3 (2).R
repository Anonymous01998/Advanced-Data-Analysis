### PROBLEM 1 

# Installing Packages
install.packages("QuantPsyc")
install.packages("leaps")
install.packages("foreign")
install.packages("psych")
library(car)
library(corrplot)
library(ggplot2)
library(dplyr)
library(QuantPsyc)
library(leaps)
library(foreign)
library(psych)

# Importing the text file and reading it
Emp <- read.delim("Employment.txt")
head(Emp)
p = prcomp(Emp[,-1])  
print(p)      
summary(p)
p$x

# Analyzing Correlation Plot
cor(Emp[,-1])
corrplot(cor(Emp[,-1]), method="ellipse", order="AOE")
Corrtest <- corr.test(Emp[,-1], adjust = "none")
Corrtest
C <- Corrtest$p
Ctest <- ifelse(C < 0.1, T,F)
colSums(Ctest)-1

# Removing Agr, Fin and SPS
Emp_reduced <- Emp[,-c(2,8,9)]
head(Emp_reduced)
p_new = prcomp(Emp_reduced[,-1])  
print(p_new)      
summary(p_new)

### PROBLEM 2

# Installing Packages
library(psych)

# Importing the csv file and reading it
census <- read.csv("Census2.csv")
head(census)

# PCA
p = prcomp(census)  
print(p)      
summary(p)
plot(p)
p$x

# Changing median home value
census2 <- census
census2$MedianHomeVal <- census$MedianHomeVal/100000
head(census2)
p = prcomp(census2)  
print(p)      
summary(p)
plot(p)
p$x

# PCA with correlation matrix
p2 = principal(census2, nfactors = 2, rotate = "none")
print(p2)


### PROBLEM 3

# Importing the csv file and reading it
tests <- read.csv("wiscsem.csv")
head(tests)
p <- prcomp(tests)
print(p)      
summary(p)
plot(p)
p$x
corrplot(cor(tests))
Corrtest <- corr.test(tests, adjust = "none")
Corrtest
C <- Corrtest$p
Ctest <- ifelse(C < 0.01, T,F)
colSums(Ctest)-1

# Excluding client,agemate,codind and simil 
tests2 <- tests[,-c(1,2,6,13)]
head(tests2)
p2 <- prcomp(tests2)
print(p2)      
summary(p2)

# Computing Principal Factor Analysis
p3 = principal(tests2, rotate="varimax", nfactors=5)
print(p3$loadings, cutoff=.4, sort=T)

# Computing Common Factor Analysis
fit = factanal(tests2, 5)
print(fit$loadings, cutoff=.4, sort=T)
print(fit)            
fit$correlation
