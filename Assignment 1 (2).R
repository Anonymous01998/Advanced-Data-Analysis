#PROBLEM 1

# dot product of two vectors

M = c(8,2,2,3)
M
prod = M %% M
prod
var(M)

#PROBLEM 2

# Creating a data frame

stock_portfolios <- data.frame( int = c(4,2),
                                amd = c(3,7))
data
M = as.matrix(data)
cov(M)
trans = t(M)
prod = M %*% trans
prod

#PROBLEM 4

# Reads the specified csv file and opens it
housing = read.csv("housing.csv")      
# Gives the structure of the data including data-types
str(housing)
# For each column a summary statistics have been provided
summary(housing)

# Fits the entire model under lm() function
fullFit = lm(CRIM ~ . - ZN - INDUS - CHAS - NOX - RM - AGE - DIS - RAD - TAX - PTRATIO - LSTAT - MEDV, data = housing)
summary(fullFit)

# The Forward Selection Technique is used first 
housingForward = step(null, scope = list(lower=null, upper = full), direction ="forward", trace=F)
summary(housingForward)

# The Backward Selection Technique is implemented next
housingBackward = step(full, scope= list(lower=null, upper = full), direction="backward", trace=F)
hosuingBackward = step(full, direction="backward", trace=F)
summary(housingBackward)

# Using the Stepwise search
housingStep = step(null, scope= list(lower=null, upper = full), direction="both", trace=F)
summary(housingStep)
summary(housingForward)
summary(housingBackward)
anova(housingStep, housingForward)   
anova(housingStep, housingBackward)

