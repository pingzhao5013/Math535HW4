###Computing AIC

###Let's propose the following model.

data = na.omit(airquality)
attach(data)
model1 = lm(Ozone~Temp+Wind+Solar.R)


###To compute the logLik, AIC, and BIC I would need the following

k = 5 ##Why 5 parameters?  3 predictors, one intercept, and s^2 errors.
n = length(Ozone)
RSS = sum(model1$residuals^2)

loglik = (-n*log(RSS/n)/2 -n*log(2*pi)/2 - n/2)
AIC = 2*k - 2*loglik
BIC = log(n)*k - 2*loglik

###Or we could use the built in function for AIC and logLik.
###There is also a built in function for BIC, it's in the stats4 package.

logLik(model1)
AIC(model1)
library(stats4)
BIC(model1)

###Alternatives to AIC, BIC, partial F tests ect are shrinkage models.

# ridge regression

# Suppose we have a model that is y = B1x1 + B2x2 + e where e~N(0,1)
# Two variables are measured: x1,x2  
# Ridge regression function, ridge.lm(), is on MASS package

install.packages("MASS")
library(MASS)

# Generating the data

N <- 200      

#First let's assume x1 and x2 are independent
#Note the mean of x1 and x2 are both 0, the mean of y is 0.
#This is important, if this wasn't the case we first have to normalize.

x1 <- runif(n=N)
x2 <- runif(n=N)
e <- rnorm(n=N)
y <- x1 + x2 + e

# Ordinary least squares model for x1 and x2
ols <- lm(y~ x1 + x2)
summary(ols)
par(mfrow=c(2,2))
plot(ols)
ols$coef*(200/210)


# Ridge regression model for x1 and x2
ridge <- lm.ridge(y ~ x1+x2, lambda = 10)
summary(ridge)
ridge$coef
ridge$scales
ridge$coef/ridge$scales

ridge <- lm.ridge(y ~ x1+x2, lambda = 1:10)
summary(ridge)
ridge$coef
ridge$scales
ridge$coef/ridge$scales


#Now let's do the same exercise but let's let x1 and x2 be dependent

x1 <- runif(n=N)
x2 <- x1 + runif(n=N)
e <- rnorm(n=N)
y <- x1 + x2 + e

# Ordinary least squares model for x1 and x2
ols <- lm(y~ x1 + x2)
summary(ols)
par(mfrow=c(2,2))
plot(ols)
ols$coef*(200/210)


# Ridge regression model for x1 and x2
ridge <- lm.ridge(y ~ x1+x2, lambda = 10)
summary(ridge)
ridge$coef
ridge$scales
ridge$coef/ridge$scales

###Download the dataset "data4.csv"  .  I want you to try and find 
###the best valid model that you can for predicting y from x1,x2,x3,x4.  

###Note this will essentially be half of your next homework assignment.