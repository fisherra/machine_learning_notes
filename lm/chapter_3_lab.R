#### ISLR Chapter 3 ####


### Load Libraries ###
library('tidyverse')
library('MASS')
library('ISLR')

### Explore the variables and lm() ###
names(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)

summary(lm.fit)

names(lm.fit)

coef(lm.fit)

confint(lm.fit)

### prediction ###

predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = 'confidence')

predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = 'prediction')

### plotting ###

plot(Boston$lstat, Boston$medv)
abline(lm.fit,
       col = 'firebrick', 
       lwd = 3)

### Diagnostic Plots ###

# 4 diagnostic plots at once
par(mfrow = c(2,2))
plot(lm.fit)
dev.off()

# residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# leverage statistic
plot(hatvalues(lm.fit))


### Multiple Linear Regression ###

lm.fit <- lm(medv ~ lstat + age, data = Boston)

summary(lm.fit)

lm.fit.all <- lm(medv ~ . , data = Boston)

summary(lm.fit.all)

# accessing elements of the summary
summary(lm.fit.all)$r.sq  # R^2
summary(lm.fit.all)$sigma # RSE

# Variance Inflation Factor
install.packages('car')
library('car')
vif(lm.fit.all)

# Removing a variable from the model 
lm.fit.all <- lm(medv ~ . -age, data = Boston)




### Interaction Terms ###

# just the interaction
lm.fit <- lm(medv ~ lstat:black, data = Boston)

# interaction + individual terms (short form)
lm.fit <- lm(medv ~ lstat*black, data = Boston)

# interaction + indivudal terms (long form)
lm.fit <- lm(medv ~ lstat + black + lstat:black, data = Boston)

summary(lm.fit)


### Non Linear Transformations of Predictor ###

lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit.2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)

# see if there's a difference in model fit
anova(lm.fit,lm.fit.2)

# there is! examine diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit.2)
dev.off()

# can create more complex polynomials
lm.fit.poly <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit.poly)

# 6th order polynomial is not significant
lm.fit.poly.2 <- lm(medv ~ poly(lstat, 6), data = Boston)
summary(lm.fit.poly.2)

# can also transform with log 
lm.fit.log <- lm(medv ~ log(lstat), data = Boston)
summary(lm.fit.log)



### Qualitiative Predictors ###

# view variables
names(Carseats)

# create the model with interactions
lm.fit <- lm(Sales ~ . +
             Income:Advertising + 
             Price:Age,
             data = Carseats
             )

# view the model
summary(lm.fit)

# show the dummy variables
contrasts(Carseats$ShelveLoc)




