### ISLR Chapter 3 Exercises ###
### Applied Question

## Question 8 

# Use the lm() function to perform a simple 
# linear regression with mpg as the response 
# and horsepower as the predictor. 
# Use the summary() function to print the results.
# Comment on the output. 

library('ISLR')
library('MASS')
library('tidyverse')

model_1 <- lm(mpg ~ horsepower, data = Auto)

summary(model_1)

predict(model_1,
        data.frame(horsepower = 98),
        interval = 'prediction'
)

# There is a relationship between horsepower and mpg, 
# R-squared of 0.6059 and RSE of 4.906,
# this is a negative relationship (coefficient is negative)
# at horsepower of 98, mpg is predicted between 14 and 34

plot(mpg ~ horsepower, data = Auto)
abline(model_1, col = 'firebrick', lwd = 3)

# diagnostic plots

par(mfrow = c(2,2))
plot(model_1)

# residuals have a u-shaped distribution, suggesting 
# polynomial fit. QQ suggests normality, not sure what
# the residuals vs leverage plot means

# wow pairs is quite the function

pairs(Auto)

cor(Auto[1:8])

model_2 <- lm(mpg ~ . -name, data = Auto)

summary(model_2)

# there's a relationship between mpg and 
# displacement, weight, year, and origin
# as year increases, so does mpg 

plot(model_2)

model_3 <- lm(mpg ~ weight + 
              year + 
              origin + 
              cylinders + 
              cylinders:displacement + 
              weight:displacement, 
              data = Auto
              )

summary(model_3)

# how are the models performing?

summary(model_1)$r.sq
summary(model_2)$r.sq
summary(model_3)$r.sq

# are models 2 and 3 actually different?

anova(model_3, model_2)

# anova isn't returning a p-value...


## transform some of the variables

plot(mpg ~ weight, data = Auto)
plot(mpg ~ log(weight), data = Auto)
# log seems to reduce curvature

plot(mpg ~ sqrt(weight), data = Auto)
# nah

plot(mpg ~ I(weight^2), data = Auto)
# worst of all


### Question 10 

model_4 <- lm(Sales ~ Price + Urban + US, data = Carseats)

summary(model_4)

par(mfrow = c(2,2))
plot(model_4)

# price inversely affects carseat sales, us-yes positively
# affects carseat sales (more than no). Urban v rural doesn't

# relevel() function can be significant here

model_5 <- lm(Sales ~ Price + US, data = Carseats)
summary(model_5)

# 95% confidence interval
confint(model_5)

par(mfrow = c(2,2))
plot(model_5)
dev.off()

# yes, a few high leverage outliers are present. 


##### question 11 #################

set.seed(1)
x <- rnorm(100)
y <- 2*x + rnorm(100)

# perform a regression without an intercept

model_6 <- lm(y ~ x + 0)
summary(model_6)

model_6_b <- lm(x ~ y + 0)
summary(model_6_b)

# the p-value and t-test statistic are the same
plot(x,y)
abline(model_6)


######## Question 13 ###############
set.seed(1)

x <- rnorm(100)
eps <- rnorm(100,0,0.25)
y <- -1 + 0.5 * x + eps

model_7 <- lm(y ~ x)

plot(x,y)

abline(coef = c(-1,0.5), col = 'firebrick', lwd = 2)
abline(model_7, col = 'slateblue', lwd = 2)
legend('topleft', inset = 0.05,
       legend = c('population', 'model'),
       col = c('firebrick','slateblue'),
       lwd = 2
       )

# now fit using a quadratic
model_7_b <- lm(y ~ x + x^2)

summary(model_7)
summary(model_7_b)

# no evidence that this is a better fit

