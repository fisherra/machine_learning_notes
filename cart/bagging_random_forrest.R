### Introduction to Statistical Learning
### Chapter 8 - Bagging and Random Forest
### Fisher Ankney

library(MASS)
library(ISLR)
library(randomForest)

# for reproducability
set.seed(1)

# training set
train <- sample(1:nrow(Boston), nrow(Boston/2))

# test set 
boston.test <- Boston[-train, 'medv']

# bagging
bag.boston <- randomForest(medv ~ .,
                           data = Boston,
                           subset = train,
                           mtry = 13,
                           importance = T)

# mtry = 13 means all 13 predictors are used for each split

bag.boston
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

plot(yhat.bag, bag.boston)
abline(0,1)

### Change number of trees using ntree arguement

bag.boston <- randomForest(medv ~ .,
                           data = Boston, 
                           subset = train, 
                           mtry = 13, 
                           ntree = 25)

yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

plot(yhat.bag, boston.test)
abline(0,1)

mean((yhat.bag - boston.test)^2)


### Random Forest 

rf_boston <- randomForest(medv ~ . ,
                          data = Boston, 
                          subset = train, 
                          mtry = 6, 
                          importance = T)

yhat_rf <- predict(rf_boston, newdata = Boston[-train, ])

plot(yhat_rf, boston.test)
abline(0,1)

mean((yhat_rf - boston.test)^2)

# wow, down to MSE = 11.48

# how important is each variable?
importance(rf_boston)

varImpPlot(rf_boston)


### Boosting ###############

library(gbm)

set.seed(1)

boost.boston <- gbm(medv ~ . , 
                    data = Boston[train, ],
                    distribution  = 'gaussian', 
                    n.trees = 5000, 
                    interaction.depth = 4)

summary(boost.boston)

yhat.boost <- predict(boost.boston, newdata = Boston[-train, ],
                      n.trees = 5000)

mean((yhat.boost - boston.test)^2)
