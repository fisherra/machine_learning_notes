library('ISLR')
library('tidyverse')
library('class') # for knn function

dim(Caravan)
summary(Caravan$Purchase)

# knn doesnt understand units $1000 >> 50 years
# must standardize

# scale function
?scale

# normal(0,1)
standard_x <- scale(Caravan[, -86])

var(Caravan[,1])
var(standard_x[,1])


#### Training and Test Set #############

test <- 1:1000

train_x <- standard_x[-test, ]
test_x <- standard_x[test, ]

train_y <- Caravan$Purchase[-test]
test_y <- Caravan$Purchase[test]


#### Model Data ################

set.seed(1)

knn_pred <- knn(train_x, test_x, train_y, k = 1)
mean(test_y != knn_pred)
# error rate of 11.8% 

knn_pred <- knn(train_x, test_x, train_y, k = 5)
table(knn_pred, test_y)
# 4 / 15 correct