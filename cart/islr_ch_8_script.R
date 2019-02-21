library(ISLR)

High <- ifelse(Carseats$Sales <= 8, "No", "Yes")

Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High ~ . -Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats)

set.seed(2)
train <- sample(1:nrow(Carseats), 200)

Carseats.test <- Carseats[-train, ]
High.test <- High[-train]

tree.carseats <- tree(High ~ . -Sales, Carseats, subset = train)
tree.predict <- predict(tree.carseats, Carseats.test, type = 'class')

table(tree.predict, High.test)

set.seed(3)

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

cv.carseats

# dev refers to cross validation error rate, 9 terminal nodes = lowest rate

par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')
dev.off()

prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats)

# how good is it?

tree.predict <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.predict, High.test)

# 77%


### Fitting Regression Trees

library(MASS)

set.seed(1)

train <- sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston <- tree(medv ~ . , data = Boston, subset = train)

summary(tree.boston)
plot(tree.boston)
text(tree.boston)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, 'medv']

plot(yhat, boston.test)
abline(0,1)
