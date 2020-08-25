# Initialize variables to load data from provided excel
carseatstrain <- read.csv(file.choose(),header = T)
carseatstest <- read.csv(file.choose(),header = T)

library(rpart)
library(tree)
library(ISLR)
#regression tree
tree_carseats = tree(carseatstrain$Sales~.,carseatstrain)
summary(tree_carseats)


plot(tree_carseats)
text(tree_carseats,pretty=0)

#cross validation to check tree complexity
cv.tree_carseats=cv.tree(tree_carseats)
plot(cv.tree_carseats$size,cv.tree_carseats$dev,type='b', main = 'Tree Complexity')
cv.tree_carseats

#calculate mse
tree_trainpredict <- predict(tree_carseats,newdata = carseatstrain)
mse_train <- mean((tree_trainpredict - carseatstrain$Sales)^2)
tree_testpredict <- predict(tree_carseats,newdata = carseatstest)
mse_test <- mean((tree_testpredict - carseatstest$Sales)^2)
mse_train
mse_test

#tree pruning
prune.tree_carseats=prune.tree(tree_carseats, best = 13)
plot(prune.tree_carseats)
text(prune.tree_carseats,pretty=0)

#calculate test mse after pruning
prune_tree_testpredict <- predict(prune.tree_carseats,newdata = carseatstest)
prune_mse_test <- mean((prune_tree_testpredict - carseatstest$Sales)^2)
prune_mse_test
prune_tree_trainpredict <- predict(prune.tree_carseats,newdata = carseatstrain)
prune_mse_train <- mean((prune_tree_trainpredict - carseatstest$Sales)^2)
prune_mse_train



#Bagged Trees
library(randomForest)
bag.carseats = randomForest(Sales~.,data = carseatstrain, mtry=9, importance = TRUE)
bag.carseats

#calculate test and train MSE
bag.train.pred = predict(bag.carseats, newdata = carseatstrain)
bag.MSE.train = mean((bag.train.pred-carseatstrain$Sales)^2)
bag.test.pred = predict(bag.carseats, newdata = carseatstest)
bag.MSE.test = mean((bag.test.pred-carseatstest$Sales)^2)
bag.MSE.test
bag.MSE.train


#Random forest
rf.carseats = randomForest(Sales~.,data = carseatstrain, mtry=3, importance = TRUE)
rf.carseats

#calculate test and train MSE
rf.train.pred = predict(rf.carseats, newdata = carseatstrain)
rf.MSE.train = mean((rf.train.pred-carseatstrain$Sales)^2)
rf.test.pred = predict(rf.carseats, newdata = carseatstest)
rf.MSE.test = mean((rf.test.pred-carseatstest$Sales)^2)
rf.MSE.test
rf.MSE.train


#Boosted Regression tree
library(gbm)
boost.carseats = gbm(Sales~.,data = carseatstrain, distribution = "gaussian",,n.trees=1500,interaction.depth=5,shrinkage = 0.01)

summary(boost.carseats)

yhat.boost_train=predict(boost.carseats,newdata=carseatstrain,n.trees=750)
boost.MSE.train = mean((yhat.boost_train-carseatstrain$Sales)^2)
yhat.boost_test=predict(boost.carseats,newdata=carseatstest,n.trees=750)
boost.MSE.test = mean((yhat.boost_test-carseatstest$Sales)^2)
boost.MSE.test
boost.MSE.train

