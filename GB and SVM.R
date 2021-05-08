library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(caret)
hit = na.omit(Hitters)
hit$League = as.factor(hit$League) hit$Division = as.factor(hit$Division) hit$NewLeague = as.factor(hit$NewLeague)
# (a)
# Grow a tree
set.seed(1)
tree.hit = tree(log(Salary) ~ ., data=hit) tree.hit
summary(tree.hit)

pdf("tree.pdf", width = 7, height= 7) par(mfrow=c(1,1))
plot(tree.hit, main="Tree") text(tree.hit,pretty=0,cex=0.5) dev.off()
# predict and get training mse yhat.tree = predict(tree.hit, newdata mean((yhat.tree - log(hit$Salary))^2)
results.a = c()
for (i in 1:263)
{
  test.data = hit[i, ]
  train.data = hit[-i, ]
  fit.tree = tree(log(Salary) ~ ., data = train.data)
                  pred.tree = predict(fit.tree, newdata
                                      =test.data)
results.a[i] = (pred.tree - log(test.data$Salary))^2 }


mean(results.a)
# (b)
# LOOCV prune tree
cv.hit = cv.tree(tree.hit, FUN = prune.tree, K = 263)
# K = size cv.college
# Plot the estimated test error rate par(mfrow=c(1,2))
plot(cv.hit$size, cv.hit$dev, type = "b") plot(cv.hit$k, cv.hit$dev, type = "b")
# Get the best size cv.hit$size[which.min(cv.hit$dev)]
# Get the pruned tree of the best size prune.hit = prune.tree(tree.hit, best = 9)
summary(prune.hit)
# (c)
# bagging B=1000
fit.bag = randomForest(log(Salary) ~ ., data = hit, mtry = 19, ntree = 1000, importance = TRUE) summary(fit.bag)
results.c = c()
for (i in seq(nrow(hit))) {
  test.data = hit[i, ] train.data = hit[-i, ]
  fit.bag = randomForest(log(Salary) ~ ., data = train.data, mtry = 19, ntree = 1000, importance = TRUE) pred.bag = predict(fit.bag, newdata = test.data)
  results.c[i] = (pred.bag - log(test.data$Salary))^2 }
mean(results.c)
# (d)
# bagging B=1000
fit.rf = randomForest(log(Salary) ~ ., data = hit, mtry = 19/3, ntree = 1000, importance = TRUE) summary(fit.rf)
results.d = c()
for (i in seq(nrow(hit))) {
  test.data = hit[i, ] train.data = hit[-i, ]
  fit.rf = randomForest(log(Salary) ~ ., data = train.data, mtry = 19/3, ntree = 1000, importance = TRUE)
  
  
  pred.rf = predict(fit.rf,newdata=test.data)
                    results.d[i] = (pred.rf - log(test.data$Salary))^2}
mean(results.d)
  
# (e)
fit.bst = gbm(log(Salary),
              results.e = c(),
              for (i in seq(nrow(hit))) {
                test.data = hit[i, ] train.data = hit[-i, ]
                fit.bst = gbm(log(Salary))
                              pred.bst = predict(fit.bst, newdata = test.data, n.trees = 1000)
                              results.e[i] = (pred.bst - log(test.data$Salary))^2 })

mean(results.e)
              
library(tree)
library(ISLR)
library(e1071)
diabetes = read.table("diabetes.csv", head = T, sep = ",")
# make group a factor
diabetes$Outcome = as.factor(diabetes$Outcome)
# (a)
# linear kernel (scale = T by default)
set.seed(1)
tune1.out = tune(svm, Outcome ~ ., data = diabetes, kernel = "linear",ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5)))
summary(tune1.out)
fit1 = tune1.out$best.model 
summary(fit1)
y.pred1 = predict(fit1, diabetes)
table(predict = y.pred1, truth = diabetes$Outcome) (137+314)/2000
# (b)
# quadratic kernel (scale = T by default) set.seed(1)
tune2.out = tune(svm, Outcome ~ ., data = diabetes, kernel = "polynomial", degree = 2,
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5))) summary(tune2.out)
fit2 = tune2.out$best.model 
summary(fit2)
y.pred2 = predict(fit2, diabetes)
table(predict = y.pred2, truth = diabetes$Outcome) 
#(441+75)/2000

# (c)
tune3.out = tune(svm, Outcome ~ ., data = diabetes, kernel = "radial",
                 ranges = list(cost = 2:7,
                               gamma = c(0.001, 0.01, 0.05, 0.5))) summary(tune3.out)
fit3 = tune3.out$best.model 
summary(fit3)
ypred3 = predict(fit3, diabetes)
table(predict = ypred3, truth = diabetes$Outcome)



















