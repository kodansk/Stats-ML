# Regression (logistic and lASSO) and KNN
Problem 1
# Package Load
library(boot)
library(caret)
library(leaps)
library(bestglm)
library(glmnet)
wine = read.table("wine.txt", header=T)
wine$Region = as.factor(wine$Region)
str(wine)
# (a)
set.seed(1)
model.lr = train(Quality ~ ., data = wine, trControl = trainControl(method = "LOOCV"),
                 method = "glm", metric = "RMSE")
model.lr$finalModel
model.lr
1.065438^2
# (b)
# Fit the best model
fit.cmd = regsubsets(Quality ~ ., data = wine, method="exhaustive")
summary.out=summary(fit.cmd)
which.max(summary.out$adjr2)
summary.out$which[4,]
model.cmd = train(Quality ~ Flavor+Oakiness+Region, data = wine, method = "glm",
                  trControl = trainControl(method = "LOOCV"), metric = "RMSE")
model.cmd$finalModel
model.cmd
0.9330443^2
# (c)
# Use forward stepwise selection to find best LR model.
fit.fwd = regsubsets(Quality ~ ., data = wine, method="forward")
summary.fwd=summary(fit.fwd)
which.max(summary.fwd$adjr2)
summary.fwd$which[4,]
model.fwd = train(Quality ~ Flavor+Oakiness+Region, data = wine, method = "glm",
                  trControl = trainControl(method = "LOOCV"), metric = "RMSE")
model.fwd
# (d)
# Use backward stepwise selection to find best LR model.
fit.bwd = regsubsets(Quality ~ ., data = wine, method="backward")
summary.bwd=summary(fit.bwd)
which.max(summary.bwd$adjr2)
summary.bwd$which[4,]
model.fwd = train(Quality ~ Flavor+Oakiness+Region, data = wine, method = "glm",
                  trControl = trainControl(method = "LOOCV"), metric = "RMSE")
model.fwd
# (e)
grid = 10^seq(10, -2, length = 1000)
y = wine$Quality
# Create the design matrix (without the first column of 1s)
x = model.matrix(Quality ~ ., data=wine)[ , -1]
# Find test error rate with k-fold cv
set.seed(1)
model.ridge = cv.glmnet(x, y, alpha = 0, type.measure = "mse", nfolds = 38,lambda = grid)
plot(model.ridge)
bestlam.ridge = model.ridge$lambda.min
3
ridge.pred = predict(model.ridge, s = bestlam.ridge, type="response", newx = x)
mean((ridge.pred-y)^2)
fit.ridge = glmnet(x, y, alpha = 0, lambda = bestlam.ridge)
coef(model.ridge)
# (f)
set.seed(1)
model.lasso = cv.glmnet(x, y, alpha = 1, type.measure = "mse", nfolds = 38, lambda = grid)
plot(model.lasso)
bestlam = model.lasso$lambda.min
lasso.pred = predict(model.lasso, s = bestlam, type="response", newx = x)
mean((lasso.pred-y)^2)
fit.lasso = glmnet(x, y, alpha = 1, lambda = bestlam)
coef(model.lasso)
# Problem 2
# Package Load
library(boot)
library(caret)
library(leaps)
library(bestglm)
library(glmnet)
diabetes = read.csv("diabetes.csv", header = T)
# (a)
# Fit a logistic regression.
set.seed(1)
train.control = trainControl(method="cv", number=10)
model.lr = train(as.factor(Outcome) ~ ., data = diabetes, trControl = train.control,
                 method = "glm", family = "binomial", metric = "Accuracy")
model.lr$finalModel
model.lr
1-0.7805178
# (b)
# Fit the best model
fit.cmd = bestglm(diabetes, IC="AIC", method="exhaustive", family=binomial)
fit.cmd
# Find test error rate with k-fold cv
set.seed(1)
model.cmd = train(as.factor(Outcome) ~ Pregnancies.. + Glucose.. + BloodPressure.. + Insulin.. + BMI.. + DiabetesPedigreeFunction.. + Age..,
                  data = diabetes, method = "glm", trControl = train.control)
model.cmd$finalModel
model.cmd
1-0.7800303
# (c)
# Use forward stepwise selection to find best LR model.
fit.fwd = bestglm(diabetes, IC="AIC", method="forward", family=binomial)
fit.fwd
# Find test error rate with k-fold cv
set.seed(1)
model.fwd = train(as.factor(Outcome) ~ Pregnancies.. + Glucose.. + BloodPressure.. + Insulin.. + BMI.. + DiabetesPedigreeFunction.. + Age..,
                  data = diabetes, method = "glm", trControl = train.control)
model.fwd
# (d)
# Use backward stepwise selection to find best LR model.
fit.bwd = bestglm(diabetes, IC="AIC", method="backward", family=binomial)
fit.bwd
# Find test error rate with k-fold cv
set.seed(1)
4
model.bwd = train(as.factor(Outcome) ~ Pregnancies.. + Glucose.. + BloodPressure.. + Insulin.. + BMI.. + DiabetesPedigreeFunction.. + Age..,
                  data = diabetes, method = "glm", trControl = train.control)
model.bwd
# (e)
grid = 10^seq(10, -2, length = 1000)
y = as.factor(diabetes$Outcome)
# Create the design matrix (without the first column of 1s)
x = model.matrix(as.factor(Outcome) ~ ., data = diabetes)[, -1]
# Find test error rate with k-fold cv
set.seed(1)
model.ridge = cv.glmnet(x, y, alpha = 0, family = "binomial", type.measure = "class", nfolds = 10, lambda = grid)
plot(model.ridge)
bestlam.ridge = model.ridge$lambda.min
ridge.pred = predict(model.ridge, s = bestlam.ridge, type="class", newx = x)
mean(y!=ridge.pred)
fit.ridge = glmnet(x, y, family="binomial", alpha = 0, lambda = bestlam.ridge)
coef(model.ridge)
# (f)
# Find test error rate with k-fold cv
set.seed(1)
model.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class", nfolds = 10, lambda = grid)
plot(model.lasso)
bestlam = model.lasso$lambda.min
lasso.pred = predict(model.lasso, s = bestlam, type="class", newx = x)
mean(y!=lasso.pred)
fit.lasso = glmnet(x, y, family = "binomial", alpha = 1, lambda = bestlam)
coef(model.lasso)