
library(ISLR)
library(PerformanceAnalytics)
hit = na.omit(Hitters)
hit.quant = hit[ , -c(14, 15, 20)] hit.quali = hit[ , c(14, 15, 20)]
hit$League = ifelse(hit$League =="A", 1, 0)
hit$Division = ifelse(hit$Division =="E", 1, 0)
hit$NewLeague = ifelse(hit$NewLeague =="A", 1, 0)
# (a)
pdf("corr.pdf", width = 7, height= 7) chart.Correlation(hit.quant) dev.off()
mean = apply(hit.quant, 2, mean) sd = apply(hit.quant, 2, sd)
# (b)
# Perform PCA
pca = prcomp(hit, center = T, scale = T)
pca$rotation # loadings
components = pca$rotation
# Compute the proportion of variance explained (PVE) pc.var = pca$sdev^2
pve = pc.var/sum(pc.var)
pve
cumsum(pve)
# Scree plot
pdf("scree_plot.pdf", width = 7, height= 7)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance", main = "Scree Plot", ylim = c(), type = "b")
dev.off()
# Plot of cumulative PVE
pdf("cum_plot.pdf", width = 7, height= 7)
plot(cumsum(pve), xlab = "Principal Component", main = "Cumulative Variance Explained", ylab = "Cumulative Proportion of Variance ", type = "b")
dev.off()
# Sample covariance between Z_k and X_j: lambda_k* phe_jk x.std = apply(hit, 2, function(x){(x-mean(x))/sd(x)}) cov.hit = cov(x.std) # cor(x.std)
ev = eigen(cov.hit)
values = ev$values
vectors = ev$vectors
# (c)
# correlation between PC1, PC2 & variables
correlation = data.frame(rbind(sqrt(values[1])*vectors[, 1],sqrt(values[2])*vectors[, 2])) colnames(correlation) = colnames(hit)
# Get the score matrix
pca$rotation
pdf("biplot.pdf", width = 14, height= 14) biplot(pca, scale = 0)
dev.off()
#################################################################

library(PerformanceAnalytics)
library(psych)
5
library(MASS)
#(c)
hit.scaled = scale(hit)
row.names(hit.scaled) = c(1:263)
hit.complete = hclust(dist(hit.scaled), method = "complete")
pdf("dendogram.pdf", width = 21, height= 14)
par(mfrow=c(1,1))
plot(hit.complete, main = " Scaled Features", xlab = "", sub = "", cex = 0.7) a = cutree(hit.complete, 2)
a
abline(h=14.5, col="red")
dev.off()
newdata=cbind(hit,a)
splitdata = split(newdata,newdata$a)
apply(splitdata$’1’[,1:20],2,mean)
apply(splitdata$’2’[,1:20],2,mean)
# (e)
#KNN clustering
set.seed(1)
km.out = kmeans(hit.scaled, 2, nstart = 20) km.out
b = km.out$cluster 
sum(km.out$cluster[1:101])
newdata.1 = cbind(hit,b) 
splitdata.1=split(newdata.1,newdata.1$b) 
apply(splitdata.1$’1’[,1:20],2,mean) 
apply(splitdata.1$’2’[,1:20],2,mean)
##############################################################

library(caret)
library(glmnet)
library(boot)
# (a)
# calculate using cv.glm()
# fit a linear model
lm.fit = glm(log(Salary) ~ ., data = hit)
cv.err = cv.glm(hit, lm.fit) cv.err$delta
model.lm = train(log(Salary) ~ ., data = hit, method = "lm", trControl = trainControl(method = "LOOCV" )) names(model.lm)
model.lm$finalModel
model.lm$results$RMSE
model.lm$results$RMSE^2
#(b)
myGrid = expand.grid(ncomp = seq(1, 20, 1))
model.pcr = train(log(Salary) ~ ., data = hit, method = "pcr", scale = TRUE, tuneGrid = myGrid, trControl = trainControl(method = "LOOCV"))
# Optimal M
opt.m.pcr = model.pcr$bestTune 
opt.m.pcr
model.pcr$finalModel
# RMSE
# min(model.pcr$results$RMSE) model.pcr$results$RMSE[16]
# MSE
pcr.test.error = (min(model.pcr$results$RMSE[16]))^2
6

pcr.test.error
# (c)
model.pls = train(log(Salary) ~ ., data = hit, method = "pls", scale = TRUE, tuneGrid = myGrid, trControl = trainControl(method = "LOOCV" ))
# Optimal M
opt.m.pls = model.pls$bestTune 
opt.m.pls
model.pls$finalModel
# RMSE min(model.pls$results$RMSE) model.pls$results$RMSE[12]
# MSE
pls.test.error = (min(model.pls$results$RMSE))^2 
pls.test.error
# (d)
# find the range of lambda on ridge regression
y = log(hit$Salary)
x = model.matrix(log(hit$Salary) ~ ., hit)[, -1]
ridge.mod = glmnet(x, y, alpha = 0) plot(ridge.mod, xvar = "lambda")
cv.out = cv.glmnet(x, y, alpha = 0) cv.out$lambda.min
# get LOOCV cross validation using caret package: train()
myGrid = expand.grid(alpha = 0, lambda = seq(0.01, 1, length = 200)) model.ridge = train(log(Salary) ~ ., data = hit, method = "glmnet", scale = T, tuneGrid = myGrid, trControl = trainControl(method = "LOOCV"))
model.ridge
# see the result of ridge regression
# Optimal lambda
i = which(model.ridge$results$RMSE == min(model.ridge$results$RMSE)) opt.lambda.ridge = cv.out$lambda.min
# fit model on whole data
model.best.ridge = glmnet(x, y, alpha = 0, lambda = opt.lambda.ridge)
# get coefficients of the model
predict(model.best.ridge, s = opt.lambda.ridge, type = "coefficients")[1:18, ] # mean square error min(model.ridge$results$RMSE)
ridge.test.error = (min(model.ridge$results$RMSE))^2
ridge.test.error