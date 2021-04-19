## Use Wine data ; admission data and diabetes data 
## For Linear discriminant analysis and QDA
## Problem 1 ##
library(PerformanceAnalytics)
library(MASS)
library(lmtest)
library(car)
wine = read.table("wine.txt", header = T)
wine$Region = as.factor(wine$Region)
summary(wine)
# (a)
pdf("scatter_quality.pdf", width = 7, height= 7)
chart.Correlation(wine[ , -7]) #matrix of scatterplot
dev.off()
pdf("Region.pdf", width = 7, height= 7)
boxplot(wine$Quality ~ wine$Region)
dev.off()
# (b)
mod.1 = lm(Quality~., data = wine)
summary(mod.1)
plot(mod.1)
b = boxcox(mod.1) #evaluate possible Box-Cox transformations (MASS package must be installed)
plot(b)
mod.2 = lm(sqrt(Quality)~., data = wine)
summary(mod.2)
plot(mod.2)
mod.3 = lm(log(Quality)~., data = wine)
summary(mod.3)
plot(mod.3)
plot(mod.1$residuals,ylab="Quality_Residuals")
plot(mod.2$residuals,ylab="Sqrt_Quality_Residuals")
plot(mod.3$residuals,ylab="Log_Quality_Residuals")
# (c)
#For each predictor, fit a simple linear regression model to predict
#the response. Describe your results. In which of the models is
#there a statistically significant association between the predictor
#and the response? Create some plots to back up your assertions.
options(digits = 3)
pval=c()
# regress log transformed psa against each predictor separately
for (i in 1:ncol(wine))
{
  if(i!=6)
  {
    result=summary(lm(wine$Quality ~ wine[,i]))
    pval=c(pval,result$coefficients[2,4]) # record p-values of t-test
  }
}
M=NULL
M=data.frame(pval)
M=cbind(names(wine)[c(1:5,7)],M,ifelse(pval<0.05,’Yes’,’No’))
names(M)=c(’Predictor’,’T-test p-value’,’Significant’)
print(M) # print results
# (d)
#Fit a multiple regression model to predict the response using
6
#all of the predictors. Describe your results. For which predictors
#can we reject the null hypothesis
#regress length against all predictors
mod.1=lm(Quality ~., data = wine)
summary(mod.1)
mod.1b = lm(Quality ~Clarity + Aroma + Body + Flavor + Oakiness, data = wine)
anova(mod.1b, mod.1)
# (e)
#Build a "reasonably good" multiple regression model for these data. Be sure to explore interac-
#tions of Region with other predictors. Carefully justify all the choices you make in building
#the model and verify the model assumptions.
#Including interaction with qualitative variable
mod.2 = lm(Quality ~ (Flavor + Region + Flavor:Region), data = wine)
summary(mod.2)
mod.3 = lm(Quality ~ (Flavor + Region), data = wine)
summary(mod.3)
anova(mod.3, mod.1)
anova(mod.3, mod.2)
# Check model assumptions
plot(fitted(mod.3), resid(mod.3), ylab = "residuals",
     xlab = "fitted values")
abline(h=0, lty = 2)
plot(mod.3)
shapiro.test(mod.3$residuals) # Shapiro-Wilk - normality
bptest(mod.3) # Breusch Pagan Test
# (g)
# predict Quality for wine whose quantitative predictors are at the sample means
# of the variables and qualitative predictors are at the most frequent category
7.09 + 1.12*mean(wine$Flavor)
#confidence and prediction interval
predict(mod.3, data.frame(Flavor = mean(wine$Flavor), Region = "1"), interval="confidence")
predict(mod.3, data.frame(Flavor = mean(wine$Flavor), Region = "1"), interval="prediction")
predict(mod.3, data.frame(Flavor = mean(wine$Flavor), Region = "2"), interval="confidence")
predict(mod.3, data.frame(Flavor = mean(wine$Flavor), Region = "2"), interval="prediction")
predict(mod.3, data.frame(Flavor = mean(wine$Flavor), Region = "3"), interval="confidence")
predict(mod.3, data.frame(Flavor = mean(wine$Flavor), Region = "3"), interval="prediction")
##################################################
## Problem 2 ##
library(MASS)
admission = read.csv("admission.csv", header = T)
attach(admission)
test_index = c()
for(i in 1:3)
{
  test_index = c(test_index,which(admission$Group==i)[1:5])
}
test.x = cbind(GPA,GMAT)[test_index,]
test.y = cbind(Group)[test_index]
train.x = cbind(GPA,GMAT)[-test_index,]
train.y = Group[-test_index]
7
as.factor(train.y)
# (a)
pdf("admission_explore.pdf", width = 7, height= 7)
par(mfrow=c(1,2))
boxplot(admission[-test_index,]$GPA ~ admission[-test_index,]$Group, ylab="GPA", xlab="Groups")
boxplot(admission[-test_index,]$GMAT ~ admission[-test_index,]$Group, ylab="GMAT", xlab="Groups")
par(mfrow=c(1,1))
dev.off()
# (b)
# Performing LDA on training data
train = data.frame(cbind(train.x,train.y))
lda_fit = lda(train.y ~ GPA+GMAT, data=train)
coef(lda_fit)
lda.pred_train = predict(lda_fit, data.frame(train.x))
lda.pred_train$posterior
table(lda.pred_train$class, train.y)
mean(lda.pred_train$class != train.y)
lda.pred_test = predict(lda_fit, data.frame(test.x))
lda.pred_test$posterior
table(lda.pred_test$class, test.y)
mean(lda.pred_test$class!=test.y)
n.grid = 100
x1.grid = seq(f = 0, t = 4, l = n.grid)
x2.grid = seq(f = 300, t = 700, l = n.grid)
grid = expand.grid(x1.grid, x2.grid)
colnames(grid) = colnames(test.x)
pred.grid = predict(lda_fit, grid)
pdf("lda_decision.pdf", width = 7, height= 7)
prob1 = matrix(pred.grid$posterior[, "1"], nrow = n.grid, ncol = n.grid, byrow = F)
prob2 = matrix(pred.grid$posterior[, "2"], nrow = n.grid, ncol = n.grid, byrow = F)
plot(train.x, xlab = "GPA", ylab = "GMAT", col = ifelse(train.y == 1, "green",ifelse(train.y==2,"blue","red")))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "",
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "",
        main = "", col="red",add = T)
legend("topleft", legend = c("Group 1", "Group 2", "Group 3"), col = c("green", "blue", "red"), pch=1)
dev.off()
# (c)
train = data.frame(cbind(train.x,train.y))
colnames(train) = c("GPA","GMAT","y")
qda_fit = qda(y~GPA+GMAT, data=train)
# Get predictions for test data
qda.pred_train = predict(qda_fit, data.frame(train.x))
table(qda.pred_train$class, train.y)
mean(qda.pred_train$class!=train.y)
qda.pred_test = predict(qda_fit, data.frame(test.x))
table(qda.pred_test$class, test.y)
mean(qda.pred_test$class!=test.y)
pdf("qda_decision.pdf", width = 7, height= 7)
prob3 = matrix(pred.grid$posterior[, 1], nrow = n.grid, ncol = n.grid, byrow = F)
prob4 = matrix(pred.grid$posterior[, 2], nrow = n.grid, ncol = n.grid, byrow = F)
plot(train.x, xlab = "GPA", ylab = "GMAT", col = ifelse(train.y == 1, "green",ifelse(train.y==2,"blue","red")))
contour(x1.grid, x2.grid, prob3, levels = 0.5, labels = "", xlab = "", ylab = "",
        main = "", add = T)
contour(x1.grid, x2.grid, prob4, levels = 0.5, labels = "", xlab = "", ylab = "",
        main = "", col="red",add = T)
8
legend("topleft", legend = c("Group 1", "Group 2", "Group 3"), col = c("green", "blue", "red"), pch=1)
dev.off()
#########################################
## Problem 3 ##
library(MASS)
library(class)
library(pROC)
diabetes = read.csv("diabetes.csv", header = T)
train.y = as.factor(diabetes$Outcome)
train.x = diabetes[,-9]
# (a)
pdf("diabetes_explore.pdf", width = 14, height= 14)
par(mfrow=c(2,4))
boxplot(diabetes$Pregnancies.. ~ diabetes$Outcome, ylab="Pregnancies", xlab="Outcome")
boxplot(diabetes$Glucose.. ~ diabetes$Outcome, ylab="Glucose", xlab="Outcome")
boxplot(diabetes$BloodPressure.. ~ diabetes$Outcome, ylab="BloodPressure", xlab="Outcome")
boxplot(diabetes$SkinThickness.. ~ diabetes$Outcome, ylab="SkinThickness", xlab="Outcome")
boxplot(diabetes$Insulin.. ~ diabetes$Outcome, ylab="Insulin", xlab="Outcome")
boxplot(diabetes$BMI.. ~ diabetes$Outcome, ylab="BMI", xlab="Outcome")
boxplot(diabetes$DiabetesPedigreeFunction.. ~ diabetes$Outcome, ylab="DiabetesPedigreeFunction", xlab="Outcome")
boxplot(diabetes$Age.. ~ diabetes$Outcome, ylab="Age", xlab="Outcome")
par(mfrow=c(1,1))
dev.off()
# (b)
lda.fit = lda(as.factor(Outcome) ~ ., data = diabetes)
coef(lda.fit)
lda.pred = predict(lda.fit, diabetes)
table(lda.pred$class, diabetes$Outcome)
mean(lda.pred$class != diabetes$Outcome)
# (c)
qda.fit = qda(as.factor(Outcome) ~ ., data = diabetes)
qda.pred = predict(qda.fit, diabetes)
table(qda.pred$class, diabetes$Outcome)
mean(qda.pred$class != diabetes$Outcome)
# (d)
#ROC Curve
roc.lda = roc(train.y, lda.pred$posterior[, "1"], levels = c("0", "1"))
roc.qda = roc(train.y, qda.pred$posterior[, "1"], levels = c("0", "1"))
pdf("ROC.pdf", width = 7, height= 7)
plot(roc.lda, col="RED", legacy.axes = T)
plot(roc.qda,add=T, col="BLUE", legacy.axes = T)
legend("bottomright", legend = c("LDA", "QDA"),
       col = c("red", "blue"), lty =1)
dev.off()
lda.pred.adj = ifelse(lda.pred$posterior[, 2] > .30, "1", "0")
#Misclasification rate
table(lda.pred.adj, diabetes$Outcome)
pdf("threshold.pdf", width = 7, height= 7)
plot(roc.lda$thresholds, roc.lda$sensitivities, type = "l",
     col="black", xlab = "threshold", ylab = "probability")
lines(roc.lda$thresholds, roc.lda$specificities,
      col = "blue")
legend("right", legend = c("sensitivity", "specificity"),
       col = c("black", "blue"), lty =1)
dev.off()