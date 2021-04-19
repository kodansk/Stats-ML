### Bootstrap ### 
## Problem 1 ##
library(PerformanceAnalytics)
diabetes = read.csv("diabetes.csv", header = T)
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
#building reasonably good model
fit_1 = glm(Outcome~.,family="binomial", data = diabetes)
summary(fit_1)
fit_2 = glm(Outcome~ Pregnancies.. + Glucose.. + BloodPressure.. + Insulin.. + BMI.. + DiabetesPedigreeFunction.. + Age.. , family="binomial", data = diabetes)
anova(fit_2, fit_1, test = "Chisq")
summary(fit_2)
fit_3 = glm(Outcome~ Pregnancies.. + Glucose.. + BloodPressure.. + Insulin.. + BMI.. + DiabetesPedigreeFunction.. , family="binomial", data = diabetes)
anova(fit_3, fit_2, test = "Chisq")
# (c)
print(as.data.frame(summary(fit_2)$coefficients))
CI = as.data.frame(confint.default(fit_2))
lr.prob = predict(fit_2, data = diabetes, type="response")
lr.pred = ifelse(lr.prob >= 0.5,"1","0")
mean(lr.pred != diabetes[,9])
###################################################################
## Problem 2 ##
library(class)
library(caret)
library(e1071)
lr.prob = predict(fit_1,diabetes, type="response")
lr.pred = ifelse(lr.prob >= 0.5,"1","0")
# Training error rates
mean(lr.pred != diabetes[ ,9])
table(lr.pred, diabetes[ ,9])
# Sensitivity & Specificity
c(1180/(1180+136),388/(296+388))
# (b)
s = NULL
acc3 = NULL
#10-fold validation
set.seed(1)
for(i in 1:nrow(diabetes))
{
  dataTest = diabetes[i, ]
  dataTrain = diabetes[-i, ]
  lm.pred = predict(glm(formula = Outcome~., family="binomial", data=dataTrain),dataTest,type="response")
  4
  mod.test = ifelse(lm.pred >= 0.5, "1", "0")
  misClass_Error = (dataTest[,9] != mod.test)
  acc3[i] = misClass_Error
}
mean(acc3)
# 0.2195
# (c)
model.lr = train(as.factor(Outcome)~., diabetes, method="glm", family="binomial", metric="Accuracy",
                 trControl=trainControl(method = "LOOCV"))
error.rate = 1 - model.lr$results[2]
error.rate
# (d)
model_final.lr = train(as.factor(Outcome)~ Pregnancies.. + Glucose.. + BloodPressure.. +
                         Insulin.. + BMI.. + DiabetesPedigreeFunction.. + Age.. , diabetes,
                       method="glm", family="binomial", metric="Accuracy",
                       trControl = trainControl(method = "LOOCV"))
model_final.lr
1-0.7815
# (e)
model.lda = train(as.factor(Outcome)~., diabetes, method="lda", metric="Accuracy",
                  trControl=trainControl(method = "LOOCV"))
model.lda
1-0.777
# (f)
model.qda = train(as.factor(Outcome)~., diabetes, method="qda", metric="Accuracy",
                  trControl=trainControl(method = "LOOCV"))
model.qda
1-0.7555
# (g)
model.knn = train(as.factor(Outcome)~., diabetes, method="knn",trControl=trainControl(method = "LOOCV"),
                  tuneGrid=expand.grid(k=seq(from=1,to=100,by=1)))
print(model.knn)
###################################################################
## Problem 3 ##
library(boot)
os = read.table("oxygen_saturation.txt", header = T)
# (a)
pdf("plot_1.pdf", width = 14, height = 7)
par(mfrow=c(1,2))
plot(os$osm~os$pos, xlab = "pos", ylab = "osm")
abline(0,1, col="red")
boxplot(abs(os$osm-os$pos))
dev.off()
# (c)
tdi = function(data)
{
  abs_D = abs(data$osm-data$pos)
  tdi.hat = quantile(abs_D, p=0.9)
  return(tdi.hat)
}
theta.hat = tdi(os)
# (d)
boot.tdi = c()
bootstrap = function(nboot, data)
{
  for(i in 1:nboot)
  {
    5
    boot.sample = data[sample(1:dim(data)[1], replace = T), ]
    boot.tdi[i] = tdi(boot.sample)
  }
  return(boot.tdi)
}
boot.theta = bootstrap(10000, os)
mean(boot.theta)
mean(boot.theta) - theta.hat
se = sd(boot.theta)
uci = quantile(boot.theta, 0.95)
# (e)
tdi.fn = function(data, index)
{
  return(quantile(abs(data[index, "osm"]-data[index, "pos"]), p=0.9))
}
set.seed(1)
tdi.boot = boot(os, tdi.fn, R = 10000)
print(tdi.boot)
# bootsrap CI
ci = quantile(tdi.boot$t, 0.95)