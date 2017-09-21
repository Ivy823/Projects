setwd("/Users/ivy/Desktop/230/project2")
library(MASS)
library(VGAM)
balance <- read.table("balance.txt", header = TRUE)
balance1 <- read.csv("balance1.csv", header = TRUE) # three-way
balance2 <- read.csv("balance2.csv", header = TRUE) # four-way
balance3 <- read.table("new.txt", header = TRUE) 

surface <- balance[,6]
bal <- balance[,8]
vision <- balance[,7]

# Investigate how balance depends on surface and vision. 
lapply(balance[, c("CTSIB", "surface", "vision")], table)
ftable(xtabs(~ Surface + Vision + Sex + factor(age) + factor(height) + factor(weight) + CTSIB, data = balance3))


balance$gender <- ifelse(balance$Sex == "male", 0, ifelse(balance$Sex == "female", 1,2))

########## vglm ###########
fit <- vglm(CTSIB ~Surface + Vision + Sex + Height + Weight, 
        family =cumulative(parallel=TRUE) ,data=training)
fit1 <- vglm(CTSIB ~Surface + Vision + Sex  + Height + Weight, 
             family =cumulative(parallel=FALSE) ,data=test)

fitfull <- vglm(CTSIB ~Surface + Vision + Sex + factor(age) + factor(height) + factor(weight), 
                family =cumulative(parallel=TRUE) ,data=balance3)
fitfull1 <- vglm(CTSIB ~Surface + Vision + Sex + factor(height) + factor(weight), 
                family =cumulative(parallel=TRUE) ,data=balance3)

fit1 <- vglm(cbind(Y1,Y2,Y3,Y4) ~ Surface + Vision, 
                family =cumulative(parallel=TRUE) ,reverse = TRUE, data=balance1)
fit11 <- vglm(CTSIB ~ Surface + Vision, 
              family =cumulative(parallel=TRUE) ,reverse = TRUE, data=balance)
fit1.1 <- vglm(cbind(Y1,Y2,Y3,Y4) ~ factor(Surface) + factor(Vision) + factor(Sex), 
             family =cumulative(parallel=TRUE) ,reverse = TRUE, data=balance2)
fit1.1non <- vglm(cbind(Y1,Y2,Y3,Y4) ~ factor(Surface) + factor(Vision) + factor(Sex), 
               family =cumulative(parallel=FALSE), data=balance2)
# odds ratios
exp(coef(cv.tr.1))
exp(coef(fit1.1))
# check parallelism
is.parallel(fit)
is.parallel(fit1.1)
### Check proportional odds assumption ###
# Difference in -2 log likelihood
(diff.neg.2logLik <- -2*(logLik(fit) - logLik(fit1)))
diff.df <- df.residual(fit)-df.residual(fit1)
pchisq(q = diff.neg.2logLik, df = diff.df, lower.tail = FALSE)
lrtest(fit1, fit)


############ plor ##############
fit.clogit0 <- polr(factor(CTSIB) ~ 1, data=balance)
fit.clogit <- polr(factor(CTSIB) ~ Surface + Vision, data=balance)
fit.clogitfull <- polr(factor(CTSIB) ~ Surface + Vision + Sex + Height + Weight, data=test)
fit.clogitfull11 <- polr(factor(CTSIB) ~ Surface + Vision + Sex + Age + Height + Weight, data=test)
fit.clogitfull1 <- polr(factor(CTSIB) ~ Surface + Vision + Sex + factor(Height) + factor(Weight), data=test)


############ CV #############
library(rms)
a = seq(1,480,2)
training <- balance[-a,]
b = seq(0,480,2)
test <- balance[-b,]
attach(training)
attach(test)
# option 1
cv.te <- polr(factor(CTSIB) ~ Surface + Vision + Age + Sex + Height + Weight, data=test)
cv.tr <- lrm(factor(CTSIB) ~ Surface + Vision + Age + Sex + Height + Weight, data=training)
summary(cv.tr)
step(cv.tr, direction="both")
cv.te.1 <- polr(factor(CTSIB) ~ Surface + Vision + Sex + Height + Weight, data=test)
cv.tr.1 <- lrm(factor(CTSIB) ~ Surface + Vision + Sex + Height + Weight, data=training)
cv.tr.2 <- lrm(factor(CTSIB) ~ Surface + Vision + Height + Weight, data=training)
summary(cv.te.1)

library(leaps)
a<- regsubsets(formula(cv.tr),data=training, method="exhaustive")
(rs = summary(a))
n = nrow(training)
AIC = n*log(rs$rss) + 2*(2:8)
plot(2:8, AIC, xlab="Number of parameters", ylab="AIC")

confint(cv.tr)
pred1 <- data.frame(
  sex2 = test$Sex,
  surface2 =test$Surface,
  vision2 = test$Vision,
  age2 = test$Age,
  weight2 = test$Weight,
  height2 = test$Height)
library(Hmisc)
(s.half2 = with(data2,summary(as.numeric(ctsib2)~ surface2 + vision2+sex2 +age2 + height2 + weight2, fun = sf)))
predict(cv.tr.1,pred1,type = 'class')
pred11 = cbind(pred1,predict(cv.tr.2,pred1))
head(pred11)
prediction = cbind(test,pred11[,7])
pred.weight = cbind(pred11[,2],pred11[,3],pred11[,5])

pred22 <- pred11
pred22 = cbind(pred11,predict(cv.tr.1,pred11,type = 'probs'))
head(pred22)
prob = predict(cv.tr.1,pred11,type = 'probs')

# option 2
a1 = seq(1,480,2)
training1 <- balance3[-a1,]
b1 = seq(0,480,2)
test1 <- balance3[-b1,]
attach(training1)
attach(test1)
cv.te.f <- polr(factor(CTSIB) ~ Surface + Vision + Sex + factor(age) + factor(height) 
                + factor(weight), data=test1)
step(cv.te.f, direction="both")
cv.te.f.1 <- polr(factor(CTSIB) ~ Surface + Vision + factor(height) + factor(weight), data=test1)
summary(cv.te.f.1)
anova(cv.te.f.1, cv.te.1)


