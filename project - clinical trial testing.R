setwd("/Users/ivy/Desktop/220B/final project")
nasal_spray <- read.csv("VirusAndNutrition.csv", header = TRUE)
attach(nasal_spray)

NumInf <- nasal_spray$NumInf
NasalSpray <- nasal_spray$NasalSpray
Vitamin <- nasal_spray$Vitamin
Age <- nasal_spray$Age
Fruit <- nasal_spray$Fruit
Gender <- nasal_spray$Gender
Height <- nasal_spray$Height
Weight <- nasal_spray$Weight
Soy <- nasal_spray$Soy
Days <- nasal_spray$Days
City <- nasal_spray$City


############# EDA #################
summary(nasal_spray)
sapply(nasal_spray,sd)
# scatter
pairs(nasal_spray)
# Boxplot
par(mfrow=c(1,3),
    oma=c(1,1,1,1),
    mar=c(2,2,2,2))
boxplot(NumInf[NasalSpray=="1"],NumInf[NasalSpray=="2"],
        names=c("antiviral nasal spray","Placebo"),main="NumInf")
boxplot(Days[NasalSpray=="1"],Days[NasalSpray=="2"],
        names=c("antiviral nasal spray","Placebo"),main="Days")
boxplot(Age[NasalSpray=="1"],Age[NasalSpray=="2"],
        names=c("antiviral nasal spray","Placebo"),main="Age")
# boxplot(Weight[NasalSpray=="1"],Weight[NasalSpray=="2"],names=c("antiviral nasal spray","Placebo"),main="Weights")
# Histogram
hist(NumInf)
# 
plots <-function(x){
  par(mfrow=c(1,3),lwd=0.7,
      oma=c(1,1,1,1),
      mar=c(2,2,2,2))
  hist(x)          # histograms
  qqnorm(x)      # QQ-plots
  qqline(x, col = "red")
  plot(density(x))    # Kernel density estimates
}
plots(NumInf)

############## fit models ################
fit <- glm(NumInf ~ factor(NasalSpray) + factor(Vitamin) + factor(Gender) + Days 
           + Age + Weight + Height + factor(Fruit) + factor(Soy)+ factor(City) + offset(log(Days)), family=poisson, data=nasal_spray)
step(fit, scale=1, direction="both")
fit.1 <- glm(NumInf ~ factor(NasalSpray) + factor(Vitamin) + Days 
           + Age + Weight + factor(Soy)+ offset(log(Days)), family=poisson, data=nasal_spray)
summary(fit.1)
# AIC 250.46    phi = 134.04/121 = 1.107
anova(fit.1, test="Chisq")
avPlots(fit.1)
fit.2 <- glm(NumInf ~ factor(NasalSpray) + factor(Vitamin) + Days 
             + Age + Weight + factor(Soy)+ factor(Soy)*factor(NasalSpray)
             + factor(Vitamin)*Age + Days* factor(NasalSpray) + offset(log(Days)), family=poisson, data=nasal_spray)


fit1.1 <- stepAIC(fit,~.^2,scale=1, trace=F)  # 241.52
# fit1.1 <- glm(NumInf ~ factor(NasalSpray) + factor(Vitamin) + factor(Gender) + Days 
#            + Age + Height + factor(Fruit) + factor(Soy)+ factor(City) 
#            + Age*factor(Soy) + factor(Vitamin)*Height + Days* factor(City) + Age*Height + offset(log(Days)), family=poisson, data=nasal_spray)
summary(fit1.1)

# fit2 <- update(fit1, ~. + I(Days^2) + I(Days^3))
fit2 <- update(fit1, ~. + I(Days^2) + I(Age^2) +  I(Days^3) + I(Age^3))
fit2.1 <- stepAIC(fit2)
anova(fit2.3,fit2.2, test="Chisq")
fit2.2 <- update(fit2.1, ~. -Height-Age-factor(Soy))
fit2.3 <- update(fit2.2, ~. -I(Days^2)) # 235.4
fit2.4 <- update(fit2.3, ~. -factor(Vitamin)*Height) 
fit2.5 <- update(fit2.3, ~. -Age*Height) 
fit2.6 <- update(fit2.4, ~. -Age-factor(Soy))
anova(fit2.4, fit2.3, fit2.2,fit2.1, test="Chisq")
summary(fit2.4)
summary(fit2.1)


fit3 <- glm(NumInf ~ factor(NasalSpray) + Days + I(Days^3)
            + Age + Weight + factor(Soy)+ factor(Soy)*Age + offset(log(Days)), family=poisson, data=nasal_spray)
glm.diag.plots(fit3)


# fit2 <- glm(NumInf ~ factor(NasalSpray) + Age + I(Age^2) + I(Age^3)+ Age*Height + Age*Weight 
#             + Days + I(Days^2) + factor(Gender) + Days*Height + Days*Weight
#             + Height + Weight + factor(Fruit) + I(Days^3) + factor(Fruit)*factor(NasalSpray)
#             + factor(NasalSpray)*factor(Gender) + factor(NasalSpray)*factor(Soy)  
#             + factor(Soy) + factor(Soy)*factor(Gender) + Days*Age+ offset(log(Days)), 
#             family=poisson, data=nasal_spray)
# step(fit2, scale=1,direction="backward")


# fit2.1 <- glm(NumInf ~ factor(NasalSpray) + Age + Days + I(Days^3) + factor(Soy) + factor(NasalSpray)*factor(Soy) + offset(log(Days)), 
#               family=poisson, data=nasal_spray)
# summary(fit2.1)
# AIC 241.7  phi = 125.29/121 = 1.035
# anova(fit2.1, test="Chisq")
# influence.measures(fit2.3)
# fit2.2 <- glm(NumInf ~ factor(NasalSpray) + Age + Days + I(Days^3) + factor(Soy)  + offset(log(Days)), 
#               family=poisson, data=nasal_spray)
# summary(fit2.2)
# AIC 243.16  phi = 128.73/122 = 1.055


############### Diagnostic ###############
library(boot)
glm.diag.plots(fit2.3)

plot(fit2.3, which=1)
plot(fit2.3, which=2)
plot(fit2.3, which=3)
plot(fit2.3, which=4)

# partial residuals
resi <- residuals(fit2.3,type="partial")
dim(resi)

par(mfrow=c(1,3)) 
plot(y=resi[,5],x = I(Age^2), main="Partial residual vs Age^2")
lines(smooth.spline(Age^2,resi[,5]),col="red")
plot(y=resi[,6],x = I(Days^3),main="Partial residual vs Days^3")
lines(smooth.spline(Days^3,resi[,6]),col="red")
plot(y=resi[,7],x = I(Age^3),,main="Partial residual vs Age^3")
lines(smooth.spline(I(Age^3),resi[,7]),col="red")


fit2.3.1 <- update(fit2.3, ~. +I(Age*Height)^2) 

resis <- residuals(fit2.3)
plot(y=resis,x = Days^2)
plot(y=resis,x = Days^3)
plot(y=resis,x = Age^2)

par(mfrow=c(1,3)) 
plot(residuals.glm(fit2.3,type=c("pearson")),ylab="pearson residual", main="pearson residual")
plot(residuals.glm(fit2.3,type=c("deviance")),ylab="deviance residual",main="deviance residual") 
plot(residuals.glm(fit2.3,type=c("response")),ylab="response residual",main="response residual") 
plot(residuals.glm(fit2.3,type=c("working")),ylab="working residual",main="working residual") 

rp     <- residuals(fit2.3, type = "pearson")
rd     <- residuals(fit2.3, type = "deviance")
rraw   <- residuals(fit2.3, type = "response") 
rw     <- residuals(fit2.3, type = "working")

fv <- fitted(fit2.3)
pre <- fit2.3$linear.predictors
par(mfrow = c(1, 3))  
### fitted value vs pearson residual
plot(log(fv), rp, xlab = "fitted values", 
      ylab = "Pearson Resids", main="fitted values vs Pearson residuals")
plot(log(fv),rd, xlab = "fitted values",
     ylab = "Deviance Resids" ,main="fitted values vs Deviance residuals")
plot(log(fv), log(rraw^2), xlab = "fitted values", 
     ylab = "Response Resids",main="fitted values vs Response residuals")

# Dispersion
rd <- residuals(fit,2.3,type="deviance")
phihat<-sum(rd^2)/fit2.3$df.res
phihat


# Leverage
par(mfrow=c(1,2))
h = hatvalues(fit2.3) 
cd = cooks.distance(fit2.3) 
plot(h/(1-h),cd, xlim=c(0,7),ylim=c(0, 0.4),xlab="Leverage",ylab="Cook statistic",main = "Leverages vs Cook's statistic")
identify(h/(1-h),cd,n=2)

influencePlot(fit2.3,main = "Influential points")

# without outliers
fit2.3.1 <- update(fit2.3, ~. -c(38,115)) # 240.24

# prediction boxplot

predict.glm(fit2.5, newdata=data.frame(NasalSpray=="2"),type="response", se.fit=TRUE)

par(mfrow = c(2, 2)) 
boxplot(exp(fit2.3$linear.predictors)[NasalSpray=="1"],exp(fit2.3$linear.predictors)[NasalSpray=="2"],
        ylim = c(0, 3), names=c("antiviral nasal spray","Placebo"),main="Nasal Spray")
boxplot(exp(fit2.3$linear.predictors)[Soy=="1"],exp(fit2.3$linear.predictors)[Soy=="2"],
        ylim = c(0, 3), names=c("Drink Soymilk","Don't Drink Soymilk"),main="Soy milk")
boxplot(exp(fit2.3$linear.predictors)[Vitamin=="1"],exp(fit2.3$linear.predictors)[Vitamin=="2"],
        ylim = c(0, 3), names=c("Vitamin","No Vitamin"),main="Vitamin")
boxplot(exp(fit2.3$linear.predictors)[City=="1"],exp(fit2.3$linear.predictors)[City=="2"],
        exp(fit2.3$linear.predictors)[City=="3"],exp(fit2.3$linear.predictors)[City=="4"],
        ylim = c(0, 3), names=c("City 1","City 2","City 3","City 4"),main="City")

# boxplot(exp(fit2.3$linear.predictors)[Gender=="1"],exp(fit2.3$linear.predictors)[Gender=="2"],
#         ylim = c(0, 3), names=c("Male","Female"),main="Gender")
# boxplot(exp(fit2.3$linear.predictors)[Fruit=="1"],exp(fit2.3$linear.predictors)[Fruit=="2"],
#        ylim = c(0, 3), names=c("Fruit","No Fruit"),main="Fruit")


