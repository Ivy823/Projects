############ Import data ###############
property <- read.table("/Users/Xuanyi/Desktop/220A/property.txt")
colnames(property) <- c("Size","Age","DC","DT","Price")
size <- c(property[,1])
age <- c(property[,2])
dc <- c(property[,3])
dt <- c(property[,4])
price <- c(property[,5])
library(car)
library(MASS)
library(boot)

############ Basic analysis ##############
summary(property)
comb_plots <-function(x){
  par(mfrow=c(2,2),lwd=0.7,
      oma=c(1.8,1,1.8,1),
      mar=c(1.8,1,1.8,1))
  boxplot(x,col="grey", main="Box-plot")       
  hist(x, breaks = 30, main="Histogram")        
  qqnorm(x)   
  qqline(x, col = "red")
  plot(density(x), main="Kernel Density")  
}
comb_plots(price)
comb_plots(size)
comb_plots(age)
comb_plots(dc)
comb_plots(dt)
plot(property)

############ Linear regression ###############
### AIC ###
fit <- lm(Price~Size+Age+DC+DT, data=property)   # AIC 773.04
step(fit,direction = "both")
model1 <- lm(Price ~ Size + Age + DT, data=property)
model001 <- lm(Price ~ Size + Age + DT+ I(Age^2) + I(Size^2)+ I(DT^2)+ I(Age^2), data=property)
model11 <- lm(Price ~ Size + Age + I(Age^2) + DT, data=property)
model12 <- lm(Price ~ Size + Age + I(Size^2) + DT, data=property)
model13 <- lm(Price ~ Size + Age + I(DT^2) + DT, data=property)
model14 <- lm(Price ~ Size + Age + I(Age^2) + DT+I(DT*Size), data=property)  # R2 = 0.5664  AIC=769.19
model11new <- lm(price ~ size + age + I(age^2) + dt, subset=-c(51))
summary(model1) # R2=53.55, p-value: 8.665e-14
summary(model11) # R2=55.89, p-value: 4.601e-14
summary(model12) # R2=53.73, p-value: 2.863e-13
summary(model13) # R2=52.96, p-value: 5.403e-13
summary(lm(sqrt(price)~size+age+I(age^2)+I(size*dt) ), data=property)

anova(fit)

fit1 <- lm(Price~Size+Age+DC+DT+I(Size^2)+I(Age^2), data=property) # AIC 768.63, R2=56.86, p-value: 1.966e-14
step(fit1,direction = "both")
model2 <- lm(Price ~ DT + I(Size^2) + I(Age^2), data=property)
model21 <- lm(Price ~ DT + Age + I(Age^2) + Size + I(Size^2), data=property)
summary(model2)
summary(model21) # R^2 = 55.85, p-value = 1.643e-13

fit2 <- lm(Price~Size+Age+DC+DT+I(DT^2)+I(DC^2), data=property)   # Bad 
step(fit2,direction = "both")

fit3 <- lm(Price~Size+Age+DC+DT+I(Size^2)+I(Size^3)+I(Age^2)+I(Age^3), data=property)   # 768.57
step(fit3,direction = "both")
model3 <- lm(Price ~ DT + I(Size^3) + I(Age^2), data=property)
model31 <-  lm(Price ~ DT + Size + I(Size^2) + I(Size^3) + Age + I(Age^2), data=property)
summary(model3)
summary(model31)  # R2 = 55.28, p-value = 8.085e-13

model3_new <- lm(Price ~ DT + I(Size^3) + I(Age^2), data=property, subset=-c(51))
summary(model3)

fit4 <- lm(Price~Size+Age+DC+DT+I(Size*DC)+I(Size*Age)+I(Size*DT)+I(Age*DC)+I(Age*DT), data=property)  # 771.34
step(fit4,direction = "both")
model4 <- lm(Price ~ Size + I(Size * Age) + I(Size * DT), data=property)
model41 <- lm(Price ~ Size + Age + DT + I(Size * Age) + I(Size * DT), data=property)
summary(model4)
summary(model41) # R2=55.38,  p-value: 1.265e-12   AIC=775.21

fit5 <- lm(Price~Size+Age+DC+DT+I(Size^2)+I(Size^3)+I(Age^2)+I(Age^3)+I(Size*DT)+I(Size*Age), data=property)   # 768.57
step(fit5,direction = "both")
model5 <- lm(Price ~ I(Size^3) + I(Age^2) + I(Size * DT), data=property)
model51 <- lm(Price ~ Size + I(Size^2) + I(Size^3) + Age + DT + I(Age^2) + I(Size * DT), data=property)
summary(model5)
summary(model51)  # R2 = 56.46, p-value: 8.647e-13   AIC=771.36

model5_new <- lm(Price ~ I(Size^3) + I(Age^2) + I(Size * DT), data=property, subset=-c(51))
summary(model5_new)

model5_new1 <- lm(Price ~ I(Size^3) + I(Age^2) + I(Size * DT), data=property, subset=-c(51,22))
summary(model5_new1)

### Plots of AIC, BIC, Cp and adjusted R2  & cross-validation ###
## Model 1
o <- regsubsets(Price~Size+Age+I(Age^2)+DT, data=property,
                method="exhaustive") 
(rso <- summary(o))

n <- nrow(property)
AIC <- n*log(rso$rss) + 2*(2:5)
BIC <- n*log(rso$rss) + log(n)*(2:5)
par(mfrow=c(2,2))
plot(2:5,AIC,xlab="Number of parameters",ylab="AIC")
plot(2:5,BIC,xlab="Number of parameters",ylab="BIC")
plot(2:5,rso$cp,xlab="Number of parameters",  # use smallest cp to select model
     ylab="Cp statistic")
abline(0,1)
plot(2:5, rso$adjr2, xlab="Number of parameters", ylab="Adjusted R-square")

# Model 2
a <- regsubsets(Price~Size+Age+DC+DT+I(Size^2)+I(Age^2), data=property,
                method="exhaustive") 
(rsa <- summary(a))

n <- nrow(property)
AIC <- n*log(rsa$rss) + 2*(2:7)
BIC <- n*log(rsa$rss) + log(n)*(2:7)
par(mfrow=c(2,2))
plot(2:7,AIC,xlab="Number of parameters",ylab="AIC")
plot(2:7,BIC,xlab="Number of parameters",ylab="BIC")
plot(2:7,rsa$cp,xlab="Number of parameters",  # use smallest cp to select model
     ylab="Cp statistic")
abline(0,1)
plot(2:7, rsa$adjr2, xlab="Number of parameters", ylab="Adjusted R-square")

## Model 3
b <- regsubsets(Price~Size+Age+DC+DT+I(Size^2)+I(Size^3)+I(Age^2)+I(Age^3), data=property,
                method="exhaustive") 
(rsb <- summary(b))
AIC <- n*log(rsb$rss) + 2*(2:9)
BIC <- n*log(rsb$rss) + log(n)*(2:9)
par(mfrow=c(2,2))
plot(2:9,AIC,xlab="Number of parameters",ylab="AIC")
plot(2:9,BIC,xlab="Number of parameters",ylab="BIC")
plot(2:9,rsb$cp,xlab="Number of parameters",  # use smallest cp to select model
     ylab="Cp statistic")
abline(0,1)
plot(2:9, rsb$adjr2, xlab="Number of parameters", ylab="Adjusted R-square")

# Model 5
c <- regsubsets(Price~Size+Age+DC+DT+I(Size^2)+I(Size^3)+I(Age^2)+I(Age^3)+I(Size*DT), data=property,
                method="exhaustive") 
(rsc <- summary(c))
AIC <- n*log(rsc$rss) + 2*(2:9)
BIC <- n*log(rsc$rss) + log(n)*(2:9)
par(mfrow=c(2,2))
plot(2:9,AIC,xlab="Number of parameters",ylab="AIC")
plot(2:9,BIC,xlab="Number of parameters",ylab="BIC")
plot(2:9,rsc$cp,xlab="Number of parameters",  # use smallest cp to select model
     ylab="Cp statistic")
abline(0,1)
plot(2:9, rsc$adjr2, xlab="Number of parameters", ylab="Adjusted R-square")


### Diagnostics ### 

# Residual Plots
residual2 <- residuals(model2)
residual3 <- rstandard(model3)
residual5 <- rstandard(model5)
residual1 <- rstandard(model11)
residual11 <- rstandard(model11new)
residual14 <- rstandard(model14)
par(mfrow=c(1,2))
qqnorm(residual5, ylab="residuals", cex=0.5, main="QQ plot of Residuals for model 5") 
qqline(residual5, col="red")  ### best
qqnorm(residual1, ylab="residuals", cex=0.5, main="QQ plot of Residuals for model 1") 
qqline(residual1, col="red")  
qqnorm(residual11, ylab="residuals", cex=0.5, main="QQ plot of Residuals for model 11 removing 51") 
qqline(residual11, col="red") 
residual51 <- rstandard(new)
qqnorm(residual14, ylab="residuals", cex=0.5, main="QQ plot of Residuals for model 1-4") 
qqline(residual14, col="red")
 
# Residuals VS fitted plots
par(mfrow=c(2,2))
plot(fitted(model11),residual1) 
title("Residuals vs Fitted Value for model 1") 
abline(h=0, col = "red")
plot(fitted(model5),residual5) 
title("Residuals vs Fitted Value for model 5") 
abline(h=0, col = "red")

# Absolute residuals VS fitted plots 
absolute1 <- abs(model11$residuals)
which.max(absolute1)     # 45
absolute5 <- abs(model5$residuals)
which.max(absolute5)    # 45

par(mfrow=c(2,1))
plot(fitted(model11), abs(residuals(model11)), xlab="Fitted",
     ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted for model 1")
plot(fitted(model5), abs(residuals(model5)), xlab="Fitted",
     ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted for model 5")

# Collinearity check
vif(model11new)
# Variance inflation factor of each variable is low and less than 5. 
# VIF also indicates that variables do not have collinearity problem

#Added variables 
# look for nonlinear pattern, outliers
par(mfrow=c(2,2))
d1 <- residuals(lm(price~age+dt+I(age^2)))
m1 <- residuals(lm(size~age+dt+I(age^2)))
plot(m1,d1,xlab="size residual",ylab="price residuals")
abline(0,coef(model11)[2])
lines(lowess(m1,d1),col="red",lty=2)
title("Added variable plot for size")

d2<-residuals(lm(price~size+dt+I(age^2)))
m2<-residuals(lm(age~size+dt+I(age^2)))
plot(m2,d2,xlab="age residual",ylab="price residuals")
abline(0,coef(model11)[3])
lines(lowess(m2,d2),col="red",lty=2)
title("Added variable plot for age")
# has collinearity 

d3<-residuals(lm(price~size+age+I(age^2)))
m3<-residuals(lm(dt~size+age+I(age^2)))
plot(m3,d3,xlab="dt residual",ylab="price residuals")
abline(0,coef(model11)[4])
lines(lowess(m3,d3),col="red",lty=2)
title("Added variable plot for dt")

d4<-residuals(lm(price~size+age+dt))
m4<-residuals(lm(I(age^2)~size+age+dt))
plot(m4,d4,xlab="dt residual",ylab="price residuals")
abline(0,coef(model11)[4])
lines(lowess(m4,d4),col="red",lty=2)
title("Added variable plot for age^2")

# Model 5
par(mfrow=c(1,3))
d1 <- residuals((lm(price~I(age^2)+I(size*dt))))
m1 <- residuals(lm(I(size^3)~I(age^2)+I(size*dt)))
plot(m1,d1,xlab="size residual",ylab="price residuals")
abline(0,coef(model5)[2])
lines(lowess(m1,d1),col="red",lty=2)
title("Added variable plot for size^3")

d2<-residuals((lm(price~I(size^3)+I(size*dt))))
m2<-residuals(lm(I(age^2)~I(size^3)+I(size*dt)))
plot(m2,d2,xlab="age residual",ylab="price residuals")
abline(0,coef(model5)[3])
lines(lowess(m2,d2),col="red",lty=2)
title("Added variable plot for age^2")
# has collinearity 

d3<-residuals(lm(price~I(age^2)+I(size^3)))
m3<-residuals(lm(I(size*dt)~I(age^2)+I(size^3)))
plot(m3,d3,xlab="dt residual",ylab="price residuals")
abline(0,coef(model5)[4])
lines(lowess(m3,d3),col="red",lty=2)
title("Added variable plot for dt*size")

# Partial residual plots  
# How the relationship between variables and respons variable price.
# If it is a line, the mode is feasible
# Model 11

par(mfrow=c(2,2))
pr.size<-residuals(model11)+coef(model11)[2]*size
plot(size,pr.size,xlab="size",ylab="partial residuals")
abline(0,coef(model11)[2])
lines(lowess(size,pr.size),col="red", lty=2)
title("Partial residual plot for size")

pr.age<-residuals(model11)+coef(model11)[3]*age 
plot(age,pr.age,xlab="size",ylab="partial residuals")
abline(0,coef(model11)[3])
lines(lowess(age,pr.age),col="red", lty=2)
title("Partial residual plot for age")

pr.age2<-residuals(model11)+coef(model11)[4]*age^2
plot(age*2,pr.age2,xlab="dt",ylab="partial residuals")
abline(0,coef(model11)[4])
lines(lowess(age^2,pr.age2),col="red", lty=2)
title("Partial residual plot for age^2")

pr.dt<-residuals(model11)+coef(model11)[4]*dt
plot(dt,pr.dt,xlab="dt",ylab="partial residuals")
abline(0,coef(model11)[4])
lines(lowess(dt,pr.dt),col="red", lty=2)
title("Partial residual plot for dt")



# Model 5
# does not make sense, if model includes higher order item, which must contains its lower order iterms
# residuals plot only can be ploted between two variables
par(mfrow=c(3,1))
pr.size3 <- residuals(model5)+coef(model5)[2]*size^3
plot(size,pr.size3,xlab="size",ylab="partial residuals")
abline(0,coef(model5)[2])
lines(lowess(size,pr.size3),col="red", lty=2)
title("Partial residual plot for size^3")

pr.age2 <- residuals(model5)+coef(model5)[3]*age^2
plot(age,pr.age2,xlab="size",ylab="partial residuals")
abline(0,coef(model5)[3])
lines(lowess(age,pr.age2),col="red", lty=2)
title("Partial residual plot for age^2")

pr.size.dt<-residuals(model3)+coef(model3)[4]*size*dt
plot(size*dt,pr.size.dt,xlab="dt",ylab="partial residuals")
abline(0,coef(model5)[4])
lines(lowess(size*dt,pr.size.dt),col="red", lty=2)
title("Partial residual plot for size*dt")


# Leverage & Influential points 
par(mfrow=c(1,2))
h2 <- hatvalues(model11)
h3 <- hatvalues(model5)
plot(h2, main="Leverages Plots for model 1") 
identify(h2, n=3)
plot(h3, main="Leverages Plots for model 5") 
identify(h3, n=3)


par(mfrow=c(2,2))
cd2 <- cooks.distance(model11)
plot(h2, cd2, main="Influential Points for model 1", xlim=c(-0.1,0.8), ylim=c(0,0.07)) 
abline(h=0, col="red")
identify(h2, cd2, n=2)   # 25 47
cd3 <- cooks.distance(model5)
plot(h3, cd3, main="Influential Points for model 5", xlim=c(0,0.65), ylim=c(0,0.07)) 
abline(h=0, col="red")
identify(h3, cd3, n=3)  # 43 77 

library(car)
influencePlot(model11)
title("Influence points for model 1")
influencePlot(model5)
title("Influence points for model 5")

glm.diag.plots<-(model51)
glm.diag.plots

# Box-cox
par(mfrow=c(1,2))
boxcox(model11, plotit=T,lamda=seq(-1.5,2,len=100)) # near to 0.5
title("Box-cox plot for model 1")
boxcox(model5, plotit=T,lamda=seq(-1.5,2,len=100)) # near to 0.6
title("Box-cox plot for model 5")



boxcox(model11new, plotit=T,lamda=seq(-.5,1.5,len=100)) # near to 0.5
title("Box-cox plot for model 11")
# When the selected ?? is zero, which corresponds to the log transformation.
fit_new <- lm(sqrt(Price)~Size+Age+DC+DT+I(Size^2)+I(Size^3)+I(Age^2)+I(Age^3)+I(Size*DT), data=property)   
step(fit_new,direction = "backward")
new <- glm(formula = sqrt(Price) ~ Size + Age + I(Age^2) + DT, 
          data = property)
summary(new)

newmodel.glm<-glm(sqrt(price) ~ size + age + I(age^2) + I(size * dt))
boxcox(new,plotit=T,lambda=seq(-2,3,len=100))
title("Box-Cox Transformation for sqrt model11")

### New model ###
# sqrt 
par(mfrow=c(2,2))
residual51 <- rstandard(new)
qqnorm(residual51, ylab="residuals", cex=1, main="QQ plot of Residuals for sqrt model") 
qqline(residual51, col="red")

plot(fitted(new),residual51) 
title("Residuals vs Fitted Value for sqrt model") 
abline(h=0, col = "red")

influencePlot(new)
title("Influence points for sqrt model")

plot(fitted(new), abs(residuals(new)), xlab="Fitted",
     ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals for sqrt model 1")


# remove 51
par(mfrow=c(2,2))
residual511 <- rstandard(model11new)
qqnorm(residual511, ylab="residuals", cex=1, main="QQ plot of Residuals for removing 51") 
qqline(residual511, col="red")

plot(fitted(model11new),residual511) 
title("Residuals vs Fitted Value for removing 51") 
abline(h=0, col = "red")

influencePlot(model11new)
title("Influence points for removing 51")

boxcox(model11new,plotit=T,lambda=seq(-.5,1.5,len=100))
title("Box-Cox Transformation for removing 51")


# LASSO
set.seed(294)
par(mfrow=c(1,2))
Y <- model.matrix(model001)[,-1]
fit.lasso <- glmnet(Y, price, lambda.min=0, nlambda=101, alpha=1)
plot(fit.lasso, xvar="lambda", xlim=c(-4,5)) 
text(-6,coef(fit.lasso)[-1,length(fit.lasso$lambda)],labels=colnames(X),cex=0.6) 
fit.lasso.cv <- cv.glmnet(Y, price, lambda.min=0, nlambda=101) 
abline(v=log(fit.lasso.cv$lambda.min), col="red")
mtext("CV estimate", side=1, at=log(fit.lasso.cv$lambda.min), cex=.6)
plot(fit.lasso.cv)


set.seed(294)
par(mfrow=c(1,2))
Y <- model.matrix(new)[,-1]
fit.lasso <- glmnet(Y, price, lambda.min=0, nlambda=101, alpha=1)
plot(fit.lasso, xvar="lambda", xlim=c(-4,5)) 
text(-6,coef(fit.lasso)[-1,length(fit.lasso$lambda)],labels=colnames(X),cex=0.6) 
fit.lasso.cv <- cv.glmnet(Y, price, lambda.min=0, nlambda=101) 
abline(v=log(fit.lasso.cv$lambda.min), col="red")
mtext("CV estimate", side=1, at=log(fit.lasso.cv$lambda.min), cex=.6)
plot(fit.lasso.cv)


# Test for Autocorrelated Errors
durbinWatsonTest(model11)

