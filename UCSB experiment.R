setwd("/Users/ivy/Desktop/230/takehome")
warpeace <- read.table("warpeace.txt", header = TRUE)
attach(warpeace)

Group <- warpeace[,2]
Peace <- warpeace[,3]
Pairpeace <- warpeace[,4]
Belief1 <- warpeace[,5]
Belief2 <- warpeace[,6]
Agreement <- warpeace[,7]
Guiltbefore <- warpeace[,8]
Guiltafter <- warpeace[,9]

# agreement 102/140, both peace is 38
# no agreement 38/140, both peace is 2
# both war 40/140


library(xlsx)
write.xlsx(warpeace, "/Users/ivy/Desktop/230/takehome/peace.xlsx")
warpeace_earn <- read.xlsx("/Users/ivy/Desktop/230/takehome/peace.xlsx",sheetName = "peace")

peace.sub <- subset(warpeace, Peace =="1") # 70
war.sub <- subset(warpeace, Peace =="0") # 70
two_peace <- subset(warpeace, Peace =="1" & Pairpeace == "1") # 40
two_war <- subset(warpeace, Peace =="0" & Pairpeace == "0") # 40
agreement <- subset(warpeace, Agreement == "1") # 102 -- 61 group
agreement_peace_both <- subset(warpeace, Agreement == "1"& Peace =="1" & Pairpeace == "1") # 38
agreement_peace <- subset(warpeace, Agreement == "1" & Peace =="1") # 60 
agreement_war <- subset(warpeace, Agreement == "1" & Peace =="0") # 42
no_agreement_peace <- subset(warpeace, Agreement == "0" & Peace =="1") # 10
no_agreement_peace_both <- subset(warpeace, Agreement == "0" & Peace =="1" & Pairpeace == "1") # 2

############# EDA #############
# Scatterplot (relationship) 
summary(warpeace)
pairs(warpeace)
plot(as.ordered(warpeace[,3]),col="grey", ylab="Frequency", xlab=peace)
par(mfrow=c(1,2),
    oma=c(1,1,1,1),
    mar=c(2,2,2,2))
plot(as.ordered(guilta),col="grey", ylab="Frequency", xlab=guilta, 
     ylim=c(0,100),main="Sense of guilty after the revelation")
plot(as.ordered(guiltb),col="grey", ylab="Frequency", 
     ylim=c(0,100),xlab=guiltb, main="Sense of guilty before the revelation")


# Boxplot (distribution, outliers, means)
par(mfrow=c(1,3),
    oma=c(1,1,1,1),
    mar=c(2,2,2,2))
boxplot(split(warpeace[,3],warpeace[,7]),
        names=c("Mutual Agreement"),main="Agreement")



segavg <- function (z,y,k) {
  y <- y[order(z)]
  z <- sort(z)
  prop <- rep(NA,k)
  q <- seq(k+1)
  d <- as.integer(length(z)/k)
  for (i in 1:(k-1)) {
    q[i] <- z[(1+(i-1)*d)]
    prop[i] <- mean(y[(1+(i-1)*d):(i*d)])
  }
  q[k] <- z[1+(k-1)*d]
  q[k+1] <- max(z)
  prop[k] <- mean(y[(1+(k-1)*d):length(y)])
  return(list("q"=q,"prop"=prop))
}
a <- segavg(Agreement,peace,2)
plot(Agreement,peace,xlab=dimnames(warpeace)[[2]][7],
     ylab="Agreement")
for (j in 1:2){
  segments( a$q[j], a$prop[j],  a$q[j+1], a$prop[j],
            lwd=4)
}

for (i in 4:9) print(table(warpeace[,3],warpeace[,i]))



######### Regression ###########
# response variable: peace

# agreement
fit <- glm(factor(Peace) ~ factor(Agreement), family=binomial, data=warpeace)
summary(fit)


agreement1 <- cbind(expand.grid(Peace=c("1","0"),
                            Agreement=c("1","0")),
                     ct=c(60,42,10,28))
attach(agreement1)
fit1.1 <- glm(ct ~ Agreement*Peace, family=poisson, data=agreement1)


chisq.test(agreement1)


Peace1 <- agreement1[1,]
Agreement1 <- agreement[2,]
ct <- agreement[3,]

agreelogit <- cbind(agreement1[Peace1 == "1",],
                    N = agreement1[Peace1 =="0","ct"])
attach(agreelogit)
fit2.1 <- glm(cbind(N,ct) ~ Agreement, family=binomial, data=agreelogit)


# belief1
fit1 <- glm(Peace ~ factor(belief1) + factor(belief2), family=binomial, data=warpeace)
summary(fit1)
fit1.1 <- glm(Peace ~ factor(belief1) + factor(belief2) + factor(belief1)*factor(belief2) , family=binomial, data=warpeace)
summary(fit1.1)
anova(fit1,fit1.1,test="Chisq") # 0.623 no interaction

# Both
fit2 <- glm(Peace ~ factor(Agreement) + factor(belief1) + factor(belief2), family=binomial, data=warpeace)
summary(fit2)
fit2.1 <- glm(Peace ~ factor(Agreement) + factor(belief1) + factor(belief2), family=binomial, data=warpeace)
summary(fit2.1)
anova(fit2.2, fit2,test="Chisq")

# guilt & war
fit3 <- glm(guilta ~ factor(Peace), family=poisson, data=warpeace)
summary(fit3)
fit3.1 <- glm(guiltb ~ factor(Peace), family=poisson, data=warpeace)
summary(fit3.1)

# guilt & agreement
fit4 <- glm(guilta ~ factor(Agreement), family=poisson, data=warpeace)
summary(fit4)
fit4.1 <- glm(guiltb ~ factor(Agreement), family=poisson, data=warpeace)
summary(fit4.1)

# guilt & pairpeace
fit5 <- glm(guilta ~ factor(Pairpeace), family=poisson, data=warpeace)
summary(fit5)
fit5.1 <- glm(guiltb ~ factor(Pairpeace), family=poisson, data=warpeace)
summary(fit5.1)

fit6 <- glm(guilta ~ factor(Peace) + factor(Agreement) + factor(Pairpeace), family=poisson, data=warpeace)
summary(fit6)
fit6.1 <- glm(guiltb ~ factor(Peace) + factor(Agreement) + factor(Pairpeace), family=poisson, data=warpeace)
summary(fit6.1)

anova(fit6, fit5, fit4, fit3,test="Chisq")

fit7 <- glm(guilta ~ factor(Agreement) + factor(Pairpeace) + factor(Agreement)*factor(Pairpeace), family=poisson, data=warpeace)
summary(fit7)
fit7.1 <- glm(guilta ~ factor(Agreement) + factor(Pairpeace), family=poisson, data=warpeace)
summary(fit7.1)
anova(fit7.1, fit7, test="Chisq") # no interaction

fit7.2 <- glm(guiltb ~ factor(Agreement) + factor(Pairpeace)+ factor(Agreement)*factor(Pairpeace), family=poisson, data=warpeace)
summary(fit7.2)
fit7.3 <- glm(guiltb ~ factor(Agreement) + factor(Pairpeace), family=poisson, data=warpeace)
summary(fit7.3)
anova(fit7.3, fit7.2, test="Chisq") # no interaction

interaction.plot(Agreement,Pairpeace,guilta)

########### Diagonistic ###########

par(mfrow=c(1,2),
    oma=c(1,1,1,1),
    mar=c(2,2,2,2))
boxplot(guilta[Agreement=="1"],guilta[Agreement=="0"],
        names=c("Agreement","No Agreement"),main="Guilt feeling after revelation")
boxplot(guiltb[Agreement=="1"],guiltb[Agreement=="0"],
        names=c("Agreement","No agreement"),main="Guilt feeling before revelation")

par(mfrow=c(1,2),
    oma=c(1,1,1,1),
    mar=c(2,2,2,2))
boxplot(guilta[Peace=="1"],guilta[Peace=="0"],
        names=c("Peace","War"),main="Guilt feeling after revelation")
boxplot(guiltb[Peace=="1"],guiltb[Peace=="0"],
        names=c("Peace","War"),main="Guilt feeling before revelation")

par(mfrow=c(1,2),
    oma=c(1,1,1,1),
    mar=c(2,2,2,2))
boxplot(guilta[Pairpeace=="1"],guilta[Pairpeace=="0"],
        names=c("Peace","War"),main="Guilt feeling after revelation")
boxplot(guiltb[Pairpeace=="1"],guiltb[Pairpeace=="0"],
        names=c("Peace","War"),main="Guilt feeling before revelation")

# Residual plots
glm.diag.plots(fit2)


# Dispersion
rd <- residuals(fit7.3,type="deviance")
phihat<-sum(rd^2)/fit7.3$df.res
phihat

rd <- residuals(fit7,type="pearson")
phihat<-sum(rd^2)/fit7$df.res
phihat


