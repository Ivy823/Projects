library(ggplot2);
library(GGally);
library(data.table);
library(MASS);
library(class);
library(randomForest);
library(tree);
library(grid);
library(gridExtra);
library(ROCR);
library(dplyr)
library(tidyr)
library(ggplot2)
library(repr)
### Input Data ###
setwd("/Users/Xuanyi/Desktop/231/final project")
Pokemon <- read.csv("Pokemon.csv", header = TRUE)
colnames(Pokemon) <- c("Number", "Name", "Type1", "Type2", "Total", "HP", 
                       "Attack", "Defense", "SP.ATK", "SP.DEF", "Speed", 
                       "Generation", "Legendary")
Legendary <- Pokemon[,13]
# split into two sets
train_ind <- sample(1:nrow(Pokemon), size = 0.75*nrow(Pokemon))
train.set <- Pokemon[train_ind,c(1:13)]
test.set <- Pokemon[-train_ind,c(1:13)]

### Clustering ###

d <- dist(Pokemon[,-c(2,3,4,13)],method="euclidean")
fit <- hclust(d, method="average")
plot(fit,main="Average Linkage",col.main="Blue",xlab="average linkage",ylab="height")

pok.clust<-cutree(fit,h = 170)
table(pok.clust)
table(pok.clust, Pokemon$Type1)
pie(table(Pokemon$Type1),col=c("yellow","purple","blue"), radius=2, edges=200)


### GGplot ###
# Convert data to the long format
Generation <- Pokemon[,12]
Total <- Pokemon[,5]
Generation <- as.factor(Generation)
pkmon_long <- Pokemon %>%
  #   select(-Total) %>%
  gather(Attribute, Value, -Number, -Name, -Type1, 
         -Type2, -Legendary, -Generation, factor_key = TRUE) %>% 
  mutate(Generation) %>%
  mutate(Dual_type <- Type2 != "") 

# Label the names for details in attribute
pkmon_long$Attribute <- factor(
  pkmon_long$Attribute, 
  labels=c("Tota","Attack", "Defense", "HP", "Special\nAttack", "Special\nDefense", "Speed")
)

# Boxplot
options(repr.plot.width=6, repr.plot.height=6)

ggplot(pkmon_long) + 
  geom_boxplot(aes(x=Attribute, y=Value), lwd=0.3,
               width = 0.7, outlier.shape = 1, outlier.size=0.3) + 
  scale_x_discrete(name="")

# Scatter plots
g1 <- ggplot(Pokemon) + 
  geom_point(aes(x=Defense, y=SP.DEF, colour=Legendary), 
             alpha=0.5, position=position_jitter(width=5, height=5)) + 
  ggtitle("Defense and Special Defense") + 
  theme_classic() + 
  theme(legend.position="bottom", 
        panel.border=element_rect(linetype=1, fill=NA))

g2 <- ggplot(Pokemon) + 
  geom_point(aes(x=Attack, y=SP.ATK, colour=Legendary), 
             alpha=0.5, position=position_jitter(width=5, height=5)) + 
  ggtitle("Attack and Special Attack") + 
  theme_classic() + 
  theme(legend.position="bottom", 
        panel.border=element_rect(linetype=1, fill=NA))

grid.arrange(g1,g2,nrow=1,ncol=2, top = "Compare Denfense and Attack based on Lengendary")


Pokemon$Per_speed=(Pokemon$Speed/Pokemon$Total)*100
Pokemon$Attack_per=(Pokemon$Attack/Pokemon$Total)*100
Pokemon$SP.ATK_per=(Pokemon$SP.ATK/Pokemon$Total)*100
Pokemon$def_per=(Pokemon$Defense/Pokemon$Total)*100
Pokemon$spdef_per=(Pokemon$SP.DEF/Pokemon$Total)*100
Pokemon$hp_per=(Pokemon$HP/Pokemon$Total)*100
names(Pokemon)
# select variabbles: Type1, Per_speed, Attack_per, SP.ATK_per, def_per, spdef_per, hp_per
pkg <- Pokemon[c(3,14:19)]

g1 <- ggplot(pkg,aes(Per_speed,fill=Type1,colours=Type1))+geom_density()
g2 <- ggplot(pkg,aes(Attack_per,fill=Type1,colours=Type1))+geom_density()
g3 <- ggplot(pkg,aes(SP.ATK_per,fill=Type1,colours=Type1))+geom_density()
g4 <- ggplot(pkg,aes(def_per,fill=Type1,colours=Type1))+geom_density()
g5 <- ggplot(pkg,aes(spdef_per,fill=Type1,colours=Type1))+geom_density()
g6 <- ggplot(pkg,aes(hp_per,fill=Type1,colours=Type1))+geom_density()
grid.arrange(g1,g2,g3,g4,g5,g6,nrow=2,ncol=3, top = "Density plots of Strongest Character for each type of pokemon")


qplot(pkg$Per_speed,geom = "histogram",main = "histogram for % speed",xlab = "speed % of total")
qplot(pkg$Attack_per,geom = "histogram",main = "histogram for % attack ",xlab = "attack % of total")
qplot(pkg$SP.ATK_per,geom = "histogram",main = "histogram for % spAttack ",xlab = "spattack % of total")
qplot(pkg$spdef_per,geom = "histogram",main = "histogram for % spdefense ",xlab = "spdef % of total")
qplot(pkg$def_per,geom = "histogram",main = "histogram for % defense ",xlab = "defense % of total")
qplot(pkg$hp,geom = "histogram",main = "histogram for % hp ",xlab = "hp % of total")

### EDA ###
g <- ggpairs(Pokemon, mapping = aes(color = Type1), 
             columns = 1:6)
g




### LDA ### 




### PCA ### 
Type1ID <- sapply(Pokemon[,3], as.numeric)
features <- Pokemon[,5:12]
targets  <- Type1ID
# Normalizing dataset with standard scaler. Mean of transformed features will be close to zero, 
# and standard deviation also very close to 1.
#feature_n <- data.frame(feature=rnorm(800,0,1))
#scaled.fea <- scale(feature_n)
#P_pca <- prcomp(scaled.fea)

centr <- apply(features,2,median)
disp <- apply(features,2,mad)
scaled_feature <- scale(features,
                                center=centr,scale=disp)
P_pca <- prcomp(scaled_feature, cor=F)
summary(P_pca)
P_pca$rotation=-P_pca$rotation
P_pca$x=-P_pca$x
biplot (P_pca , scale =0)
               
# autoplot(P_pca, colour='Generation')



### KNN ### 
# K=3
pc.comp <- P_pca$x[,1:2]
km.cl.3 <- kmeans(scaled_feature,centers=3)
pc.comp3 <- as.data.frame(cbind(pc.comp,km.cl.3$cluster)) 
centroids.3 <- aggregate(.~km.cl.3$cluster, pc.comp3,mean)
km.3 <- qplot(x=pc.comp3$PC1,y=pc.comp3$PC2,
              main="K Means 3 Clusters", xlab="PC1", ylab= "PC2") +
  geom_point(aes(color=factor(pc.comp3$V3)),size=1) + 
  geom_point(data=centroids.3,aes(x=centroids.3[,2], y=centroids.3[,3],
  color=factor(centroids.3[,1]), size=5))

# K=4
km.cl.4 <- kmeans(scaled_feature,centers=4)
pc.comp4 <- as.data.frame(cbind(pc.comp,km.cl.4$cluster)) 
centroids.4 <- aggregate(.~km.cl.4$cluster, pc.comp4,mean)
km.4 <- qplot(x=pc.comp4$PC1,y=pc.comp4$PC2,
              main="K Means 4 Clusters", xlab="PC1", ylab= "PC2") +
  geom_point(aes(color=factor(pc.comp4$V3)),size=1) + 
  geom_point(data=centroids.4,aes(x=centroids.4[,2],y=centroids.4[,3],   
                                  color=factor(centroids.4[,1]), size=5))

# K=5
km.cl.5 <- kmeans(scaled_feature,centers=5)
pc.comp5 <- as.data.frame(cbind(pc.comp,km.cl.5$cluster)) 
centroids.5 <- aggregate(.~km.cl.5$cluster, pc.comp5,mean)
km.5 <- qplot(x=pc.comp5$PC1,y=pc.comp5$PC2,
              main="K Means 5 Clusters",xlab="PC1",ylab="PC2") +
  geom_point(aes(color=factor(pc.comp5$V3)),size=1) + 
  geom_point(data=centroids.5,aes(x=centroids.5[,2],y=centroids.5[,3],
     color=factor(centroids.5[,1]), size=5))

grid.arrange(km.3,km.4,km.5,ncol=2)

# test error
feature.training <- sample(nrow(scaled_feature), floor(nrow(scaled_feature)*0.80))
feature.training.predictor <- scaled_feature[feature.training,list(Total, HP, 
                                                                   Attack, Defense, 
                                                                   SP.ATK, SP.DEF, 
                                                                   Speed, Generation)]
feature.training.response <- scaled_feature[feature.training,Type1ID]
feature.train <- as.data.table(cbind(feature.training.predictor,
                                     feature.training.response))
feature.test.predictor <- scaled_feature[-feature.training,list(Total, HP, 
                                                                Attack, Defense, 
                                                                SP.ATK, SP.DEF, 
                                                                Speed, Generation)]
feature.test.response <- scaled_feature[-feature.training,Type1ID]




pca_fit <- princomp(scaled_feature, cor=F)
X <- pca_fit$scores[1:800,1:2]
plot(X,main="First two components")

# 80% training
n = nrow(X)
index <- sample(n,size = floor(n*0.80), replace = F)
train.set <- X[index,]
dim(train.set)
test.set <- X[-index,]
dim(test.set)

obs_class <- Legendary[index,]
(model.knn <- knn(train=train.set,
                  test=test.set,
                  cl=obs_class,
                  k=19,
                  prob=T))
error <- table(model.knn,Type1ID[-index,]) #  Confusion matrix
(error[1,2] + error[2,1])/sum(error)  # Misclassification error for k = 19


### QDA ### 





### Random Forest ### 






### Bagging Tree ### 







### Classification Tree ### 


### Decision Tree ### 
library(dplyr)
library(rpart)

Sample <- sample_n(Pokemon,200,replace = FALSE)
LegendPok <- subset(Sample,Sample$Legendary=="True")
NoLegPok <- subset(Sample,Sample$Legendary=="False")

Sample <- sample_n(pokemon,200,replace = FALSE)
LegendPok <- subset(Sample,Sample$Legendary=="True")
NoLegPok <- subset(Sample,Sample$Legendary=="False")
modeltree <- rpart(Type1~.,data =NoLegPok[,-c(2,4,13)])
plot(modeltree,main="Desicion Tree",col.main="black")
text(modeltree, use.n=TRUE,col="red")
#prediction<-predict(modeltree,newdata=LegendPok,type="class")
#table(prediction,LegendPok$Type1)


### Principle Components ### 

