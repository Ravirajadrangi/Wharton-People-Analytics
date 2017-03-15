##K means

data<- read.csv("Whartonkmeans.csv",header= T)
data$X17accept.size.ratio<-NULL
data$X16accept.size.ratio<-NULL

colmostbold =c(1,6,8,12,13,14,16,18,19,20,21,23,24)
colsreduced =c(1,8,12,13,14,16,19,20,21,23,24)
colssparse =c(1,12,13,14,16,21)
data<- data[colssparse]

library(dplyr)

grpdata <- group_by(data,Unid)
str(data)
uniqdata<- unique(grpdata)
uniqdata$tier <- NULL
View(grpdata)
data<-uniqdata
#Remove Predictors with zero variance
data <- data[sapply(data, function(x)length(levels(factor(x,exclude = NA)))>1)]
data <- scale(data)
dim(data)

require(caret)
tier_dummy <- data

dmy <- dummyVars(~.,tier_dummy)
tier_dummy <- predict(dmy,newdata = tier_dummy)

tier_dummy <- data.frame(tier_dummy)
tier_dummy[is.na(tier_dummy)] <- 0

#tier_dummy<- scale(tier_dummy)

#mydata <- data[,c(8,9,10)]
wss <- (nrow(tier_dummy)-1)*sum(apply(tier_dummy,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(tier_dummy,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

#mydata<- na.omit(data)
#mydata<- scale(mydata)
#mydata$Sizenum <- as.numeric(mydata$Sizenum)
fit <- kmeans(tier_dummy, 4,nstart = 20)
fit$cluster
resultssparse <- (fit$centers)
resultssparse <- data.frame(resultssparse)
View(resultsmostbold)

write.csv(resultssparse,file="Whartonkmsparse.csv",row.names = FALSE)
x<-table(fit$cluster)#,data$tier)
addmargins(x)
kmeanstier<- fit$cluster
results <- NULL


results$reduced <- fit$cluster
results$mostbold <- fit$cluster
results$sparse <-fit$cluster

results <- data.frame(results)
results$allbold <- NULL
data$newtier <- as.factor(kmeanstier)

write.csv(data,file="Whartonkmeans.csv",row.names = FALSE)

##merge results with whartonkmeans
whdata<- read.csv("Whartonkmeans.csv",header= T)
mdata <- merge(x=whdata,y=results,by)
##DBSCAN

library(dbscan)
EPS = 7

cluster.dbscan <- dbscan(tier_dummy, eps = EPS, minPts = 30, borderPoints = T, 
                         search = "kdtree")

plot(cluster.dbscan$cluster,tier_dummy)

table(cluster.dbscan$cluster)
plot(lat ~ lng, data = data, col = cluster.dbscan$cluster + 1L, pch = 20)

summary(uniqdata$Accepted)
