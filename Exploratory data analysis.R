install.packages("plotly")
library(plotly)
install.packages("scales")
library(scales)
library("openxlsx")
install.packages("corrplot")
library(corrplot)
library(caret)
data2016 <- read.xlsx("DataSet_final_orig.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
data2017 <- read.xlsx("DataSet_final_orig.xlsx", sheet = 2, startRow = 1, colNames = TRUE)

colnames(data2016)<- c("app_id","prospect_type","decision","app_year","app_deadline","Unid","ratings","tier","major1","major2","minor","gpa36","metrt","srcrt")
colnames(data2017)<- c("app_id","prospect_type","decision","app_year","app_deadline","Unid","ratings","tier","major1","major2","minor","gpa36","metrt","srcrt")

data2016$metrt =ifelse(is.na(data2016$metrt),"N",data2016$metrt)
data2017$metrt =ifelse(is.na(data2017$metrt),"N",data2017$metrt)

str(data2016)
numcols =seq(from=1,to=14)

for( i in numcols)
{
  data2016[,i] <- as.factor(data2016[,i])
  
}

for( i in numcols)
{
  data2017[,i] <- as.factor(data2017[,i])
  
}

library(ggplot2)
library(plotly)

ggplot(data2016,aes(x=prospect_type,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
ggplot(data2017,aes(x=prospect_type,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)

table(data2016$prospect_type,data2016$app_deadline)
table(data2017$prospect_type,data2017$app_deadline)
#all juniors apply Jan-Mar of previous year. Seniors apply from Aug the previous year until Jan of that year.

ggplot(data2016,aes(x=app_year,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
p=ggplot(data2017,aes(x=app_year,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
ggplotly(p)


p=ggplot(data2017,aes(x=srcrt,fill=decision)) +geom_bar(width=0.7)#+facet_wrap(~tier)
ggplotly(p)

##Not much difference between decision for self sign up and src rt
p=ggplot(data2017,aes(x=srcrt,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)
ggplotly(p)
ggplot(data2016,aes(x=gpa36,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
#GPA is an important variable, irrespective of tier.

ggplot(data2016,aes(x=metrt,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)

ggplot(data2017,aes(x=metrt,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)

ggplot(data2016,aes(x=ratings,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)

prop.table(table(data2016$decision,data2016$metrt),margin = 2)
prop.table(table(data2017$decision,data2017$metrt),margin = 2)

ggplot(data2017,aes(x=metrt,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
ggplot(data2016,aes(x=metrt,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
#As you go lower the tier, if met with rep, chances of acceptance significantly improve. 

## Majority applicants from lower ratings are reject cases
ggplot(data2017,aes(x=gpa36,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)
#As you go higher the selective, if gpa high, chances of acceptance significantly improve. 
ggplot(data2016,aes(x=gpa36,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)

# #As you go higher the selective, if campus tier is high, chances of acceptance significantly improve. 
# ggplot(data2017,aes(x=tier,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)

prop.table(table(data2017$ratings,data2017$tier),1)
prop.table(table(data2016$ratings,data2016$tier),1)
#selectivity is highly correlated with tier 

# ggplot(data2016,aes(x=tier,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)


unidata <- read.xlsx("DataSet_final_orig.xlsx", sheet = 3, startRow = 1, colNames = TRUE)

unidata$`2017.Recruitment.Tier`<- as.factor(unidata$`2017.Recruitment.Tier`)
unidata$`2016.Recruitment.Tier`<- as.factor(unidata$`2016.Recruitment.Tier`)
unidata$`Public/Private`<- as.factor(unidata$`Public/Private`)
unidata$US.World.News.Selectivity<-as.factor(unidata$US.World.News.Selectivity)
unidata$Size <-as.factor(unidata$Size)

colnames(unidata) = c("UID","Tier17","Tier16","PubPri","size","ratings","regions","alumni","cm","staff","awareness")

View(unidata)
hist(unidata$cm,breaks = 100,xlim = c(0,40))
table(unidata$cm)
unidata$cmbin = cut(unidata$cm,breaks = c(-1,10,20,100,Inf),labels = c("vl","l","m","h"))

hist(unidata$alumni,breaks = 100,xlim = c(0,400))
unidata$alum_bin = cut(unidata$alumni,breaks = c(-1,50,200,400,Inf),labels = c("vl","l","m","h"))
##Good Insights 
ggplot(unidata,aes(x=size,fill=cmbin))+geom_bar(width=0.7)#+facet_wrap(~Tier17)
ggplot(unidata,aes(x=size,fill=cmbin))+geom_bar(width=0.7)+facet_wrap(~Tier16)

ggplot(unidata,aes(x=size,fill=alum_bin))+geom_bar(width=0.7)#+facet_wrap(~Tier17)
ggplot(unidata,aes(x=size,fill=cmbin))+geom_bar(width=0.7)+facet_wrap(~Tier16)

cor(unidata$cm,unidata$alumni)
cor(unidata$alumni,unidata$staff)
#CM, staff and Alumni are very highly correlated. Use Alumni.
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")
ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity")

ggplot(unidata,aes(x=regions,fill=alum_bin))+geom_bar(width=0.7)#+facet_wrap(~regions)
prop.table(table(unidata$regions,unidata$alum_bin),1)
temp = unidata
temp$regions <- as.factor(temp$regions)
levels(temp$regions)[[4]] <- "S"
levels(temp$regions)

unidata$regions <- as.factor(unidata$regions)
levels(unidata$regions)[[4]] <- "S"
levels(unidata$regions)
#Higher the tier, more the average alumni
tapply(unidata$alumni,INDEX = unidata$Tier17,mean)

##high number of alumni in most selective
# ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity")+facet_wrap(~ratings)
# ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")+facet_wrap(~ratings)


##Location is important
ggplot(unidata,aes(x=Tier16,fill = alum_bin))+geom_bar(stat="identity")+facet_wrap(~regions)
ggplot(unidata,aes(x=Tier17,fill=alum_bin))+geom_bar(stat="identity")+facet_wrap(~regions)

prop.table(table(unidata$regions,unidata$alum_bin),1)
prop.table(s,1)

ggplot(unidata,aes(x=Tier16,y=cm))+geom_bar(stat="identity")+facet_wrap(~ratings)
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")+facet_wrap(~ratings)

ggplot(unidata,aes(x=Tier16,y=staff))+geom_bar(stat="identity")
ggplot(unidata,aes(x=Tier17,y=staff))+geom_bar(stat="identity")


unidata$awareness <-as.factor(unidata$awareness)
ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity") +facet_wrap(~awareness)
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity") +facet_wrap(~awareness)


ggplot(unidata,aes(x=staff))+geom_bar(stat="identity")+facet_wrap(~as.factor(awareness))

