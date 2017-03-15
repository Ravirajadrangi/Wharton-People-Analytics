#setwd(path)
library(xlsx)
library(data.table)
data <- read.xlsx(file="DataSet_final.xlsx", sheetName = "2016 Undergrad Apps EDL to 5DL")

library("openxlsx")
mydf <- read.xlsx("DataSet_final.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
data2016<-mydf

mydf1 <- read.xlsx("DataSet_final.xlsx", sheet = 2, startRow = 1, colNames = TRUE)
data2017<-mydf1
str(mydf)

numcols =seq(from=1,to=14)

for( i in numcols)
{
    data2016[,i] <- as.factor(data2016[,i])
  
}

for( i in numcols)
{
  data2017[,i] <- as.factor(data2017[,i])
  
}
colnames(data2016)<- c("app_id","prospect_type","decision","app_year","app_deadline","Unid","ratings","tier","major1","major2","minor","gpa36","metrt","srcrt")
colnames(data2017)<- c("app_id","prospect_type","decision","app_year","app_deadline","Unid","ratings","tier","major1","major2","minor","gpa36","metrt","srcrt")


library(ggplot2)
library(plotly)
require(scales)

ggplot(data2016,aes(x=prospect_type,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
ggplot(data2017,aes(x=prospect_type,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)


ggplot(data2016,aes(x=app_year,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
p=ggplot(data2017,aes(x=app_year,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)
ggplotly(p)

p=ggplot(data2017,aes(x=srcrt,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~tier)+scale_y_continuous(labels = percent)
ggplotly(p)

##Not much difference between decision for self sign up and src rt
p=ggplot(data2017,aes(x=srcrt,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)
ggplotly(p)
ggplot(data2016,aes(x=srcrt,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)


ggplot(data2016,aes(x=metrt,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)

ggplot(data2017,aes(x=metrt,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)

ggplot(data2016,aes(x=ratings,fill=tier)) +geom_bar(width=0.7)+facet_wrap(~gpa36+decision)


## Majority applicants from lower ratings are reject cases
ggplot(data2017,aes(x=gpa36,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)

ggplot(data2016,aes(x=gpa36,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)



ggplot(data2017,aes(x=tier,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)

ggplot(data2016,aes(x=tier,fill=decision)) +geom_bar(width=0.7)+facet_wrap(~ratings)


unidata <- read.xlsx("DataSet_final.xlsx", sheet = 3, startRow = 1, colNames = TRUE)

unidata$`2017.Recruitment.Tier`<- as.factor(unidata$`2017.Recruitment.Tier`)
unidata$`2016.Recruitment.Tier`<- as.factor(unidata$`2016.Recruitment.Tier`)
unidata$`Public/Private`<- as.factor(unidata$`Public/Private`)
unidata$US.World.News.Selectivity<-as.factor(unidata$US.World.News.Selectivity)
unidata$Size <-as.factor(unidata$Size)

colnames(unidata) = c("UID","Tier17","Tier16","PubPri","size","ratings","regions","alumni","cm","staff","awareness")


##Good Insights 
ggplot(unidata,aes(x=size,fill=PubPri))+geom_bar(width=0.7)+facet_wrap(~Tier17)
ggplot(unidata,aes(x=size,fill=PubPri))+geom_bar(width=0.7)+facet_wrap(~Tier16)


ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")


ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity")


##high number of alumni in most selective
ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity")+facet_wrap(~ratings)
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")+facet_wrap(~ratings)


##Location is important
ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity")+facet_wrap(~regions)
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")+facet_wrap(~regions)


ggplot(unidata,aes(x=Tier16,y=cm))+geom_bar(stat="identity")+facet_wrap(~ratings)
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity")+facet_wrap(~ratings)

ggplot(unidata,aes(x=Tier16,y=staff))+geom_bar(stat="identity")
ggplot(unidata,aes(x=Tier17,y=staff))+geom_bar(stat="identity")


unidata$awareness <-as.factor(unidata$awareness)
ggplot(unidata,aes(x=Tier16,y=alumni))+geom_bar(stat="identity") +facet_wrap(~awareness)
ggplot(unidata,aes(x=Tier17,y=alumni))+geom_bar(stat="identity") +facet_wrap(~awareness)


ggplot(unidata,aes(x=staff))+geom_bar(stat="identity")+facet_wrap(~as.factor(awareness))


