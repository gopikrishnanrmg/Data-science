# data.df<-read.csv("train.csv",stringsAsFactors = TRUE)
# test.df<-read.csv("test.csv",stringsAsFactors = TRUE)
# is.null(data.df)
# summary(data.df)
# set.seed(123)
# hist(data.df$medv)
# maxvalue<-apply(data.df, 2,max)
# minvalue<-apply(data.df, 2,min)
# dataframe<-as.data.frame(scale(data.df,center = minvalue,scale = maxvalue-minvalue))
# index<-sample(1:nrow(data.df),0.7*nrow(data.df))
# train.df<-data.df[index,]
# valid.df<-data.df[-index,]
# library(neuralnet)
# 
# n<-names(train.df)
# f<-as.formula(paste("medv~",paste(n[!n %in% "medv" & !n %in% "ID"],collapse = " + ")))
# nm<-neuralnet(f,data=train.df,hidden = c(5,3) , linear.output = T)
# plot(nm)
# result.df<-compute(nm,valid.df[,1:13])
# result.df<-result.df$net.result*(max(data.df$medv)-min(data.df$medv)+min(data.df$medv))
# actualvalue<-valid.df$medv*(max(data.df$medv)-min(data.df$medv)+min(data.df$medv))
# mse<-sum(result.df-actualvalue)^2/nrow(valid.df)
# mse

library(MASS)
library(neuralnet)
set.seed(50)
data<-Boston
is.null(data)
summary(data)
minvalue<-apply(data,2,min)
maxvalue<-apply(data,2,max)
scaled<-as.data.frame(scale(data,center = minvalue,scale = maxvalue-minvalue))
index<-sample(1:nrow(data),0.7*nrow(data))
train<-scaled[index,]
test<-scaled[-index,]
n<-names(scaled)
f<-as.formula(paste("medv~",paste(n[!n %in% "medv"],collapse= " + ")))
nm<-neuralnet(f,data=train,hidden=c(3,5),linear.output=T)
predicted<-compute(nm,test[,1:13])
final<-predicted$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test$medv<-test$medv*(max(data$medv)-min(data$medv))+min(data$medv)
mse<-(final-test$medv)^2/nrow(test)
mse