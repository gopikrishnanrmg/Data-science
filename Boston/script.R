setwd("~/Boston")
dataset<-read.csv("train.csv",stringsAsFactors = TRUE)
test<-read.csv("test.csv",stringsAsFactors = TRUE)
set.seed(12)
is.null(dataset)
summary(dataset)
data<-dataset[,2:ncol(dataset)]
minvalue<-apply(data,2,min)
maxvalue<-apply(data,2,max)
scaled<-as.data.frame(scale(data,center=minvalue,scale = maxvalue-minvalue))
index<-sample(1:nrow(scaled),0.7*nrow(scaled))
train<-scaled[index,]
valid<-scaled[-index,]
library(neuralnet)
n<-names(train)
f<-as.formula(paste("medv~",paste(n[!n %in% "medv"],collapse=" + ")))
nm<-neuralnet(f,data=train,hidden = c(3,5),linear.output = T)
result<-compute(nm,valid[1:13])
predicted<-result$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
valid<-valid$medv*(max(data$medv)-min(data$medv))+min(data$medv)
plot(predicted,valid)
mse<-sum((predicted-valid)^2)/NROW(valid)
mse

resultoftest<-compute(nm,test[2:ncol(test)])
unscaledvalue<-resultoftest$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
answer<-cbind(test$ID,unscaledvalue)
write.csv(answer,"answer.csv",row.names=F)