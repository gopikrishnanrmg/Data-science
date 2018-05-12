# setwd("~/R project/World happiness report")
# df16<-read.csv("2016.csv", stringsAsFactors = TRUE)
# df15<-read.csv("2015.csv", stringsAsFactors = TRUE)
# 
# sum(is.na(df16))
# sum(is.na(df15))
# 
# df16$Standard.Error=0
# df15$Lower.Confidence.Interval=0
# df15$Upper.Confidence.Interval=0
# 
# datasets<-rbind(df16,df15)
# datasets
# datasets <- datasets[sample(1:nrow(datasets)), ]
# 
# samp <- sample(1:nrow(datasets),.6*nrow(datasets))
# train.df<-datasets[samp,]
# remain40.df<-datasets[-samp,]
# samp2 <- sample(1:nrow(remain40.df),0.5*nrow(remain40.df))
# valid.df <- remain40.df[samp2,]
# test.df <- remain40.df[-samp2,]
set.seed(1)

lm1<-lm(Happiness.Score~Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity,data = train.df)
lm1#linear regression
result1<-predict(lm1,valid.df)
result1
plot(valid.df$Happiness.Score,result1)
value<-(result1-valid.df$Happiness.Score)^2
sum(value)#19.14

lm2<-randomForest::randomForest(Happiness.Score~Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity,data = train.df,importance=TRUE)
lm2#randomforest
lm2$importance#which parameter influences the most
result2<-predict(lm2,valid.df)
result2
plot(valid.df$Happiness.Score,result2)
value<-(result2-valid.df$Happiness.Score)^2
sum(value)#17.52

lm3<-party::ctree(Happiness.Score~Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity,data = train.df)
lm3#party
plot(lm3)
result3<-predict(lm3,train.df)
result3
plot(train.df$Happiness.Score,result3)
value<-(result3-train.df$Happiness.Score)^2
sum(value)#65.11

####tuning variables

lm1<-lm(Happiness.Score~(Economy..GDP.per.Capita.)^3+(Family)^2+(Health..Life.Expectancy.)^3+Freedom+Trust..Government.Corruption.+Generosity,data = train.df)
result1<-predict(lm1,valid.df)
plot(valid.df$Happiness.Score,result1)
value<-(result1-valid.df$Happiness.Score)^2
sum(value)#19.14 no change

lm2<-randomForest::randomForest(Happiness.Score~(Economy..GDP.per.Capita.)^3+(Family)^2+(Health..Life.Expectancy.)^3+Freedom+Trust..Government.Corruption.+Generosity,data = train.df,importance=TRUE)
result2<-predict(lm2,valid.df)
plot(valid.df$Happiness.Score,result2)
value<-(result2-valid.df$Happiness.Score)^2
sum(value)#18 #discard, made model worse earlier was 17

lm3<-party::ctree(Happiness.Score~(Economy..GDP.per.Capita.)^3+(Family)^2+(Health..Life.Expectancy.)^3+Freedom+Trust..Government.Corruption.+Generosity,data = train.df)
result3<-predict(lm3,train.df)
plot(train.df$Happiness.Score,result3)
value<-(result3-train.df$Happiness.Score)^2
sum(value)#65.76295




lm2<-randomForest::randomForest(Happiness.Score~Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity,data = train.df,importance=TRUE)
result2<-predict(lm2,test.df)
plot(test.df$Happiness.Score,result2)
value<-(result2-test.df$Happiness.Score)^2
sum(value)#6



