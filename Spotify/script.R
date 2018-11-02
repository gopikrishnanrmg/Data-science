setwd("~/R projects/Data-science/Spotify")

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

data <- read.csv("data.csv")
packages<-c("randomForest")
check.packages(packages)


head(data)
summary(data)
data$target = as.factor(data$target)
#data$song_title<-as.character(data$song_title)
smp_size <- floor(0.75 * nrow(data))
set.seed(1)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
#form<-as.formula(target~acousticness+danceability+energy+instrumentalness+liveness+loudness)
form<-as.formula(target~(.-song_title-artist))
model <- randomForest(form, data = train, family = binomial)
predicted <- predict(model,test)
predicted
#table(test$target, predicted > -0.75)
cm<-as.matrix(table(Actual=test$target,Predicted=predicted))
sum(diag(cm))/length(test$target)