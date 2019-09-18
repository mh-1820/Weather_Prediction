library('Metrics')

library('randomForest')

library('ggplot2')

library('ggthemes')

library('dplyr')

#set random seed

set.seed(100)

#loading dataset

data<-read.csv("Istanbul Weather Data.csv",stringsAsFactors= T)
data=data[,-1]
data
str(data)
#checking dimensions of data

dim(data)
data$Condition <- ifelse(data$Condition =="Sunny", 1, 0)
data$Condition
data$Condition<- factor(data$Condition, levels = c(0, 1))
table(data$Condition)
data$AvgWind<-as.integer(data$AvgWind)
data$MoonRise<-as.integer(data$MoonRise)
data$MoonSet<-as.integer(data$MoonSet)
data$SunRise<-as.integer(data$SunRise)
data$SunSet<-as.integer(data$SunSet)

data$Rain<-as.integer(data$Rain)
data$MinTemp<-as.integer(data$MinTemp)
data$AvgHumidity<-as.integer(data$AvgHumidity)
data$MaxTemp<-as.integer(data$MaxTemp)
data$AvgPressure<-as.integer(data$AvgPressure)


#dividing the dataset into train and test
train<-data[1:3000,]

test<-data[3001:3854,]
#applying Random Forest

model_rf<-randomForest(Condition ~., data = train)

model_rf

preds<-predict(model_rf,test[,-1])
preds = predict( model_rf,newdata = test[,-1], type ='class')
new<-data.frame("Rain"= 0,"MaxTemp"= 29,"MinTemp"= 23,"AvgWind"= 19,"AvgHumidity"= 57,"AvgPressure"=1017,"SunRise"=20,"SunSet"=175,"MoonRise"=934,"MoonSet"=165)
spred3<-predict(model_rf,new )
spred3

if (spred3==1) { 
  print('Sunny')
} else  {
  print('Rainy')
}


table(preds)

#checking accuracy

auc(preds,test$Condition)


str(model_rf)
importance(model_rf)

