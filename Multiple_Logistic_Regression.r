p<-read.csv("Istanbul Weather Data.csv")
p
summary(p)
str(p)

# remove id column
p <- p[,-1]
p
str(p)
p$Condition <- ifelse(p$Condition =="Sunny", 1, 0)
p$Condition
p$Condition<- factor(p$Condition, levels = c(0, 1))
table(p$Condition)
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(p$Condition, p=0.7, list = F)  # 70% training data
trainData <- p[trainDataIndex, ]
testData <- p[-trainDataIndex, ]
table(trainData$Condition)
#up sample
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Condition)

table(up_train$Condition)
logitmod <- glm(Condition ~Rain+AvgHumidity+MaxTemp+MinTemp+AvgWind+AvgPressure, family = "binomial", data=up_train)

summary(logitmod)
spred <- predict(logitmod, newdata = testData, type = "response")
spred



# Recode factors
y_pred_num <- ifelse(spred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Condition

# Accuracy # 77.22%
y_pred == y_act
mean(y_pred == y_act)  
table(y_pred,y_act)
new<-data.frame("Rain"=0,"MaxTemp"=29,"MinTemp"=23,"AvgWind"=19,"AvgHumidity"=57,"AvgPressure"=1017)
spred1<-predict(logitmod,new,type="response")
spred1

if (spred1>0.5) { 
  print('Sunny')
} else  {
  print('Rainy')
}

