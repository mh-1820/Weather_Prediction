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
logitmod <- glm(Condition ~Rain, family = "binomial", data=up_train)

summary(logitmod)
pred <- predict(logitmod, newdata = testData, type = "response")
pred
# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Condition

# Accuracy
mean(y_pred == y_act) 
table(y_pred,y_act)
scatter.smooth(y_pred~y_act)
new<-data.frame("Rain"=0.2)
spred<-predict(logitmod,new,type="response")
spred
if (spred>0.5) { 
  print('Sunny')
} else  {
  print('Rainy')
}
