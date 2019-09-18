
#Reading data into R
nm<-read.csv("Istanbul Weather Data.csv")
nm
summary(nm)
str(nm)

# remove id column
nm <- nm[,-1]
nm
nm <- nm[,-5:-8]
nm
#Setting outcome variables as categorical
nm$Condition <- ifelse(nm$Condition =="Sunny", TRUE, FALSE)
nm$Condition
nm$Condition = factor(nm$Condition, levels = c(FALSE, TRUE)) 
nm$Condition
#Studying the structure of the data
str(nm)
head(nm)

describe(nm)
#Convert '0' values into NA
nm[, 2:4][nm[, 2:4] == 0] <- NA
#visualize the missing data
missmap(nm)
#Use mice package to predict missing values
mice_mod <- mice(nm[, c("Rain","MinTemp","MaxTemp")], method='rf')
mice_complete <- complete(mice_mod)
mice_complete
#Transfer the predicted missing values into the main data set
nm$Rain <- mice_complete$Rain
nm$Rain
nm$MinTemp <- mice_complete$MinTemp
nm$MaxTemp <- mice_complete$MaxTemp
missmap(nm)
#Data Visualization
#Visual 1
ggplot(nm, aes(Rain, colour = Condition)) +
  geom_freqpoly(binwidth = 1) + labs(title="Rain Distribution by Condition")
#visual 2
c <- ggplot(nm, aes(x=MinTemp, fill=Condition, color=Condition)) +
  geom_histogram(binwidth = 1) + labs(title="MinTemp Distribution by Condition")
c + theme_bw()
#visual 3
P <- ggplot(nm, aes(x=MaxTemp, fill=Condition, color=Condition)) +
  geom_histogram(binwidth = 1) + labs(title="MaxTemp Distribution by Condition")
P + theme_bw()
#visual 4
ggplot(nm, aes(AvgHumidity, colour = Condition)) +
  geom_freqpoly(binwidth = 1) + labs(title="AvgHumidity Distribution by Condition")
#split data into training and test data sets
indxTrain <- createDataPartition(y = nm$Condition,p = 0.8,list = FALSE)
training <- nm[indxTrain,]
training
testing <- nm[-indxTrain,]
testing
#Check dimensions of the split

prop.table(table(nm$Condition)) * 100
prop.table(table(training$Condition)) * 100
prop.table(table(testing$Condition)) * 100
#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-1]
y = training$Condition
library(e1071)
library(klaR)
library(httpuv)
model <- naiveBayes(Condition ~ ., data=training)
summary(model)
#model1 = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing[-1] )
Predict
#Get the confusion matrix to see accuracy value and other parameter values
new<-data.frame("Rain"=44,"MaxTemp"=29,"MinTemp"=23,"AvgWind"=19,"AvgHumidity"=57,"AvgPressure"=1017)
spred1<-predict(model,new )
spred1

if (spred1==TRUE) { 
  print('Sunny')
} else  {
  print('Rainy')
}
# Accuracy
l<-table(testing[,1]==Predict)
mean(testing[,1]==Predict)


































#confusionMatrix(Predict, testing$Condition )
# Confusion Matrix and Statistics
#Plot Variable performance
#X <- varImp(model)
#plot(X)

