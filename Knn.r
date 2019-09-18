df <- read.csv("Istanbul Weather Data.csv")##load data
head(df) ## see the studcture
p<-as.data.frame(df)
str(p)
class(p)
p <- p[,-1]
p

p$AvgWind<-as.numeric(p$AvgWind)
p$MoonRise<-as.numeric(p$MoonRise)
p$MoonSet<-as.numeric(p$MoonSet)
p$SunRise<-as.numeric(p$SunRise)
p$SunSet<-as.numeric(p$SunSet)

p$Rain<-as.numeric(p$Rain)
p$MinTemp<-as.numeric(p$MinTemp)
p$AvgHumidity<-as.numeric(p$AvgHumidity)
p$MaxTemp<-as.numeric(p$MaxTemp)
p$AvgPressure<-as.numeric(p$AvgPressure)

ran <- sample(1:nrow(p), 0.9 * nrow(p)) 
ran
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 10 coulumns of dataset because they are the predictors
df_norm <- as.data.frame(lapply(p[,c(2,3,4,5,6,7,8,9,10,11)], nor))
df_norm
summary(df_norm)
df_train <- df_norm[ran,] 
df_train
##extract testing set
df_test <- df_norm[-ran,]
df_test
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
df_target_category <- p[ran,1]
df_target_category
##extract 5th column if test dataset to measure the accuracy
df_test_category <- p[-ran,1]
df_test_category##load the package class
library(class)
##run knn function
pr <- knn(df_train,df_test,cl=df_target_category,k=6)
pr
##create confusion matrix
tab <- table(pr,df_test_category)
tab
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
pr==df_test_category
mean(pr==df_test_category)
