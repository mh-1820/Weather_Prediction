p<-read.csv("Istanbul Weather Data.csv")
p
summary(p)
str(p)

# remove id column
p <- p[,-1]
p
p$Condition<-as.numeric(p$Condition)
str(p)
set.seed(1000)
d<-sample(1:nrow(p),3000)
d
tr<-p[d,]#model training data
tr
te<-p[-d,]
# build model on train data
pred<-lm(Condition~SunSet,data=tr)
scatter.smooth(p$Condition~p$MaxTemp)
library(scatterplot3d)
x<-p$Condition
y<-p$Rain
z<-p$MaxTemp
scatterplot3d(x, y, z, pch = 16)
distpred<-predict(pred,te[-1])
str(distpred)
summary(pred)#model summary actual and predicted comparison
AIC(pred)#calculate akaike information criterion
BIC(pred)
e<-table(te[,1],distpred)
e

new<-data.frame("Rain"=26.4)
distpred1<-predict(pred,new)
distpred1
if (distpred1>0&distpred1<12) { 
  print('Rain')
} else if (distpred1>11&distpred1<19) {
  print('Moderate')
} else if  (distpred1==19) {
  print('Overcast')
} else if  (distpred1==24) {
  print('Sunny')
} else {
 print('patchy light drizzle')
}
distpred<-as.integer(distpred)
distpred==te[,1]
mean(distpred==te[,1])

ap<-data.frame(cbind(actuals=te$Condition,predicteds=distpred))#making actual and predicted dataframe i.e 
ap
correlationa_accuracy<-cor(ap)#correlation accuracy
a<-as.array(correlationa_accuracy)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(a)

