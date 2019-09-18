p<-read.csv("Istanbul Weather Data.csv")
p
summary(p)
str(p)

# remove id column
p <- p[,-1]
p
#p <- p[,-7:-8]
p
p$Condition<-as.numeric(p$Condition)
str(p)
set.seed(1000)
d<-sample(1:nrow(p),3000)
d
tr<-p[d,]#model training data
tr
te<-p[-d,]
te
#prediction
# build model on train data
pred<-lm(Condition~MaxTemp+MinTemp+AvgWind+AvgHumidity+AvgPressure+Rain,data=tr)
distpred<-predict(pred,te[-1])#predict distance
distpred
summary(pred)#model summary actual and predicted comparison
AIC(pred)#calculate akaike information criterion
BIC(pred)
e<-table(te[,1],distpred)
e
#accuracy
ap<-data.frame(cbind(actuals=te$Condition,predicteds=distpred))#making actual and predicted dataframe i.e 
ap
correlationa_accuracy<-cor(ap)#correlation accuracy
a<-as.array(correlationa_accuracy)
accuracy(a)
new<-data.frame("MaxTemp"=29,"MinTemp"=23,"AvgWind"=19,"AvgHumidity"=57,"AvgPressure"=1017,"Rain"=0.0)
distpred2<-predict(pred,new)#predict distance
distpred2
if (distpred2>0&distpred2<12) { 
  print('Rain')
} else if (distpred2>11&distpred2<19) {
  print('Moderate')
} else if  (distpred2==19) {
  print('Overcast')
} else if  (distpred2==24) {
  print('Sunny')
} else {
  print('patchy light drizzle')
}
