library(caret)
library(ggplot2)
library(dplyr)
library(corrplot)
library(corrgram)
library(gplots)
library(dplyr)

monDevoir<-read.csv("file:///C:/Users/User/Desktop/AmarAssessment/assesment_Amara.csv")
View(monDevoir)

colnames(monDevoir)<-c("colA","colB","colC","colD", "colDepend")


plot(monDevoir$colA) #outlier 300
hist(monDevoir$colA)

plot(monDevoir$colB)
hist(monDevoir$colB)

plot(monDevoir$colC)
hist( monDevoir$colC)

plot(monDevoir$colDepend)
hist(monDevoir$colDepend) #outlier around 300

plot(monDevoir$colD) #outlier around -50
qplot(monDevoir$colD)
#ggplot(Devoir)+geom_histogram(aes(x=Prediction), binwidth = 0.5) #distrubition of data
ggplot(monDevoir)+geom_histogram(aes(x=colDepend), binwidth = 0.5) #distrubition of data

#looking for cor
corrData<-cor(monDevoir,use = "complete.obs") #use=complete.obs tell R to ignore the NAs
corrplot(corrData)
View(corrData)
#no high correlation
#let's remove the outliers
monDevoir1<-monDevoir
str(monDevoir1)
summary(monDevoir1)
#Replaced monDevoir$colDepend's outliers by 0 
monDevoir1$colA [monDevoir1$colA>300]<- mean(monDevoir1$colA)
plot(monDevoir1$colA)
View(monDevoir1)

#replace monDevoir$colD outlier by mean of the monDevoir1$colD
monDevoir1$colD[monDevoir1$colD< -45]<- mean(monDevoir1$colD)
plot(monDevoir1$colD)

#let's deal with the NAs now

str(monDevoir1)
summary(monDevoir1)
#Dealing with  NA
sum(is.na(monDevoir1)) #428
monDevoir1<- na.omit(monDevoir1)

monDevoir<-monDevoir1
View(monDevoir)
#"sampling" and modelin of my dataset
#Develop our model and set a data partition
inTrain<-createDataPartition(monDevoir$colDepend, p=0.75, list= FALSE)
Training<-monDevoir[inTrain,]
Testing<-monDevoir[-inTrain,]
nrow(Training) #7174
nrow(Testing) #2389

#10Fold cross validation
set.seed(2018)
myControl<-trainControl( method = "repeatedcv", number=10, repeats = 2)

#algo to try : KNN, svm, gbm, rf and prediction
svmModel<-train(colDepend~., method = "svmLinear", data=Training, trControl = myControl, preProcess = c("center","scale"))
svmModel

knnModel<-train(colDepend~., method="knn", data=Training, trControl=myControl, preProcess=c("center","scale"))
knnModel #good

gbModel<-train(colDepend~., method="gbm", data = Training, trControl=myControl, preProcess=c("center", "scale"))
gbModel

rfModel<-train(colDepend~., method="rf", data=Training, trControl=myControl, preProcess=c("center","scale"))
rfModel #awesome

result<-resamples(list(svm=svmModel, knn=knnModel, gbm=gbModel, rf=rfModel))
summary(result)

#++++++++++++++++++++++++++++++++++Predictions of the models+++++++++++++++++++++++++++
predicSvm<-predict(svmModel,Testing)
predicSvm
postResample(predicSvm, Testing$colDepend)
#sum(predicSvm<0) #138

predicKnn<-predict(knnModel,Testing)
predicKnn
postResample(predicKnn, Testing$colDepend)
sum(predicKnn<0) #0

predicgb<-predict(gbModel, Testing)
predicgb
postResample(predicgb, Testing$colDepend)
sum(predicgb<0)#17

predicRf<-predict(rfModel, Testing)
predicRf
postResample(predicRf, Testing$colDepend)
sum(predicRf<0) #0 with high Rsquare of 80

#++++++++++++++++++++++++++++++++++++++++++++++++++
ggplot(Testing)+ geom_histogram(aes(x=predicRf ),binwidth = 0.5)

plot(rfModel)
dotplot(result)

#+++++++++++++++++++++++++++++Creating  new Prediction column  in my dataset 
Devoir<-monDevoir
newPred<-predict(rfModel, Devoir)

Devoir$Prediction<-newPred
View(Devoir)

write.csv(Devoir, "AmaraAssessment.csv", row.names = TRUE)


