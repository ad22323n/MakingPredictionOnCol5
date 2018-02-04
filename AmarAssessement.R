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

#RMSE      Rsquared   MAE     
#4.922046  0.1432062  3.348673

knnModel<-train(colDepend~., method="knn", data=Training, trControl=myControl, preProcess=c("center","scale"))
knnModel #good

#k  RMSE       Rsquared   MAE      
#5  0.5104983  0.9907613  0.3079212
#7  0.5671483  0.9888010  0.3490408
#9  0.6282042  0.9866373  0.3933126

gbModel<-train(colDepend~., method="gbm", data = Training, trControl=myControl, preProcess=c("center", "scale"))
gbModel

#interaction.depth  n.trees  RMSE      Rsquared   MAE     
#1                   50      4.916458  0.1387559  3.372275
#1                  100      4.909388  0.1400920  3.389081
#1                  150      4.912415  0.1390916  3.392340
#2                   50      3.574127  0.6488130  2.127951
#2                  100      2.551697  0.7970163  1.541526
#2                  150      2.293528  0.8181739  1.508961
#3                   50      3.081003  0.7539630  1.814361
#3                  100      1.771902  0.9183369  1.173136
#3                  150      1.169977  0.9595108  0.864811


rfModel<-train(colDepend~., method="rf", data=Training, trControl=myControl, preProcess=c("center","scale"))
rfModel #awesome

#mtry  RMSE       Rsquared   MAE      
#2     0.7429164  0.9911129  0.4096504
#3     0.3351316  0.9964986  0.1830408
#4     0.2878232  0.9969871  0.1511289


result<-resamples(list(svm=svmModel, knn=knnModel, gbm=gbModel, rf=rfModel))
summary(result)

#++++++++++++++++++++++++++++++++++Predictions of the models+++++++++++++++++++++++++++
predicSvm<-predict(svmModel,Testing)
predicSvm
postResample(predicSvm, Testing$colDepend)
#RMSE  Rsquared       MAE 
#0.4206027 0.9942508 0.2841849
predicKnn<-predict(knnModel,Testing)
predicKnn
postResample(predicKnn, Testing$colDepend)

#k  RMSE       Rsquared   MAE      
#5  0.5104983  0.9907613  0.3079212
#7  0.5671483  0.9888010  0.3490408
#9  0.6282042  0.9866373  0.3933126


predicgb<-predict(gbModel, Testing)
predicgb
postResample(predicgb, Testing$colDepend)
#postResample(predicgb, Testing$colDepend)
#RMSE  Rsquared       MAE 
#1.2508613 0.9578755 0.8772916


predicRf<-predict(rfModel, Testing)
predicRf
postResample(predicRf, Testing$colDepend)
#postResample(predicRf, Testing$colDepend)
#RMSE  Rsquared       MAE 
#0.3125138 0.9968339 0.1419530


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


