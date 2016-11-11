#collecting data
setwd("D:/Machine Learning/R Books")

#exploring and preparing the data
wbcd<-read.csv("BreastCancerData.csv",stringsAsFactors=F)
str(wbcd)
wbcd<-wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))
round(prop.table(table(wbcd$diagnosis))*100,digits=1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#Transformation – normalizing numeric data
normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}  
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#Data preparation – creating training and test datasets
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

#Step 3 – training a model on the data
library(class)
wbcd_test_pred<-knn(train=wbcd_train,test=wbcd_test,cl=wbcd_train_labels,k=21)

#Step 4 – evaluating model performance
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = F)

#Step 5 – improving model performance
#Transformation – z-score standardization
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train<-wbcd_z[1:469,]
wbcd_test<-wbcd_z[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]
wbcd_test_pred<-knn(train=wbcd_train,test=wbcd_test,cl=wbcd_train_labels,k=21)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = F)

#Testing alternative values of k
wbcd_test_pred<-knn(train=wbcd_train,test=wbcd_test,cl=wbcd_train_labels,k=27)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = F)
