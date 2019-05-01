#including the file 
nr1<-read.table("C:/breast.txt",header=TRUE,sep=",")
head(nr1)

#formatting the data 
nr2<-nr1[,-1]
head(nr2)
no<-c(2,4)
class1<-c("benign","malignant")
df <- data.frame(no,class1)
nr2$A10 <- df$class1[match(nr2$A10,df$no)]
head(nr2)
#is.na(nr2)<-sapply(nr2,is.infinite)#
#is.na(nr2)<-sapply(nr2,is.nan)
#data.frame(nr2)


install.packages("caTools")
library(caTools)
set.seed(123)
split<-sample.split(nr2$A10,SplitRatio = 0.75)
training_set=subset(nr2,split==TRUE)
test_set=subset(nr2,split==FALSE)
dim(training_set)  #525 observations with 10 attributes
dim(test_set)      #174 observations with 10 attributes
print(training_set)
print(training_set)

# Decision Tree
library(rpart)
dtm<-rpart(A10~.,training_set,method = "class")
install.packages("rpart.plot")
library(rpart.plot)
dtm
rpart.plot(dtm)
p<-predict(dtm,test_set,type="class")
print(p)
install.packages("caret",dependencies = TRUE)
library(caret)
table(test_set[,10],p)
confusionMatrix(test_set[,10],p)
model= train(A10~.,training_set,'rpart',trControl = trainControl(method='cv',number=10))
model

#knn

iris_train_target <- training_set[,10]
iris_test_target <- test_set[,10]
sqrt(700) 
summary(training_set[,c(1,2,3,4,5,6,7,8,9)])
normalize <- function(x) {
  return((x- min(x))/(max(x)- min(x)))
}
z<-c(1,2,3,4,5,6,7,8,9)
summary(z) 
normalize(z)
print(nr2 [,c(1,2,3,4,5,6,7,8,9)])
cancer <- as.data.frame(lapply(nr2 [,c(1,2,3,4,5,7,8,9)], normalize))
str(cancer)
summary(cancer)


library(class) 
m1<-knn(train = training_set[,c(1,2,3,4,5,7,8,9)],test =
          test_set[,c(1,2,3,4,5,7,8,9)],cl=iris_train_target,k=13)
m1
table(iris_test_target,m1)
library(caret)
install.packages("caret",dependencies = TRUE)
confusionMatrix(iris_test_target,m1) 

#naive bayes
head(nr2)
x=nr2[,-10]
y=nr2$A10
install.packages("e1071") 
library(e1071)
model<-naiveBayes(nr2$A10~.,data=nr2)

pred<-predict(model,nr2[,-10])
table(pred,y) 
library(caret)
confusionMatrix(iris_test_target,m1) 


#random sub sampling
nr2
library(rpart)
library(caret)
acc<-c()
for(i in 1:10){
  
  s<-sample(1:150, size=round(0.75*150), replace=FALSE)
  cancer_train<- nr2[s,]
  cancer_test<-nr2[-s,]
  dtm<-rpart(A10~.,cancer_train,method="class")
  p<-predict(dtm,cancer_test,type="class")
  dti<-confusionMatrix(cancer_train[,10],p)
  acc<-c(acc,dti$overall['Accuracy'])
}
acc
cat("average of accuracy is ",mean(acc))


#cross validation
var1<-nr2[,-10]
var1
var2<-nr2$A10
model=train(var1,var2,'nb',trControl=trainControl(method='cv', number=10))
model
