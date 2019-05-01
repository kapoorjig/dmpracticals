#method to split data using caTools Library
install.packages("caTools")
library(caTools)
set.seed(123)
split<-sample.split(iris$Species,SplitRatio = 0.75)
training_set=subset(iris,split==TRUE)
test_set=subset(iris,split==FALSE)
dim(training_set)   #114 obs. of 5 variables
dim(test_set)       #36 obs. of 5 variables
print(training_set)
print(training_set)
print (iris)
# Decision Tree
library(rpart)
?rpart
dtm<-rpart(Species~.,training_set,method = "class")
install.packages("rpart.plot")
library(rpart.plot)
dtm
rpart.plot(dtm)
p<-predict(dtm,test_set,type="class")
install.packages("caret",dependencies = TRUE)
library(caret)
table(test_set[,5],p)
confusionMatrix(test_set[,5],p)
model= train(Species~.,training_set,'rpart',trControl = trainControl(method='cv',number=10))
model

#knn

iris_train_target <- training_set[,5]
iris_test_target <- test_set[,5]
sqrt(150) 
summary(training_set[,c(1,2,3,4)])
normalize <- function(x) {
  return((x- min(x))/(max(x)- min(x)))
}
z<-c(1,2,3,4,5)
summary(z) 
normalize(z)
iris_n <- as.data.frame(lapply(iris [,c(1,2,3,4)], normalize))
str(iris_n)
summary(iris_n)
library(class) 
m1<-knn(train = training_set[,c(1,2,3,4)],test =
          test_set[,c(1,2,3,4)],cl=iris_train_target,k=13)
m1
table(iris_test_target,m1)
library(caret)
install.packages("caret",dependencies = TRUE)
confusionMatrix(iris_test_target,m1) 

#naive bayes
head(iris)
x=iris[,-5]
y=iris$Species
install.packages("e1071") 
library(e1071)
model<-naiveBayes(iris$Species~.,data=iris)

pred<-predict(model,iris[,-5])
table(pred,y) 
library(caret)
confusionMatrix(iris_test_target,m1) 

#random sub sampling
iris
library(rpart)
library(caret)
acc<-c()
for(i in 1:10){
  
 s<-sample(1:150, size=round(0.75*150), replace=FALSE)
 iris_train<- iris[s,]
 iris_test<-iris[-s,]
 dtm<-rpart(Species~.,iris_train,method="class")
 p<-predict(dtm,iris_test,type="class")
 dti<-confusionMatrix(iris_test[,5],p)
 acc<-c(acc,dti$overall['Accuracy'])
}
acc
cat("average of accuracy is ",mean(acc))


#cross validation
var1<-iris[,-5]
var1
var2<-iris$Species
model=train(var1,var2,'nb',trControl=trainControl(method='cv', number=10))
model
