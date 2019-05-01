data("iris")
summary(iris)

mean(iris$sepal.Length)
sd(iris$Sepal.Length)
sd(iris$Petal.Length)
install.packages("caret")
library(caret)
iris.f<-iris

#divide the attribute value by standard deviation(scaling)
preProcessParams<-preProcess(iris.f[,-5],method=c("scale"))
print(preProcessParams)
iris_transformed<-predict(preProcessParams,iris.f[,1:4])
summary(iris_transformed)
sd(iris_transformed$Petal.Length)
sd(iris_transformed$Sepal.Length)

#subtract the mean from the attribute value(centering)
preProcessParams<-preProcess(iris.f[,-5],method=c("center"))
print(preProcessParams)
iris_transformed<-predict(preProcessParams,iris.f[,1:4])
summary(iris_transformed)

#doing both the process simultaneously
preProcessParams<-preProcess(iris.f[,-5],method=c("center","scale"))
print(preProcessParams)
iris_transformed<-predict(preProcessParams,iris.f[,1:4])
summary(iris_transformed)
sd(iris_transformed$Petal.Length)
sd(iris_transformed$Sepal.Length)
