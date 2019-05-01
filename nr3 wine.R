wine<-read.csv("C:/wine.csv",header = TRUE)
head(wine)

summary(wine)

mean(wine$Alcohol)
sd(wine$Ash)
sd(iris$Alcohol)
install.packages("caret")
library(caret)
wine.f<-wine

#divide the attribute value by standard deviation(scaling)
preProcessParams<-preProcess(wine.f[,-1],method=c("scale"))
print(preProcessParams)
wine_transformed<-predict(preProcessParams,wine.f[,2:14])
summary(wine_transformed)
sd(wine_transformed$Alcohol)
sd(wine_transformed$Ash)

#subtract the mean from the attribute value(centering)
preProcessParams<-preProcess(wine.f[,-1],method=c("center"))
print(preProcessParams)
wine_transformed<-predict(preProcessParams,wine.f[,2:14])
summary(wine_transformed)

#doing both the process simultaneously
preProcessParams<-preProcess(wine.f[,-1],method=c("center","scale"))
print(preProcessParams)
wine_transformed<-predict(preProcessParams,wine.f[,2:14])
summary(wine_transformed)
sd(wine_transformed$Ash)
sd(wine_transformed$Alcohol)