data("iris")
plot(iris)#scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)
str(iris)

irisScaled <- scale(iris[,-5])
irisScaled
summary(irisScaled)

fitK <- kmeans(irisScaled,3) #scaled data set and number of clusters passed
fitK
fitK$size
fitK$cluster
str(fitK)
plot(iris,col=fitK$cluster)
k <-list () 
for (i in 1:10) {
  k[[i]] <- kmeans(irisScaled,i)
}
k
betweenss_totss<-list()
for (i in 1:10) {
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10,betweenss_totss,type = "b",ylab = "betweenss ss/total ss",xlab = "Cluster(k)")
for (i in 1:4) {
  plot(iris,col=k[[i]]$cluster)
}
table(iris$Species,fitK$cluster)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width,color = iris$Species)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width,color = fitK$cluster)) + geom_point()


d <- dist(irisScaled)
fitH<- hclust(d,"ward.D2") 
?hclust 
plot(fitH)
rect.hclust(fitH,k=3,border = "red")
cluster<-cutree(fitH,3)
cluster
plot(iris,col=cluster)
table(iris$Species,cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) +
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cluster) +
  scale_color_manual(values = c('black', 'red', 'green'))

#density based clustering
install.packages("dbscan")
library(dbscan)

kNNdistplot(irisScaled,k=3)
abline(h=0.7,col="red",lty=2)
fitD<-dbscan(irisScaled,eps =0.7 ,minPts =5 )

plot(iris,col=fitD$cluster)
table(iris$Species,fitD$cluster)
