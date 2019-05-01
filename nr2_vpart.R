x<-c(3,7,2,6,9,1)
x
summary(x)
data("rivers")
rivers
head(rivers)
library(dplyr)
install.packages("tidyr")
library(tidyr)
?rivers
glimpse(rivers)
hist(rivers)
boxplot(rivers)
boxplot(rivers,horizontal = TRUE)
riv1<-rivers[rivers<1250]
riv1
boxplot(riv1,horizontal = TRUE)
hist(riv1)
boxplot.stats(riv1)
