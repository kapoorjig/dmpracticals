install.packages("arules")
library("arules")
data("Groceries")
str(Groceries)
summary(Groceries)
head(Groceries)
itemFrequencyPlot(Groceries,topN=10,type="absolute")
rules <- apriori(Groceries,parameter = list(supp=0.001,conf=0.8))#support=p(A.B), confidence=p(A.B)/p(A), lift=p(A.B)/p(A).p(B)
inspect(rules)
inspect(head(rules))
inspect(rules[1:10])
rules<-sort(rules, by="confidence", decreasing = T)
rules <- apriori(Groceries,parameter = list(supp=0.001,conf=0.8))
install.packages("arulesViz")
library(arulesViz)
plot(rules)
plot(rules[1:20], method = "graph", control = list(type = "items"))
plot(rules[1:20], method = "paracoord", control = list(reorder = TRUE))
plot(rules[1:20], method = "matrix", control = list(reorder = "support"))
plot(rules[1:20], method = "grouped")

#for dataset of basket


receipt_df <- read.csv("C:/NR Dm/apriori (1)/apriori (1)/1000/1000i.csv", header = F)
head(receipt_df)
names(receipt_df) <- c("Receipt_Number","Food","Quantity")
head(receipt_df)
id<-c(1:5)
food<-c("milk","sugar","chocolate","apples","curd")
df <- data.frame(id, food)
print(df)
receipt_df$Food <- df$food[match(receipt_df$Food,df$id)]
head(receipt_df)
test_df <- receipt_df[,c("Receipt_Number","Food","Quantity")]
typeof(test_df) 
head(test_df)

library(arules)
df_trans <- as(split(test_df$Food, test_df$Receipt_Number), "transactions")

rules2<-apriori(df_trans, parameter=list(supp=0.015,conf=0.9))
inspect(rules2)
plot(rules2[1:20], method = "graph", control = list(type = "items"))
plot(rules2[1:20], method = "paracoord", control = list(reorder = TRUE))
plot(rules2[1:20], method = "matrix", control = list(reorder = "support"))
plot(rules2[1:20], method = "grouped")

