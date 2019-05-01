library(tidyr)
nr1<-read.table("C:/Users/Nirnay_Mittal/Desktop/sem VI/dm practical/nr1.txt",header=TRUE,sep=',')
install.packages("editrules")
library(editrules)
E<-editset(expression(
 age<=150,
  age>yearmarried,
  status %in% c('single','married','widowed'),
  agegroup %in% c('child','adult','elderly'),
  if(age<18) agegroup=='child',
  if(age>=18 & age<=65) agegroup=='adult',
  if(age>65) agegroup=='elderly'
))


sm <- violatedEdits(E,nr1)
summary(sm)
plot(sm)
E
ve<-violatedEdits(E,nr1)
print(ve)
View(nr1)
summary(ve,E)
summary(ve)
plot(E)
plot(ve)
plot(E,layout = layout.grid)