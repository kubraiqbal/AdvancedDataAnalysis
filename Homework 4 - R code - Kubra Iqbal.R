#data from the excel file - reading sheets
setwd("C:/Users/kiqbal2/Desktop")
training=readxl::read_xls("BondRating.xls",sheet="training", skip=2)
validation=readxl::read_xls("BondRating.xls", sheet="validation", skip=2)
head(BondRating)
str(BondRating)

#Redefine the data from the data set 
library(psych)
pairs.panels(BondRating[2:13], 
             gap=0,
             pch=21)
newdata=BondRating[2:13]

library(MASS)
#LDA - conducted on data
dset=lda(CODERTG ~ ., data=newdata)
plot(dset)
dset
attributes(dset)


p<-predict(dset,newdata)
ldahist(data=p$x[,1], g=newdata$CODERTG)
ldahist(data=p$x[,2],g=newdata$CODERTG)
ldahist(data=p$x[,3],g=newdata$CODERTG)
ldahist(data=p$x[,4],g=newdata$CODERTG)
ldahist(data=p$x[,5],g=newdata$CODERTG)
ldahist(data=p$x[,6],g=newdata$CODERTG)


#Prediction of data set 
p = predict(dset, newdata2=newdata[,3:11])$class
p

# Compare the results of the prediction
table(p, newdata$CODERTG)

# Setting "CV = T" will have the lda function perform
# "Leave-one-out" cross-validation
LDA=lda(CODERTG ~ ., data=newdata, CV=T)
table(LDA$class, BondRating$CODERTG)
coef(LDA)