#######################################################################################################
library(readr)#Leer csv
library(dplyr)# filters type %>%
library(treemap)#treemap
library(corrplot)#plots
library(nnet)#neural net for prediction
options(max.print=1000000)



######Reading data
mydata <- read.csv("Cardiotocographic.csv")
mydata2 <- read.csv("Cardiotocographic.csv")
str(mydata)

#NSP as factor (Normal,Suspect,Pathologic)
mydata$NSP<-as.factor(mydata$NSP)


str(mydata)

#separating the train data
train<-nrow(mydata)*0.80
test<-nrow(mydata)*0.20


data_train = mydata[0:train,]
data_test  = mydata[train+1:test,]


#releveling the model making 1( Normal ) the default value
data_train$NSP<-relevel(data_train$NSP,ref="1")
mymodel<-multinom(NSP~.-MSTV-MLTV-Width-Min-Max-Nzeros-Mode-Tendency,data=data_train)
summary(mymodel)

#Statistical significant values with 2-tailed z-test
z<- summary(mymodel)$coefficients/summary(mymodel)$standard.errors

#p values  1- normal distribution of absolute values of z, with mean 0 and std 1 for normal distribution *2 because is a 2 tail test 
p<-(1-pnorm(abs(z),0,1))*2
#p values should be .05 or les for a confincence level of 95
p
 
#Multinomial logistic regression Model ln(p(NSP=2/p(NSP=1)))  log odds of patient being suspect vs normal
#ln(p(NSP=3/p(NSP=1))) log odd of patien being pathologic vs patient bein normal



#Confusion Matrix & misclassification Error-training Data
prediction_data<-predict(mymodel,data_train)
head(prediction_data)
head(data_train$NSP)

tab<-table(prediction_data,data_train$NSP)
tab
#Accuracy rate of the model for classification
sum(diag(tab))/sum(tab)

#missclasification
1-sum(diag(tab))/sum(tab)


#testing data
p1<-predict(mymodel,data_test)
tab1<-table(p1,data_test$NSP)
tab1

#Accuracy rate of the model for classification
sum(diag(tab1))/sum(tab1)

#missclasification
1-sum(diag(tab1))/sum(tab1)



####Predicion and model Assesment
n<-table(data_train$NSP)
n/sum(n)



tab/colSums(tab)
tab1/colSums(tab1)




#functions section 
treemap(mydata2, 
        index=c("NSP"), 
        vSize = "NSP",  
        palette = "Reds",  
        title="Patiens proportion", 
        fontsize.title = 14 
)

#correlation of values
#adjusted
mydataCor <- mydata2[,c("LB","AC","FM","UC","DL","DP","ASTV","ALTV","Nmax","Mean",
                        "Median","Variance","Tendency","NSP")]
mydataCor <- na.omit(mydataCor)
correlations <- cor(mydataCor)
p <- corrplot(correlations, method="circle")

