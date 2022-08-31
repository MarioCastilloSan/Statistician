library(Hmisc)
library(TTR)
#We generate the seed
set.seed(1121)
x<-pnorm(n=100, mean=10,sd=2)
y<- dnorm(n=10000, mean=10,sd=2)

#n=100
data<-data.frame(x=x)
n<-nrow(data)
#Sturges
m<-1+ceiling(3.322*log10(n))
C<-2
cuts<-c(-Inf,4,6,8,10,12,14,16,Inf)
data$xFact <-cut2(data$x,cuts,digits=5)
frcTable<-data.frame(x=levels(data$xFact))
frcTable$f <- table(data$xFact)
frcTable$fr <- frcTable$f/n
frcTable$F <- cumsum(frcTable$f)
frcTable$Fr <- cumsum(frcTable$fr)


#n=10000
datay<-data.frame(y=y)
n<-nrow(datay)
#Sturges
m<-1+ceiling(3.322*log10(n))
C<-2
cuts2<-c(-Inf,4,6,8,10,12,14,16,Inf)
datay$yFact <-cut2(datay$y,cuts2,digits=5)
frcTable2<-data.frame(y=levels(datay$yFact))
frcTable2$f <- table(datay$yFact)
frcTable2$fr <- frcTable2$f/n
frcTable2$F <- cumsum(frcTable2$f)
frcTable2$Fr <- cumsum(frcTable2$fr)


frcTable
frcTable2
