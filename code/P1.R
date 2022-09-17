#begin
library(moments)
library(gridExtra)
library(readr)
library(Hmisc)
library(TTR)
library(modeest)
library(kableExtra)
options(max.print=1000)
##### We load the google play store data #####
gpData <- read.csv("Google-Playstore.csv",header =
                        TRUE, sep = "," )
gpFreeData<-subset(gpData, gpData$Free == 'True')
gpPayData<-subset(gpData, gpData$Free == 'False')

#We define the global parameters such a nrow and sturge rule for freee data at installs
nFree<-nrow(gpFreeData)
mFree<-1+ceiling(3.322*log10(nFree))
#Downloads
iFree<- max(gpFreeData$Maximum.Installs)-min(gpFreeData$Maximum.Installs)
cFree<- iFree/mFree
intervals <- seq(min(gpFreeData$Maximum.Installs),max(gpFreeData$
                                                     Maximum.Installs),cFree)

#We create the  free dataframe for installs
insData<- data.frame(
  insQuantity=gpFreeData$Maximum.Installs)
insData$insFact<-cut2(
insData$insQuantity,intervals,digits = 5)
MC<-runMean(intervals,2)
MC<-MC[2:length(MC)]
frcTable <-data.frame(insQuantity=
                      levels(insData$insFact))
frcTable$MC <- MC
frcTable$f <- table(insData$insFact)
frcTable$fr <- round(frcTable$f/nFree,2)
frcTable$F <- cumsum(frcTable$f)
frcTable$Fr <- cumsum(frcTable$fr)
frcTable
frcTable <- kable( frcTable,
                   format = "latex" , 
                   caption = 
                     "Tabla de frecuencias de las descargas app gratis"
                   , align = c('l','c','c','c','c','c')
                   , col.names = c(
                     "insQuantity","MC","f","fr","F","Fr")
                   , row.names = TRUE, digits = 2
                   , format.args = list( decimal.mark = ",")
)

#We define the global parameters such a nrow and sturge rule for pay data at installs
nP<-nrow(gpPayData)
mP<-1+ceiling(3.322*log10(nP))
#Downloads
iPay<- max(gpPayData$Maximum.Installs)-min(gpPayData$Maximum.Installs)
cPay<- iPay/mP
intervalsp <- seq(min(gpPayData$Maximum.Installs),max(gpPayData$
                                                        Maximum.Installs),cPay)

#We create the pay dataframe for installs
insData<- data.frame(
  insQuantity=gpPayData$Maximum.Installs)
insData$insFact<-cut2(
  insData$insQuantity,intervalsp,digits = 5)
MC<-runMean(intervalsp,2)
MC<-MC[2:length(MC)]
frcTableP <-data.frame(insQuantity=
                        levels(insData$insFact))
frcTableP$MC <- MC
frcTableP$f <- table(insData$insFact)
frcTableP$fr <- round(frcTableP$f/nP,2)
frcTableP$F <- cumsum(frcTableP$f)
frcTableP$Fr <- cumsum(frcTableP$fr)
frcTableP
frcTableP <- kable( frcTableP,
                   format = "latex" , 
                   caption = 
                     "Tabla de frecuencias de las descargas app de pago"
                   , align = c('l','c','c','c','c','c')
                   , col.names = c(
                     "insQuantity","MC","f","fr","F","Fr")
                   , row.names = TRUE, digits = 2
                   , format.args = list( decimal.mark = ",")
)

#Downloads
iFreeR<- max(gpFreeData$Rating,na.rm=T)-min(gpFreeData$Rating,na.rm=T)
cFreeR<- iFreeR/mFree
intervalsrf <- seq(min(gpFreeData$Rating,na.rm=T),max(gpFreeData$
                                                          Rating,na.rm=T),cFreeR)

#We create the  free dataframe for ratings
rateDataF<- data.frame(
  rateValues=gpFreeData$Rating)
rateDataF$rateFact<-cut2(
  rateDataF$rateValues,intervalsrf,digits = 5)
MC<-runMean(intervalsrf,2)
MC<-MC[2:length(MC)]
frcTableRF <-data.frame(rateValues=
                        levels(rateDataF$rateFact))
frcTableRF$MC <- MC
frcTableRF$f <- table(rateDataF$rateFact)
frcTableRF$fr <- round(frcTableRF$f/nFree,2)
frcTableRF$F <- cumsum(frcTableRF$f)
frcTableRF$Fr <- cumsum(frcTableRF$fr)
frcTableRF
frcTableRF <- kable( frcTableRF,
                   format = "latex" , 
                   caption = 
                     "Tabla de frecuencias del rating para las app gratis"
                   , align = c('l','c','c','c','c','c')
                   , col.names = c(
                     "rateValues","MC","f","fr","F","Fr")
                   , row.names = TRUE, digits = 2
                   , format.args = list( decimal.mark = ",")
)
frcTableRF
#Ratings
irPay<- max(gpPayData$Rating,na.rm =T )-min(gpPayData$Rating,na.rm=T)
crPay<- irPay/mP
intervalsrp <- seq(min(gpPayData$Rating,na.rm=T),max(gpPayData$Rating,na.rm=T),crPay)
#We create the pay dataframe for ratings
rateData<- data.frame(
  rateValues=gpPayData$Rating)
rateData$rateFact<-cut2(
  rateData$rateValues,intervalsrp,digits = 5)
MC<-runMean(intervalsrp,2)
MC<-MC[2:length(MC)]
frcTablePR <-data.frame(rateValues=
                         levels(rateData$rateFact))
frcTablePR$MC <- MC
frcTablePR$f <- table(rateData$rateFact)
frcTablePR$fr <- round(frcTablePR$f/nP,2)
frcTablePR$F <- cumsum(frcTablePR$f)
frcTablePR$Fr <- cumsum(frcTablePR$fr)
frcTablePR
frcTablePR <- kable( frcTablePR,
                    format = "latex" , 
                    caption = 
                      "Tabla de frecuencias del rating para las app de pago"
                    , align = c('l','c','c','c','c','c')
                    , col.names = c(
                      "rateValues","MC","f","fr","F","Fr")
                    , row.names = TRUE, digits = 2
                    , format.args = list( decimal.mark = ",")
)


#Statistician
statisticianF<-function(dfCol,mdP){
  meanData <- round(mean(dfCol,na.rm=T))
  medianData <- round(median(dfCol,na.rm=T))
  sdData <- sd(dfCol,na.rm=T)
  iqrData <- IQR(dfCol,na.rm=T)
  md<-mlv(gpPayData$Rating, method = "mfv",na.rm=T)
  if(mdP == T){
  return(dataFrameG <- data.frame(
    "Mean" = meanData,
    "Median" = medianData,
    "Standar_Deviation"=sdData,
   "IQR" = iqrData,
   "Mlv" = md
  ))}else{
    return(dataFrameG <- data.frame(
      "Mean" = meanData,
      "Median" = medianData,
      "Standar_Deviation"=sdData,
      "IQR" = iqrData
    ))
  }
}
dfIF<-statisticianF(gpFreeData$Maximum.Installs,F)
dfIP<-statisticianF(gpPayData$Maximum.Installs,F)
dfRF<-statisticianF(gpFreeData$Rating,T)
dfRP<-statisticianF(gpPayData$Rating,T)
#Data
generate_table <-function(data_frame, caption_name, vector_name, names_row, digits,mlvP){
  if(mlvP == T){
    return(
      kable(data_frame,
            format = "latex" , 
            caption = caption_name,
            align = c('l','c','c','c','c','c'),
            col.names = vector_name,
            row.names = names_row, digits = digits,
            format.args = list( decimal.mark = ",")                      
      )
    )
  }else{
  return(
    kable(data_frame,
          format = "latex" , 
          caption = caption_name,
          align = c('l','c','c','c','c'),
          col.names = vector_name,
          row.names = names_row, digits = digits,
          format.args = list( decimal.mark = ",")                      
    )
  )}
}
vector_name <-c("Mean","Median","Standar_Deviation","IQR")
vector_nameMlv <-c("Mean","Median","Standar_Deviation","IQR","Mlv")
dfIF<-generate_table(dfIF,"Estadigrafo para las app gratis en base a las descargas",vector_name, TRUE,2,F)
dfIP<-generate_table(dfIP,"Estadigrafo para las app de pago en base a las descargas",vector_name, TRUE,2,F)
dfRF<-generate_table(dfRF, "Estadigrafo para las app gratis en base a su rating",vector_nameMlv, TRUE,2,T)
dfRP<-generate_table(dfRP, "Estadigrafo para las app de pago en base a su rating",vector_nameMlv, TRUE,2,T)

#Graphics
library(ggplot2)
#We load the data
#filer ratings from free and pay aps
dataFR<-subset(gpData, gpData$Free == 'True', 
                select=c(Free,Rating))
dataPR<-subset(gpData, gpData$Free == 'False', 
                     select=c(Free,Rating))


#filer installations from free and pay aps
dataFI<-subset(gpData, gpData$Free == 'True', 
               select=c(Free,Maximum.Installs))
dataPI<-subset(gpData, gpData$Free == 'False', 
               select=c(Free,Maximum.Installs))
#plot of density for free rating apps.
ggplot(dataFR, aes(x = Rating)) +  
  geom_density(aes(fill=Free),alpha=0.2) 

#plot of density for  pay rating apps.
ggplot(dataPR, aes(x = Rating)) +  
  geom_density(aes(fill=Free),alpha=0.2) 

#plot of density for free installations apps.
ggplot(dataFI, aes(x=Maximum.Installs)) + 
  geom_histogram(aes(fill=Free),alpha=0.2)

#plot of density for  pay installations apps.
ggplot(dataPI, aes(x=Maximum.Installs)) + 
  geom_histogram(aes(fill=Free),alpha=0.2)

