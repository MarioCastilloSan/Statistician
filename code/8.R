library(Hmisc)
library(TTR)
hfData <- read.csv("heart_failure_clinical_records_dataset.csv", header = TRUE, sep = ",")
platelets <- hfData$platelets
dataFrc <- data.frame(Platelets=platelets)

# Numero de intervalos con la Regla de Sturges // Number of intervals with Sturges Rule
n <- nrow(dataFrc)
m <- 1+ceiling(3.322*log10(n))

# Rango y amplitud usando Sturges calculando el maximo menos el minimo y luego dividiendo por la division de clases //
#Range and amplitude using Sturges calculating the maximum minus the minimum and then dividing by the division of classes
I <- max(dataFrc$Platelets)-min(dataFrc$Platelets)
C <- I/m

#Genera la secuencia de datos separados por el intervalo c // we generate the sequence of data separated by the interval c
cuts <- seq(min(dataFrc$Platelets),max(dataFrc$Platelets), C)

#Definida PateletsFact como variable categorica, con los datos y cortes calculados crear intervalos de 5 digitos // 
#we define PateletsFact as a categorical variable, with the calculated data and cuts  we create   5-digit intervals
dataFrc$PlateletsFact <- cut2(dataFrc$Platelets, cuts, digits=5)

#Creamos las marcas de clase calculando el promedio con un valor por la izuquierda y uno por la derecha
#We create the class marks by calculating the average with a value on the left and one on the right
MC <- runMean(cuts, 2)

#Descartamos el primer valor
#We discard the first value
MC<-MC[-1]

#Obtenemos los niveles en los que se dividen los datos
#We obtain the levels in which the data is divided
frcTable <- data.frame(Platelets=levels(dataFrc$PlateletsFact))

#Creamos y asignamos los valores ya calculados a la columna MC
# We create and assign the values already calculated to the MC column
frcTable$MC <- MC

#Calculamos la frecuencia absoluta
# Calculate the absolute frequency
frcTable$f <- table(dataFrc$PlateletsFact)

#Calculamos la frecuencia relativa
# Calculate the relative frequency
frcTable$fr <- frcTable$f/n

#Calculamos la frecuencia absoluta acumulada
# Calculate the cumulative absolute frequency
frcTable$F <- cumsum(frcTable$f)

#Calculamos la frecuencia relativa acumulada
# Calculate the cumulative relative frequency
frcTable$Fr <- cumsum(frcTable$fr)

frcTable
