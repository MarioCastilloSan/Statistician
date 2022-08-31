library(ggplot2)

#We load the data
dataFH<-read.table("heart_failure_clinical_records_dataset.csv",
                   sep=",",header=T)

#filer platelets of  patients with anaemia
dataFHA<-subset(dataFH, dataFH$anaemia == 1, 
                select=c(anaemia,platelets))

#filer platelets of  patients with diabetes
dataFHD<-subset(dataFH, dataFH$diabetes == 1, 
                select=c(diabetes,platelets))

#plot of density for anaemia patients.
ggplot(dataFHA, aes(x = platelets)) +  
  geom_density(aes(fill=anaemia ,colour="blue"),alpha=0.2) 

#plot of density for diabetes patients.
ggplot(dataFHD, aes(x = platelets)) + 
  geom_density(aes(fill=diabetes ,colour="blue"),alpha=0.2) 