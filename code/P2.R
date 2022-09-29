library(readr)
owid_covid_data <- read_csv("owid-covid-data.csv", 
                            col_types = cols(iso_code = col_skip(), 
                                             continent = col_skip()))

#functions section 

#we create a function to select the country
countrySelection <- function(Country, data) {
  dataF = data[data$location==Country,]
  return (dataF)
}

#we select the date filters to work

dateSelection<-function(initialDateS,endDateS){
  initialDate = as.Date(initialDateS)
  endDate = as.Date(endDateS)
  listR<-list(initialDate,endDate)
  return (listR)
}

#we select a country
GenericDT <- countrySelection('China',owid_covid_data)


#date filters 
date <-dateSelection("2019-9-23","2022-9-23")


#new dataframe with define dates
GenericDT = GenericDT[GenericDT$date>date[1] & GenericDT$date<date[2],]

#remove na
GenericDT[is.na(GenericDT)]=0

#we filter the data to select dates and ne cases as columns
GenericDT = GenericDT %>% select(1:2,4)

#we select the data that contains days where contagious are more than 0
y=c(GenericDT$new_cases[GenericDT$new_cases != 0])
x=1:NROW(GenericDT$new_cases[GenericDT$new_cases != 0])

# We plot this data
plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=0.2,xlab = 'Time(Days)',ylab = "Infections" ) 

# Firstly we will fit the lgModel
lgModel <- lm(y ~ log(x))

# we define our line to draw it later
x=seq(from=1,to=NROW(GenericDT$new_cases[GenericDT$new_cases != 0]),length.out=5000)

# use the lgModel to predict the  regression
y=predict(lgModel,newdata=list(x=seq(
  from=1,to=NROW(GenericDT$new_cases[GenericDT$new_cases != 0]),length.out=5000)),
  interval="confidence")

# finally we draw the line
matlines(x,y, lwd=1)

