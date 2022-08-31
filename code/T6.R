library(moments)
dfHearth <- read.csv("heart_failure_clinical_records_dataset.csv",header = TRUE, sep = ",")
dfHearth['platelets']
hMean<- mean(dfHearth$platelets)
hMed <- median(dfHearth$platelets)
hSd <- sd(dfHearth$platelets)
hIqr <- IQR(dfHearth$platelets)
hAsym <- skewness(dfHearth$platelets)
hCur <- kurtosis(dfHearth$platelets)

dfTable  <- data.frame(
  "Mean" = hMean,
  
  
  "Median" = hMed,
  "StandarDeviation"=hSd,
  "InterquartileRange" = hIqr,
  "Asymmetry" = hAsym,
  "ExcessKurtosis" = hCur
)
dfTable
