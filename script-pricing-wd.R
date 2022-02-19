
setwd("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature")
source("functions-wd.R")

######
## 0 HDD coverage
######
K = 100 #exercice price
Temp = 10 #temperature threshold
  
  
values = price_instrument(Temp, path = "folder1/", period_start = '2021-01-01', period_end = '2021-10-30', option = "HDD")
pricing = data_instrument(values, K)

#EL
#Var90
#Var95
#Var99