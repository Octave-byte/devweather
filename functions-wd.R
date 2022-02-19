library(data.table)
library(dplyr)
library(lubridate)
library(zoo)

setwd("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature")

#################
#Model function
#################

get_city_temperatures <- function(city, na_val = T){
  
  data = fread("city_temperature.csv")
  data = filter(data, City == city)
  data$AvgTemperature[data$AvgTemperature == -99] <- NA
  
  #transform F into Celsius
  data$AvgTemperature = (data$AvgTemperature - 32)*0.5556
  
  
  if (na_val){
    data$AvgTemperature= na.approx(data$AvgTemperature)
  }
  return(data)
}

get_detrend <- function(data,w = 2*pi/365){
  
  #remove 29 february
  data = filter(data, !(Month == 2 & Day == 29))
  #add timesteps
  data$timestep = 1:nrow(data)
  
  #create linear model
  model <- lm(AvgTemperature ~ timestep + sin(w*timestep) + cos(w*timestep), data = data)
  #summary(model)
  
  #we then obtain the parameters to compute Tjm
  
  A = model$coefficients[1] #intercept
  B = model$coefficients[2] #trend 
  C= sqrt(model$coefficients[3]**2 + model$coefficients[4]**2) #seasonality
  phi = atan((model$coefficients[4]/model$coefficients[3])) - pi
  
  return(c(A,B,C,phi))
}

get_seasonality <- function(data, coef, w = 2*pi/365){
  #Assign the data to a month in the year : ok
  
  data$timesteps = 1:nrow(data)
  timestep = data$timesteps
  
  ##Compute temperature variance for each month : var
  data = mutate(data, diff_temp = AvgTemperature - lag(AvgTemperature))
  data$diff_temp[is.na(data$diff_temp)] <- 0
  
  var = data %>% group_by(Year, Month) %>% summarise (sigma = sum(diff_temp**2 )/(n()+1))
  setDT(var)
  data  = merge(data, var, by = c('Year','Month'))
  
  
  #Compute Yi for each day in the dataset
  #mean temperature
  data$Tmi = coef[1] + coef[2]*data$timestep + coef[3]*sin(w*data$timestep + coef[4])
  data$Yi = (data$Tmi - data$AvgTemperature)/data$sigma
  
  
  #Given Tjm, Tj and Yi, compute mean reversion factor an
  data$Yilag = lag(data$Yi)
  data$Tmilag = lag(data$Tmi)
  data$Templag = lag(data$AvgTemperature)
  data[is.na(data)] <- 0
  
  an = -log(sum(data$Yilag * (data$AvgTemperature - data$Tmi))/sum(data$Yilag * (data$Templag - data$Tmilag)))
  
  
  #Compute variance_mu for each month involving mean reversion a
  variance_mu = data %>% group_by(Year, Month) %>% summarise(variance_mu = sum((AvgTemperature - Tmi + Tmilag - an*Tmilag - (1-an)*Templag)**2)/(n()-2))
  setDT(variance_mu)
  
  variance_mu$a = an
  variance_mu$sigma = var$sigma 
  
  return(variance_mu)
}


#################
#Simulation function
#################
run_simulations <- function(n, city, variance, a, T0, coef, w, nb){
  
  #days of the year 2020/2021
  
  timesteps <- 9497:9862
  for (year in 1:n){
    simulation_year(timesteps, city, variance, a, T0, coef, w, nb, year)
  }
}

simulation_year <- function(timesteps, city, variance, a, T0, coef, w, nb, year ){

  simulated_temp = c()
  simulated_date = c()
  final_prediction <- data.frame(matrix(0, ncol = 0, nrow = length(timesteps)))
  
  final_prediction$timesteps = timesteps
  final_prediction$Tj_m = coef[1] + coef[2]*timesteps + coef[3]*(sin(w*timesteps + coef[4]))
  
  #initialize Tj
  Tj = T0
  #initialize date
  date <- as.Date("2021-01-01")
  
  for (k in 1:length(timesteps)){
    Tj_m = final_prediction$Tj_m[k]
    Tj1_m = final_prediction$Tj_m[k+1]
    
    sigma = variance$value[variance$Month == month(date)]
    epsilon = rnorm(1,0,1)
    
    Tj1 = (1-a)*(Tj_m - Tj) + Tj1_m + sigma*epsilon
    
    simulated_temp = c(simulated_temp, Tj1)
    simulated_date = as.Date(c(simulated_date, date))
    Tj = Tj1
    date = date + 1 
  }
  
  final_prediction$temp = simulated_temp
  final_prediction$date = as.Date(simulated_date)
  fwrite(final_prediction, paste0("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature/run/", nb, "/simulation_", city, "_",year, ".csv"), sep = ";", dec = ",")
}

#################
#Prices function
#################
price_instrument <- function(Temp, path, period_start, period_end, option){
  list_run = list.files(paste0("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature/run/", path))
  final = c()
  if (option == "HDD"){
    for ( i in list_run){
      df =  fread(paste0("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature/run/",path, i), dec = ",")
      df = filter(df, (date >= period_start & date <= period_end))
      df = mutate(df, HDD = Temp - temp) 
      df$HDD[df$HDD < 0] <- 0
      value = sum(df$HDD)
      final = c(final, value)
    }
  }
  if (option == "CDD"){
    for ( i in list_run){
      df =  fread(paste0("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature/run/",path, i), dec = ",")
      df = filter(df, (date >= period_start & date <= period_end))
      df = mutate(df, CDD = temp - Temp) 
      df$CDD[df$CDD < 0] <- 0
      value = sum(df$CDD)
      final = c(final, value)
    }
  }
  return(final)
}
data_instrument <- function(values, K){
  
  values = values - K
  values[values < 0] <- 0
  
  EL = mean(values)
  quantiles = quantile(values, c(0.8, 0.9, 0.95,0.99))
  
  quantiles = c(EL, quantiles)
  return(quantiles)
}



#################
#Collateral function
#################

get_histo_correlation <- function(city1, city2, period_start, period_end, options, structures){
  df_1 = get_city_temperatures(city1)
  df_2 = get_city_temperatures(city2)
  df_1$temp = df_1$AvgTemperature
  df_2$temp = df_2$AvgTemperature
  
  #define days correctly
  df_1$date = as.Date(paste0(df_1$Year,"-",df_1$Month, "-", df_1$Day))
  df_2$date = as.Date(paste0(df_2$Year,"-",df_2$Month, "-", df_2$Day))
  
  #filter the period
  
  df_1 = filter(df_1, date >= period_start[1], date <= period_end[1])
  df_2 = filter(df_2, date >= period_start[2], date <= period_end[2])
  
  #HDD 
  
  if (option[1] == "HDD"){
    df_1 = mutate(df_1, DD = structures[1] - temp) 
    df_1$DD[df_1$HDD < 0] <- 0
  } else{df_1 = mutate(df_1, DD = temp - structures[1]) 
  df_1$DD[df_1$DD < 0] <- 0 }
  
  if (option[2] == "HDD"){
    df_2 = mutate(df_2, DD = structures[2] - temp) 
    df_2$DD[df_2$DD < 0] <- 0
  } else{df_2 = mutate(df_2, DD = temp - structures[2]) 
  df_2$DD[df_2$DD < 0] <- 0 }

  final = cov(df_1$DD, df_2$DD,method = c("pearson", "kendall", "spearman"))
  
  return(final)
}


get_modelled_correlation <- function(city1, city2){
}


get_fund_correlation <- function(positions){
}