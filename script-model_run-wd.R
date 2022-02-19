
setwd("C:/Users/octav/OneDrive/Desktop/Weather derivatives model - temperature/")
source("functions-wd.R")

######
## 0 Retrieve daily weather data
######

data = get_city_temperatures("Paris")

######
#1 Detrending
######s

coef = get_detrend(data)

######
#2 Seasonality
######

variance_mu = get_seasonality(data, coef)
variance_mu$std = sqrt(variance_mu$variance_mu)


#Average of top 5 highest values as seen in #4
variance  = variance_mu %>% group_by(Month) %>% summarise(value = mean(tail(sort(std), 8)))
setDT(variance)

######
#3 Montecarlo simulation
######

w = 2*pi/365
T0 = mean(data$AvgTemperature[data$Month == 1])
a = unique(variance_mu$a)

#simulation_year(timesteps, "Paris", variance, a, T0, coef, w, "folder1", 13)
run_simulations(100, "Paris", variance, a, T0, coef, w, "folder1")


###########
#4 Analysis Variance_months by year
###########

for (i in 1:12){
  plot(variance_mu$std[variance_mu$Month == i])
}

for (i in 1:12){
  plot(variance_mu$sigma[variance_mu$Month == i])
}

#we see a trend of higher variance in the last years which mean that we keep the mean of top  5 max sigma values by months for monte carlo

