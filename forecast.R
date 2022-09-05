library(smooth)#SMA
library(lubridate)#DATE
library(forecast)
library(tidyverse)

referrals <- read.csv("referrals.csv")
referrals$periodname <- as.character(referrals$periodname)
referrals$periodname <- as.Date(paste("01",referrals$periodname), format='%d %B %Y')

referrals_df <- referrals[,c(2,5,6)]
names(referrals_df)<- c("Time Period", "Ngelehun","Jembe")
summary(referrals_df)

#fill NA with column average
referrals_df$Ngelehun[is.na(referrals_df$Ngelehun)]<-mean(referrals_df$Ngelehun, na.rm = T)

########### SMA ############################################
#Ngelehun
Nge_SMA<-sma(referrals_df$Ngelehun, h=12, silent = F)
print(Nge_SMA)

Nge_actual_fitted<- data.frame(actual= referrals_df$Ngelehun,
                                fitted_values= Nge_SMA$fitted)
names(Nge_actual_fitted)
Nge_mse_mape <- Nge_actual_fitted[-1:-2,]  %>%
  gather(metric, value, Series.1) %>%
  group_by(metric) %>%
  summarise(MSE = mean((actual - value)^2, na.rm = TRUE),
            MAPE = mean(abs(actual - value)/actual*100,na.rm = TRUE))

#MAPE 124
Nge_forecasted <- data.frame(month= 1:12, values = Nge_SMA$forecast)


#Jembe

Jembe_SMA<-sma(referrals_df$Jembe, h=12, silent = F)
print(Jembe_SMA)
Jembe_SMA$forecast
Jembe_SMA$fitted
Jembe_actual_fitted <- data.frame(actual= referrals_df$Jembe,
                                  fitted= Jembe_SMA$fitted)
Jembe_mse_mape <- Jembe_actual_fitted %>%
  gather(metric, value, Series.1) %>%
  group_by(metric) %>%
  summarise(MSE = mean((actual - value)^2, na.rm = TRUE),
            MAPE = mean(abs((actual - value)/actual)*100), na.rm = TRUE)

#MAPE = 20.0
Jembe_forecasted <- data.frame(month= 1:12, values = Jembe_SMA$forecast)
print(Jembe_forecasted)

# the model uses the best moving  average based in the paremeters

########################Exponnetial smoothing ###############################
#1. simple exponential smoothing (data with no trend and seasonality) # SES attaches weighhts to obs. more weight is added for more recent obs. Forecast is done 
#with weighted averages, , where the weights decrease exponentially as observations come from further in the past. Large alpha, more weight on recent obs, small alpha
#more weight on past obs

Nguelen <- ts(referrals_df$Ngelehun, frequency = 1)
autoplot(Nguelen)
ses_ng <- ses(referrals_df$Ngelehun[-1:-2], alpha = 0.1) #0.1 alpha has the best MAPE value for SES

ses_ng_actual_fitted <- data.frame(actual= referrals_df$Ngelehun[-1:-2],
                                  fitted= ses_ng$fitted)
ses_ng_mape <- ses_ng_actual_fitted %>%
  gather(metric, value, fitted) %>%
  group_by(metric) %>%
  summarise(MSE = mean((actual - value)^2, na.rm = TRUE),
            MAPE = mean(abs(actual - value)/actual*100, na.rm = TRUE))
summary(ses_ng)
autoplot(ses_ng)+
  autolayer(ses_ng$fitted)
#MAPE = 83.3

Jembe <- ts(referrals_df$Jembe)
autoplot(Jembe)
ses_j <- ses(Jembe,alpha = 0.2) # 0.2 alpha has best MAPE
summary(ses_j)
autoplot(ses_j)+
  autolayer(ses_j$fitted)
#MAPE= 22.14

#All forecasts take the same value, equal to the last level component. 
#These forecasts will only be suitable if the time series has no trend or seasonal component.

#2. Holt's linear trend method
#It gives a constant upward or dowmward trend... beta is trend smooting parameter..close to 0, very smooth, close to 1, not very 

#NGuelen
holt_ng <- holt(referrals_df$Ngelehun[-1:-2],h = 12,alpha = 0.7)
summary(holt_ng)

holt_ng_actual_fitted <- data.frame(actual= referrals_df$Ngelehun,
                                   fitted= holt_ng$fitted)
holt_ng_mape <- holt_ng_actual_fitted %>%
  gather(metric, value, fitted) %>%
  group_by(metric) %>%
  summarise(MSE = mean((actual - value)^2, na.rm = TRUE),
            MAPE = mean(abs(!is.infinite((actual - value)/actual))*100), na.rm = TRUE)

autoplot(holt_ng)+
  autolayer(fitted(holt_ng)) #MAPE 83.4

holt_jembe<- holt(Jembe,h=12,alpha = 0.1)
summary(holt_jembe)


autoplot(holt_jembe)+
  autolayer(fitted(holt_jembe)) #MAPPE 22.5

#3. Holt Damped trend model 
#Gardner & McKenzie (1985) introduced a parameter that "dampens" the trend to a flat line some time in the future
#dampled contains phi which is the dampling parameter usually between 0 and 1. in practice its between 0.8 and 0.98. phi of 1 is same as holt linear method


holt_d_Ng <- holt(referrals_df$Ngelehun, damped = T,alpha = 0.1, h=12)#MAPE 116.95
summary(holt_d_Ng)

holt_ng_actual_fitted <- data.frame(actual= referrals_df$Ngelehun[-1:-2],
                                    fitted= holt_d_Ng$fitted)
holt_d_ng_mape <- holt_ng_actual_fitted %>%
  gather(metric, value, fitted) %>%
  group_by(metric) %>%
  summarise(MSE = mean((actual - value)^2, na.rm = TRUE),
            MAPE = mean(abs(!is.infinite((actual - value)/actual))*100), na.rm = TRUE)


autoplot(Nguelen) +
  autolayer(holt_d_Ng, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Months(1-12)") +
  ylab("Number of referrals Ngelehun ") +
  guides(colour=guide_legend(title="Forecast"))


holt_d_Jembe <- holt(Jembe, damped = T,alpha = 0.1) #MAPE 22.8
summary(holt_d_Jembe)

autoplot(Jembe) +
  autolayer(holt_jembe, series="Holt's method", PI=FALSE) +
  autolayer(holt_d_Jembe, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Months(1-12)") +
  ylab("Number of referrals for Jembe ") +
  guides(colour=guide_legend(title="Forecast"))
#MAPE= 22.8

#4. ETS (Error,Trend, Seasonality)

#Nguelen
ets_ng <- ets(referrals_df$Ngelehun[-1:-2])
summary(ets_ng)
autoplot(ets_ng)+
  autolayer(fitted(ets_ng),series = "fitted") #MAPE = 203.35


#Jembe
ets_jembe <- ets(Jembe)
summary(ets_jembe)
autoplot(ets_jembe)+
  autolayer(fitted(ets_jembe),series = "fitted") #MAPE = 23.7


#################################FORECAST RESULTS FOR NEXT 12 MONTHS

nguelen_forecast<- forecast(holt_d_Ng) %>% data.frame %>% select(1) #ES Holt Damped trend model , MAPE=116.95
nguelen_forecast$months<- rownames(nguelen_forecast)
rownames(nguelen_forecast)<- NULL
nguelen_forecast$months <-1:12

print(nguelen_forecast)

Jembe_forecasted <- data.frame(month= 1:12, values = Jembe_SMA$forecast) #SMA 5, MAPE=20.0
print(Jembe_forecasted)