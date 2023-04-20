## ----include=FALSE------------------------------------------------------------------------------------------------------------------------------------------
pacotes <- c("datasets","forecast","fpp2","tseries","patchwork", "DataCombine", "TTR",
             "seastests", "trend", "TSA", "caret", "urca", "fGarch", "e1071", "randtests",
             "graphics", "PerformanceAnalytics", "corrgram", "GGally","ggfortify","dplyr",
             "Metrics")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

plotForecastErrors <- function(forecastErrors, name_coin)
{
  forecastErrorsSd <- sd(x = forecastErrors,
                         na.rm = TRUE)
  forecastErrorsMin <- min(forecastErrors,
                           na.rm = TRUE) - forecastErrorsSd * 5
  forecastErrorsMax <- max(forecastErrors,
                           na.rm = TRUE) + forecastErrorsSd * 3
  forecastErrorsNorm <- rnorm(n = 10000,
                              mean = 0,
                              sd = forecastErrorsSd)
  binMin <- min(forecastErrorsMin, forecastErrorsNorm)
  binMax <- max(forecastErrorsMax, forecastErrorsNorm)
  binBreaks <- IQR(x = forecastErrors,
                   na.rm = TRUE) / 4
  bins <- seq(from = binMin,
              to = binMax,
              by = binBreaks)
  hist(x = forecastErrors,
       col = "#808080",
       freq = FALSE,
       main = paste("Residuos da",name_coin),
       breaks = bins)
  with(data = hist(x = forecastErrorsNorm,
                   plot = FALSE,
                   breaks = bins),
       expr = lines(x = mids,
                    y = density,
                    col = "#440154FF",
                    lwd = 3))
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
LTC = read.csv('./LTC-USD.csv')
BTC = read.csv('./BTC-USD.csv')
PPC = read.csv('./PPC-USD.csv')
NMC = read.csv('./NMC-USD.csv')


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
LTC_TEST = read.csv('./LTC-TEST.csv')
BTC_TEST = read.csv('./BTC-TEST.csv')
PPC_TEST = read.csv('./PPC-TEST.csv')
NMC_TEST = read.csv('./NMC-TEST.csv')


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
head(LTC_TEST)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
NEW_LTC <- rbind(LTC,LTC_TEST)
NEW_BTC <- rbind(BTC,BTC_TEST)
NEW_PPC <- rbind(PPC,PPC_TEST)
NEW_NMC <- rbind(NMC,NMC_TEST)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_set <- ts(NEW_LTC$Close,
         start = c(2014,9,17),
         frequency = 365)

bc_set <- ts(NEW_BTC$Close,
         start = c(2014,9,17),
         frequency = 365)

pc_set <- ts(NEW_PPC$Close,
         start = c(2014,9,17),
         frequency = 365)

mc_set <- ts(NEW_NMC$Close,
         start = c(2014,9,17),
         frequency = 365)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc <- window(lc_set,
         start = c(2014,9),
         end = c(2018,12))

bc <- window(bc_set,
         start = c(2014,9),
         end = c(2018,12))

pc <- window(pc_set,
         start = c(2014,9),
         end = c(2018,12))

mc <- window(mc_set,
         start = c(2014,9),
         end = c(2018,12))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tail(mc)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test <- window(lc_set,
         start = c(2018,12))

bc_test <- window(bc_set,
         start = c(2018,12))

pc_test <- window(pc_set,
         start = c(2018,12))

mc_test <- window(mc_set,
         start = c(2018,12))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ndiffs(lc)
ndiffs(bc)
ndiffs(pc)
ndiffs(mc)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.diff = diff(lc ,differences = 1)
bc.diff = diff(bc ,differences = 1)
pc.diff = diff(pc ,differences = 1)
mc.diff = diff(mc ,differences = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.diff = diff(lc_test ,differences = 1)
bc_test.diff = diff(bc_test ,differences = 1)
pc_test.diff = diff(pc_test ,differences = 1)
mc_test.diff = diff(mc_test ,differences = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.auto_arima <- forecast::auto.arima(lc, trace = T,stepwise = F, approximation = T, seasonal = T)
bc.auto_arima <- forecast::auto.arima(bc, trace = T,stepwise = F, approximation = T, seasonal = T)
pc.auto_arima <- forecast::auto.arima(pc, trace = T,stepwise = F, approximation = T, seasonal = T)
mc.auto_arima <- forecast::auto.arima(mc, trace = T,stepwise = F, approximation = T, seasonal = T)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.auto_arima.forecast = forecast(lc.auto_arima, h=100)
bc.auto_arima.forecast = forecast(bc.auto_arima, h=100)
pc.auto_arima.forecast = forecast(pc.auto_arima, h=100)
mc.auto_arima.forecast = forecast(mc.auto_arima, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.auto_arima
print("\n")
bc.auto_arima
print("\n")
pc.auto_arima
print("\n")
mc.auto_arima

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
mc.auto_arima$arma


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.auto_arima.forecast,lc_test)
print("BC")
forecast::accuracy(bc.auto_arima.forecast,bc_test)
print("PC")
forecast::accuracy(pc.auto_arima.forecast,pc_test)
print("MC")
forecast::accuracy(mc.auto_arima.forecast,mc_test)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/arima/arima.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/arima/arima_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.auto_arima.forecast$residuals,"Litecoin")
plotForecastErrors(bc.auto_arima.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.auto_arima.forecast$residuals,"Peercoin")
plotForecastErrors(mc.auto_arima.forecast$residuals,"Namecoin")
dev.off()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log = log(lc)
bc.log = log(bc)
pc.log = log(pc)
mc.log = log(mc)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.log <- log(lc_test)
bc_test.log <- log(bc_test)
pc_test.log <- log(pc_test)
mc_test.log <- log(mc_test)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.auto_arima <- forecast::auto.arima(lc.log, trace = T,stepwise = F, approximation = T, seasonal = F)
bc.log.auto_arima <- forecast::auto.arima(bc.log, trace = T,stepwise = F, approximation = T, seasonal = F)
pc.log.auto_arima <- forecast::auto.arima(pc.log, trace = T,stepwise = F, approximation = T, seasonal = F)
mc.log.auto_arima <- forecast::auto.arima(mc.log, trace = T,stepwise = F, approximation = T, seasonal = F)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.auto_arima.forecast = forecast(lc.log.auto_arima, h=100)
bc.log.auto_arima.forecast = forecast(bc.log.auto_arima, h=100)
pc.log.auto_arima.forecast = forecast(pc.log.auto_arima, h=100)
mc.log.auto_arima.forecast = forecast(mc.log.auto_arima, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.auto_arima.forecast,lc_test.log)
print("BC")
forecast::accuracy(bc.log.auto_arima.forecast,bc_test.log)
print("PC")
forecast::accuracy(pc.log.auto_arima.forecast,pc_test.log)
print("MC")
forecast::accuracy(mc.log.auto_arima.forecast,mc_test.log)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/arima/arima_log.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/arima/arima_log_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.auto_arima.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.auto_arima.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.auto_arima.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.auto_arima.forecast$residuals,"Namecoin")
dev.off()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox <- BoxCox(lc, lambda = BoxCox.lambda(lc[1:length(lc)]))
bc.boxcox <- BoxCox(bc, lambda = BoxCox.lambda(bc[1:length(bc)]))
pc.boxcox <- BoxCox(pc, lambda = BoxCox.lambda(pc[1:length(pc)]))
mc.boxcox <- BoxCox(mc, lambda = BoxCox.lambda(mc[1:length(mc)]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.lambda = BoxCox.lambda(lc_test[1:length(lc_test)])
bc_test.lambda = BoxCox.lambda(bc_test[1:length(bc_test)])
pc_test.lambda = BoxCox.lambda(pc_test[1:length(pc_test)])
mc_test.lambda = BoxCox.lambda(mc_test[1:length(mc_test)])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.boxcox <- BoxCox(lc_test, lambda = BoxCox.lambda(lc_test[1:length(lc_test)]))
bc_test.boxcox <- BoxCox(bc_test, lambda = BoxCox.lambda(bc_test[1:length(bc_test)]))
pc_test.boxcox <- BoxCox(pc_test, lambda = BoxCox.lambda(pc_test[1:length(pc_test)]))
mc_test.boxcox <- BoxCox(mc_test, lambda = BoxCox.lambda(mc_test[1:length(mc_test)]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.auto_arima <- forecast::auto.arima(lc.boxcox, trace = T,stepwise = F, approximation = T, seasonal = F)
bc.boxcox.auto_arima <- forecast::auto.arima(bc.boxcox, trace = T,stepwise = F, approximation = T, seasonal = F)
pc.boxcox.auto_arima <- forecast::auto.arima(pc.boxcox, trace = T,stepwise = F, approximation = T, seasonal = F)
mc.boxcox.auto_arima <- forecast::auto.arima(mc.boxcox, trace = T,stepwise = F, approximation = T, seasonal = F)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.auto_arima.forecast = forecast(lc.boxcox.auto_arima, h=100)
bc.boxcox.auto_arima.forecast = forecast(bc.boxcox.auto_arima, h=100)
pc.boxcox.auto_arima.forecast = forecast(pc.boxcox.auto_arima, h=100)
mc.boxcox.auto_arima.forecast = forecast(mc.boxcox.auto_arima, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.auto_arima.forecast,lc_test.boxcox)
print("BC")
forecast::accuracy(bc.boxcox.auto_arima.forecast,bc_test.boxcox)
print("PC")
forecast::accuracy(pc.boxcox.auto_arima.forecast,pc_test.boxcox)
print("MC")
forecast::accuracy(mc.boxcox.auto_arima.forecast,mc_test.boxcox)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/arima/arima_bocxox.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.auto_arima.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/arima/arima_boxcox_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.auto_arima.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.auto_arima.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.auto_arima.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.auto_arima.forecast$residuals,"Namecoin")
dev.off()


# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# print('LC')
# print(lc.result)
# print('BC')
# print(bc.result)
# print("\n")
# 
# print('PC')
# print(pc.result)
# print("\n")
# 
# print('MC')
# print(mc.result)

# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# lc.autoarima.full <- forecast::auto.arima(lc,stepwise = T,test = c("kpss", "adf", "pp"),
#                                       seasonal.test = c("seas", "ocsb", "hegy", "ch"))
# 
# bc.autoarima.full <- forecast::auto.arima(bc,stepwise = T,test = c("kpss", "adf", "pp"),
#                                       seasonal.test = c("seas", "ocsb", "hegy", "ch"))
# 
# pc.autoarima.full <- forecast::auto.arima(pc,stepwise = T,test = c("kpss", "adf", "pp"),
#                                       seasonal.test = c("seas", "ocsb", "hegy", "ch"))
# 
# mc.autoarima.full <- forecast::auto.arima(mc,stepwise = T,test = c("kpss", "adf", "pp"),
#                                       seasonal.test = c("seas", "ocsb", "hegy", "ch"))
# 
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# fit
# 
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# lc.autoarima.full.pred <- predict(lc.autoarima.full,n.ahead=100)
# bc.autoarima.full.pred <- predict(bc.autoarima.full,n.ahead=100)
# pc.autoarima.full.pred <- predict(pc.autoarima.full,n.ahead=100)
# mc.autoarima.full.pred <- predict(mc.autoarima.full,n.ahead=100)
# 
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# lc.autoarima.full.result <- forecast::accuracy(lc.autoarima.full.pred$pred,lc_test)
# bc.autoarima.full.result <- forecast::accuracy(bc.autoarima.full.pred$pred,bc_test)
# pc.autoarima.full.result <- forecast::accuracy(pc.autoarima.full.pred$pred,pc_test)
# mc.autoarima.full.result <- forecast::accuracy(mc.autoarima.full.pred$pred,mc_test)
# 
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# mc.result
# mc.autoarima.full.result
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# mc.auto_arima
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# mc.autoarima.full
# 
lc.boxcox.auto_arima
bc.boxcox.auto_arima
pc.boxcox.auto_arima
mc.boxcox.auto_arima

