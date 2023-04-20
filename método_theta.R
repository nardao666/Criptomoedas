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
PPC_TEST = read.csv('./PPC-TEST.csv')
NMC_TEST = read.csv('./NMC-TEST.csv')


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
NEW_LTC <- rbind(LTC,LTC_TEST)
NEW_PPC <- rbind(PPC,PPC_TEST)
NEW_NMC <- rbind(NMC,NMC_TEST)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
NEW_LTC <- NEW_LTC[!duplicated(NEW_LTC),]
NEW_PPC <- NEW_PPC[!duplicated(NEW_PPC),]
NEW_NMC <- NEW_NMC[!duplicated(NEW_NMC),]

op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plot(lc_set)
plot(bc_set)
plot(pc_set)
plot(mc_set)
dev.off()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_set <- ts(NEW_LTC$Close,
         start = c(2014,9,17),
         frequency = 365)

bc_set <- ts(BTC$Close,
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
lc_test <- window(lc_set,
         start = c(2018,12))

bc_test <- window(bc_set,
         start = c(2018,12),
         frequency = 365)

pc_test <- window(pc_set,
         start = c(2018,12))

mc_test <- window(mc_set,
         start = c(2018,12))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.t = thetaf(lc, h=100)
bc.t = thetaf(bc, h=100)
pc.t = thetaf(pc, h=100)
mc.t = thetaf(mc, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.t.forecast <- forecast(lc.t, h=100)
bc.t.forecast <- forecast(bc.t, h=100)
pc.t.forecast <- forecast(pc.t, h=100)
mc.t.forecast <- forecast(mc.t, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.t.forecast,lc_test)
print("BC")
forecast::accuracy(bc.t.forecast,bc_test)
print("PC")
forecast::accuracy(pc.t.forecast,pc_test)
print("MC")
forecast::accuracy(mc.t.forecast,mc_test)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.t.forecast$residuals,"Litecoin")
plotForecastErrors(bc.t.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.t.forecast$residuals,"Peercoin")
plotForecastErrors(mc.t.forecast$residuals,"Namecoin")
dev.off()


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
lc.diff.t <- thetaf(lc.diff, h=100)
bc.diff.t <- thetaf(bc.diff, h=100)
pc.diff.t <- thetaf(pc.diff, h=100)
mc.diff.t <- thetaf(mc.diff, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.diff.t.forecast <- forecast(lc.diff.t, h=100)
bc.diff.t.forecast <- forecast(bc.diff.t, h=100)
pc.diff.t.forecast <- forecast(pc.diff.t, h=100)
mc.diff.t.forecast <- forecast(mc.diff.t, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.diff.t.forecast$residuals,"Litecoin")
plotForecastErrors(bc.diff.t.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.diff.t.forecast$residuals,"Peercoin")
plotForecastErrors(mc.diff.t.forecast$residuals,"Namecoin")
dev.off()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.diff.t.forecast,lc_test.diff)
print("BC")
forecast::accuracy(bc.diff.t.forecast,bc_test.diff)
print("PC")
forecast::accuracy(pc.diff.t.forecast,pc_test.diff)
print("MC")
forecast::accuracy(mc.diff.t.forecast,mc_test.diff)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log <- log(lc)
bc.log <- log(bc)
pc.log <- log(pc)
mc.log <- log(mc)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.log <- log(lc_test)
bc_test.log <- log(bc_test)
pc_test.log <- log(pc_test)
mc_test.log <- log(mc_test)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.t <- thetaf(lc.log, h=100)
bc.log.t <- thetaf(bc.log, h=100)
pc.log.t <- thetaf(pc.log, h=100)
mc.log.t <- thetaf(mc.log, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.t.forecast <- forecast(lc.log.t, h=100)
bc.log.t.forecast <- forecast(bc.log.t, h=100)
pc.log.t.forecast <- forecast(pc.log.t, h=100)
mc.log.t.forecast <- forecast(mc.log.t, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_log.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.t.forecast,lc_test.log)
print("BC")
forecast::accuracy(bc.log.t.forecast,bc_test.log)
print("PC")
forecast::accuracy(pc.log.t.forecast,pc_test.log)
print("MC")
forecast::accuracy(mc.log.t.forecast,mc_test.log)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_log_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.t.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.t.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.t.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.t.forecast$residuals,"Namecoin")
dev.off()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.diff = diff(lc.log ,differences = 1)
bc.log.diff = diff(bc.log ,differences = 1)
pc.log.diff = diff(pc.log ,differences = 1)
mc.log.diff = diff(mc.log ,differences = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.log.diff = diff(lc_test.log ,differences = 1)
bc_test.log.diff = diff(bc_test.log ,differences = 1)
pc_test.log.diff = diff(pc_test.log ,differences = 1)
mc_test.log.diff = diff(mc_test.log ,differences = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.t <- thetaf(lc.log.diff, h=100)
bc.log.diff.t <- thetaf(bc.log.diff, h=100)
pc.log.diff.t <- thetaf(pc.log.diff, h=100)
mc.log.diff.t <- thetaf(mc.log.diff, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.t.forecast <- forecast(lc.log.diff.t, h=100)
bc.log.diff.t.forecast <- forecast(bc.log.diff.t, h=100)
pc.log.diff.t.forecast <- forecast(pc.log.diff.t, h=100)
mc.log.diff.t.forecast <- forecast(mc.log.diff.t, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_log_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.diff.t.forecast,lc_test.log.diff)
print("BC")
forecast::accuracy(bc.log.diff.t.forecast,bc_test.log.diff)
print("PC")
forecast::accuracy(pc.log.diff.t.forecast,pc_test.log.diff)
print("MC")
forecast::accuracy(mc.log.diff.t.forecast,mc_test.log.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_log_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.diff.t.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.diff.t.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.diff.t.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.diff.t.forecast$residuals,"Namecoin")
dev.off()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.lambda = BoxCox.lambda(lc[1:length(lc)])
bc.lambda = BoxCox.lambda(bc[1:length(bc)])
pc.lambda = BoxCox.lambda(pc[1:length(pc)])
mc.lambda = BoxCox.lambda(mc[1:length(mc)])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox <- BoxCox(lc, lambda = lc.lambda)
bc.boxcox <- BoxCox(bc, lambda = bc.lambda)
pc.boxcox <- BoxCox(pc, lambda = pc.lambda)
mc.boxcox <- BoxCox(mc, lambda = mc.lambda)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.lambda = BoxCox.lambda(lc_test[1:length(lc_test)])
bc_test.lambda = BoxCox.lambda(bc_test[1:length(bc_test)])
pc_test.lambda = BoxCox.lambda(pc_test[1:length(pc_test)])
mc_test.lambda = BoxCox.lambda(mc_test[1:length(mc_test)])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.boxcox <- BoxCox(lc_test, lambda = lc_test.lambda)
bc_test.boxcox <- BoxCox(bc_test, lambda = bc_test.lambda)
pc_test.boxcox <- BoxCox(pc_test, lambda = pc_test.lambda)
mc_test.boxcox <- BoxCox(mc_test, lambda = mc_test.lambda)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.t <- thetaf(lc.boxcox, h=100)
bc.boxcox.t <- thetaf(bc.boxcox, h=100)
pc.boxcox.t <- thetaf(pc.boxcox, h=100)
mc.boxcox.t <- thetaf(mc.boxcox, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.t.forecast <- forecast(lc.boxcox.t, h=100)
bc.boxcox.t.forecast <- forecast(bc.boxcox.t, h=100)
pc.boxcox.t.forecast <- forecast(pc.boxcox.t, h=100)
mc.boxcox.t.forecast <- forecast(mc.boxcox.t, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_boxcox.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.t.forecast,lc_test.boxcox)
print("BC")
forecast::accuracy(bc.boxcox.t.forecast,bc_test.boxcox)
print("PC")
forecast::accuracy(pc.boxcox.t.forecast,pc_test.boxcox)
print("MC")
forecast::accuracy(mc.boxcox.t.forecast,mc_test.boxcox)


tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_boxcox_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.t.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.t.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.t.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.t.forecast$residuals,"Namecoin")
dev.off()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff = diff(lc.boxcox ,differences = 1)
bc.boxcox.diff = diff(bc.boxcox ,differences = 1)
pc.boxcox.diff = diff(pc.boxcox ,differences = 1)
mc.boxcox.diff = diff(mc.boxcox ,differences = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc_test.boxcox.diff = diff(lc_test.boxcox ,differences = 1)
bc_test.boxcox.diff = diff(bc_test.boxcox ,differences = 1)
pc_test.boxcox.diff = diff(pc_test.boxcox ,differences = 1)
mc_test.boxcox.diff = diff(mc_test.boxcox ,differences = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.t <- thetaf(lc.boxcox.diff, h=100)
bc.boxcox.diff.t <- thetaf(bc.boxcox.diff, h=100)
pc.boxcox.diff.t <- thetaf(pc.boxcox.diff, h=100)
mc.boxcox.diff.t <- thetaf(mc.boxcox.diff, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.t.forecast <- forecast(lc.boxcox.diff.t, h=100)
bc.boxcox.diff.t.forecast <- forecast(bc.boxcox.diff.t, h=100)
pc.boxcox.diff.t.forecast <- forecast(pc.boxcox.diff.t, h=100)
mc.boxcox.diff.t.forecast <- forecast(mc.boxcox.diff.t, h=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_boxcox_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.t.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.t.forecast,lc_test.boxcox.diff)
print("BC")
forecast::accuracy(bc.boxcox.diff.t.forecast,bc_test.boxcox.diff)
print("PC")
forecast::accuracy(pc.boxcox.diff.t.forecast,pc_test.boxcox.diff)
print("MC")
forecast::accuracy(mc.boxcox.diff.t.forecast,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/theta/theta_boxcox_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(mc.boxcox.diff.t.forecast$residuals,"Litecoin")
plotForecastErrors(mc.boxcox.diff.t.forecast$residuals,"Bitcoin")
plotForecastErrors(mc.boxcox.diff.t.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.t.forecast$residuals,"Namecoin")
dev.off()







lc_set.lambda <-  BoxCox.lambda(lc_set[1:length(lc_set)])
lc_set.boxcox <- BoxCox(lc_set, lambda = lc_set.lambda)
lc_set.log <- log(lc_set)
lc_set.boxcox.diff = diff(lc_set.boxcox ,differences = 1)
lc_set.log.diff = diff(lc_set.log ,differences = 1)


op <- par(mfrow = c(3,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plot(head(lc_set, n= 1470))
plot(lc_set.boxcox)
plot(lc_set.log)
plot(lc_set.boxcox.diff)
plot(lc_set.log.diff)


dev.off()
op <- par(mfrow = c(3,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plot(head(lc_set.log, n= 1550), main='LOG')
plot(lc.log.hw.gamma.forecast)
plot(lc.log.h)
plot(lc.log.auto_arima.forecast)
plot(lc.log.t)




length(lc.boxcox.hw.forecast[7]$x)
lc.boxcox.diff.hw.forecast[3]
plot()

length()
lc.log.hw.gamma.forecast
