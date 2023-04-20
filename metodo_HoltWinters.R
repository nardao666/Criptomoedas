## ----include=FALSE--------------------------------------------------------------------------------------------------------------
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

## -------------------------------------------------------------------------------------------------------------------------------
LTC = read.csv('./LTC-USD.csv')
BTC = read.csv('./BTC-USD.csv')
PPC = read.csv('./PPC-USD.csv')
NMC = read.csv('./NMC-USD.csv')


## -------------------------------------------------------------------------------------------------------------------------------
LTC_TEST = read.csv('./LTC-TEST.csv')
PPC_TEST = read.csv('./PPC-TEST.csv')
NMC_TEST = read.csv('./NMC-TEST.csv')


## -------------------------------------------------------------------------------------------------------------------------------
NEW_LTC <- rbind(LTC,LTC_TEST)
NEW_PPC <- rbind(PPC,PPC_TEST)
NEW_NMC <- rbind(NMC,NMC_TEST)


## -------------------------------------------------------------------------------------------------------------------------------
NEW_LTC <- NEW_LTC[!duplicated(NEW_LTC),]
NEW_PPC <- NEW_PPC[!duplicated(NEW_PPC),]
NEW_NMC <- NEW_NMC[!duplicated(NEW_NMC),]


## -------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------
lc_test <- window(lc_set,
         start = c(2018,12))

bc_test <- window(bc_set,
         start = c(2018,12))

pc_test <- window(pc_set,
         start = c(2018,12))

mc_test <- window(mc_set,
         start = c(2018,12))


## -------------------------------------------------------------------------------------------------------------------------------
ndiffs(lc)
ndiffs(bc)
ndiffs(pc)
ndiffs(mc)


## -------------------------------------------------------------------------------------------------------------------------------
lc.hw <- HoltWinters(lc, gamma=F, beta=F)
bc.hw <- HoltWinters(bc, gamma=F, beta=F)
pc.hw <- HoltWinters(pc, gamma=F, beta=F)
mc.hw <- HoltWinters(mc, gamma=F, beta=F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.forecast <- forecast(lc.hw,h=100)
bc.hw.forecast <- forecast(bc.hw,h=100)
pc.hw.forecast <- forecast(pc.hw,h=100)
mc.hw.forecast <- forecast(mc.hw,h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.hw.forecast,lc_test)
print("BC")
forecast::accuracy(bc.hw.forecast,bc_test)
print("PC")
forecast::accuracy(pc.hw.forecast,pc_test)
print("MC")
forecast::accuracy(mc.hw.forecast,mc_test)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.hw.forecast$residuals,"Litecoin")
plotForecastErrors(bc.hw.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.hw.forecast$residuals,"Peercoin")
plotForecastErrors(mc.hw.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.beta <- HoltWinters(lc, gamma=F, beta=T)
bc.hw.beta <- HoltWinters(bc, gamma=F, beta=T)
pc.hw.beta <- HoltWinters(pc, gamma=F, beta=T)
mc.hw.beta <- HoltWinters(mc, gamma=F, beta=T)


## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.beta.forecast <- forecast(lc.hw.beta,h=100)
bc.hw.beta.forecast <- forecast(bc.hw.beta,h=100)
pc.hw.beta.forecast <- forecast(pc.hw.beta,h=100)
mc.hw.beta.forecast <- forecast(mc.hw.beta,h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_beta.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.hw.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.hw.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.hw.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.hw.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.hw.beta.forecast,lc_test)
print("BC")
forecast::accuracy(bc.hw.beta.forecast,bc_test)
print("PC")
forecast::accuracy(pc.hw.beta.forecast,pc_test)
print("MC")
forecast::accuracy(mc.hw.beta.forecast,mc_test)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw__beta_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.hw.beta.forecast$residuals,"Litecoin")
plotForecastErrors(bc.hw.beta.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.hw.beta.forecast$residuals,"Peercoin")
plotForecastErrors(mc.hw.beta.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.gamma <- HoltWinters(lc, gamma = T, beta = F)
bc.hw.gamma <- HoltWinters(bc, gamma = T, beta = F)
pc.hw.gamma <- HoltWinters(pc, gamma = T, beta = F)
mc.hw.gamma <- HoltWinters(mc, gamma = T, beta = F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.gamma.forecast <- forecast(lc.hw.gamma, h=100)
bc.hw.gamma.forecast <- forecast(bc.hw.gamma, h=100)
pc.hw.gamma.forecast <- forecast(pc.hw.gamma, h=100)
mc.hw.gamma.forecast <- forecast(mc.hw.gamma, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_gamma.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.hw.gamma.forecast,lc_test)
print("BC")
forecast::accuracy(bc.hw.gamma.forecast,bc_test)
print("PC")
forecast::accuracy(pc.hw.gamma.forecast,pc_test)
print("MC")
forecast::accuracy(mc.hw.gamma.forecast,mc_test)  

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw__gamma_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.hw.gamma.forecast$residuals,"Litecoin")
plotForecastErrors(bc.hw.gamma.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.hw.gamma.forecast$residuals,"Peercoin")
plotForecastErrors(mc.hw.gamma.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.season.add <- HoltWinters(lc, gamma = F, beta = F, seasonal = 'additive')
bc.hw.season.add <- HoltWinters(bc, gamma = F, beta = F, seasonal = 'additive')
pc.hw.season.add <- HoltWinters(pc, gamma = F, beta = F, seasonal = 'additive')
mc.hw.season.add <- HoltWinters(mc, gamma = F, beta = F, seasonal = 'additive')


## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.season.add.forecast <- forecast(lc.hw.season.add, h=100)
bc.hw.season.add.forecast <- forecast(bc.hw.season.add, h=100)
pc.hw.season.add.forecast <- forecast(pc.hw.season.add, h=100)
mc.hw.season.add.forecast <- forecast(mc.hw.season.add, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_season_add.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("BC")
forecast::accuracy(lc.hw.season.add.forecast,lc_test)
print("BC")
forecast::accuracy(bc.hw.season.add.forecast,bc_test)
print("PC")
forecast::accuracy(pc.hw.season.add.forecast,pc_test)
print("MC")
forecast::accuracy(mc.hw.season.add.forecast,mc_test)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_season_add_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.hw.season.add.forecast$residuals,"Litecoin")
plotForecastErrors(bc.hw.season.add.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.hw.season.add.forecast$residuals,"Peercoin")
plotForecastErrors(mc.hw.season.add.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.season.mul <- HoltWinters(lc, gamma = F, beta = F, seasonal = 'multiplicative')
bc.hw.season.mul <- HoltWinters(bc, gamma = F, beta = F, seasonal = 'multiplicative')
pc.hw.season.mul <- HoltWinters(pc, gamma = F, beta = F, seasonal = 'multiplicative')
mc.hw.season.mul <- HoltWinters(mc, gamma = F, beta = F, seasonal = 'multiplicative')


## -------------------------------------------------------------------------------------------------------------------------------
lc.hw.season.mul.forecast <- forecast(lc.hw.season.mul, h=100)
bc.hw.season.mul.forecast <- forecast(bc.hw.season.mul, h=100)
pc.hw.season.mul.forecast <- forecast(pc.hw.season.mul, h=100)
mc.hw.season.mul.forecast <- forecast(mc.hw.season.mul, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_season_mult.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.hw.season.mul.forecast,lc_test)
print("BC")
forecast::accuracy(bc.hw.season.mul.forecast,bc_test)
print("PC")
forecast::accuracy(pc.hw.season.mul.forecast,pc_test)
print("MC")
forecast::accuracy(mc.hw.season.mul.forecast,mc_test)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_season_mul_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.hw.season.mul.forecast$residuals,"Litecoin")
plotForecastErrors(bc.hw.season.mul.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.hw.season.mul.forecast$residuals,"Peercoin")
plotForecastErrors(mc.hw.season.mul.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.log <- log(lc)
bc.log <- log(bc)
pc.log <- log(pc)
mc.log <- log(mc)

lc_test.log <- log(lc_test)
bc_test.log <- log(bc_test)
pc_test.log <- log(pc_test)
mc_test.log <- log(mc_test)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.hw <- HoltWinters(lc.log, gamma=F, beta=F)
bc.log.hw <- HoltWinters(bc.log, gamma=F, beta=F)
pc.log.hw <- HoltWinters(pc.log, gamma=F, beta=F)
mc.log.hw <- HoltWinters(mc.log, gamma=F, beta=F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.hw.forecast <- forecast(lc.log.hw,h=100)
bc.log.hw.forecast <- forecast(bc.log.hw,h=100)
pc.log.hw.forecast <- forecast(pc.log.hw,h=100)
mc.log.hw.forecast <- forecast(mc.log.hw,h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print('LC')
forecast::accuracy(lc.log.hw.forecast,lc_test.log)
print('BC')
forecast::accuracy(bc.log.hw.forecast,bc_test.log)
print('PC')
forecast::accuracy(pc.log.hw.forecast,pc_test.log)
print('MC')
forecast::accuracy(mc.log.hw.forecast,mc_test.log)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.hw.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.hw.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.hw.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.hw.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.log.hw.gamma <- HoltWinters(lc.log, gamma = T, beta = F)
bc.log.hw.gamma <- HoltWinters(bc.log, gamma = T, beta = F)
pc.log.hw.gamma <- HoltWinters(pc.log, gamma = T, beta = F)
mc.log.hw.gamma <- HoltWinters(mc.log, gamma = T, beta = F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.hw.gamma.forecast <- forecast(lc.log.hw.gamma, h=100)
bc.log.hw.gamma.forecast <- forecast(bc.log.hw.gamma, h=100)
pc.log.hw.gamma.forecast <- forecast(pc.log.hw.gamma, h=100)
mc.log.hw.gamma.forecast <- forecast(mc.log.hw.gamma, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_gamma.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.hw.gamma.forecast,lc_test.log)
print("BC")
forecast::accuracy(bc.log.hw.gamma.forecast,bc_test.log)
print("PC")
forecast::accuracy(pc.log.hw.gamma.forecast,pc_test.log)
print("MC")
forecast::accuracy(mc.log.hw.gamma.forecast,mc_test.log)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_gamma_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.hw.gamma.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.hw.gamma.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.hw.gamma.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.hw.gamma.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.log.hw.gamma.beta <- HoltWinters(lc.log, gamma = T, beta = T)
bc.log.hw.gamma.beta <- HoltWinters(bc.log, gamma = T, beta = T)
pc.log.hw.gamma.beta <- HoltWinters(pc.log, gamma = T, beta = T)
mc.log.hw.gamma.beta <- HoltWinters(mc.log, gamma = T, beta = T)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.hw.gamma.beta.forecast <- forecast(lc.log.hw.gamma.beta, h=100)
bc.log.hw.gamma.beta.forecast <- forecast(bc.log.hw.gamma.beta, h=100)
pc.log.hw.gamma.beta.forecast <- forecast(pc.log.hw.gamma.beta, h=100)
mc.log.hw.gamma.beta.forecast <- forecast(mc.log.hw.gamma.beta, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_gamma_beta.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_gamma_beta_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.hw.gamma.beta.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.hw.gamma.beta.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.hw.gamma.beta.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.hw.gamma.beta.forecast$residuals,"Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.hw.gamma.beta.forecast,lc_test.log)
print("LC")
forecast::accuracy(bc.log.hw.gamma.beta.forecast,bc_test.log)
print("LC")
forecast::accuracy(pc.log.hw.gamma.beta.forecast,pc_test.log)
print("LC")
forecast::accuracy(mc.log.hw.gamma.beta.forecast,mc_test.log)


## -------------------------------------------------------------------------------------------------------------------------------
ndiffs(lc.log)
ndiffs(bc.log)
ndiffs(pc.log)
ndiffs(mc.log)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff = diff(lc.log ,differences = 1)
bc.log.diff = diff(bc.log ,differences = 1)
pc.log.diff = diff(pc.log ,differences = 1)
mc.log.diff = diff(mc.log ,differences = 1)

lc_test.log.diff = diff(lc_test.log ,differences = 1)
bc_test.log.diff = diff(bc_test.log ,differences = 1)
pc_test.log.diff = diff(pc_test.log ,differences = 1)
mc_test.log.diff = diff(mc_test.log ,differences = 1)


## -------------------------------------------------------------------------------------------------------------------------------
ndiffs(lc.log.diff)
ndiffs(bc.log.diff)
ndiffs(pc.log.diff)
ndiffs(mc.log.diff)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.hw <- HoltWinters(lc.log.diff, gamma=F, beta=F)
bc.log.diff.hw <- HoltWinters(bc.log.diff, gamma=F, beta=F)
pc.log.diff.hw <- HoltWinters(pc.log.diff, gamma=F, beta=F)
mc.log.diff.hw <- HoltWinters(mc.log.diff, gamma=F, beta=F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.hw.forecast <- forecast(lc.log.diff.hw,h=100)
bc.log.diff.hw.forecast <- forecast(bc.log.diff.hw,h=100)
pc.log.diff.hw.forecast <- forecast(pc.log.diff.hw,h=100)
mc.log.diff.hw.forecast <- forecast(mc.log.diff.hw,h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.diff.hw.forecast,lc_test.log.diff)
print("LC")
forecast::accuracy(bc.log.diff.hw.forecast,bc_test.log.diff)
print("LC")
forecast::accuracy(pc.log.diff.hw.forecast,pc_test.log.diff)
print("LC")
forecast::accuracy(mc.log.diff.hw.forecast,mc_test.log.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.diff.hw.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.diff.hw.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.diff.hw.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.diff.hw.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.hw.gamma <- HoltWinters(lc.log.diff, gamma = T, beta = F)
bc.log.diff.hw.gamma <- HoltWinters(bc.log.diff, gamma = T, beta = F)
pc.log.diff.hw.gamma <- HoltWinters(pc.log.diff, gamma = T, beta = F)
mc.log.diff.hw.gamma <- HoltWinters(mc.log.diff, gamma = T, beta = F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.hw.gamma.forecast <- forecast(lc.log.diff.hw.gamma, h=100)
bc.log.diff.hw.gamma.forecast <- forecast(bc.log.diff.hw.gamma, h=100)
pc.log.diff.hw.gamma.forecast <- forecast(pc.log.diff.hw.gamma, h=100)
mc.log.diff.hw.gamma.forecast <- forecast(mc.log.diff.hw.gamma, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_diff_gamma.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.diff.hw.gamma.forecast,lc_test.log.diff)
print("LC")
forecast::accuracy(bc.log.diff.hw.gamma.forecast,bc_test.log.diff)
print("LC")
forecast::accuracy(pc.log.diff.hw.gamma.forecast,pc_test.log.diff)
print("LC")
forecast::accuracy(mc.log.diff.hw.gamma.forecast,mc_test.log.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_diff_gamma_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.diff.hw.gamma.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.diff.hw.gamma.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.diff.hw.gamma.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.diff.hw.gamma.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.hw.gamma.beta <- HoltWinters(lc.log.diff, gamma = T, beta = T)
bc.log.diff.hw.gamma.beta <- HoltWinters(bc.log.diff, gamma = T, beta = T)
pc.log.diff.hw.gamma.beta <- HoltWinters(pc.log.diff, gamma = T, beta = T)
mc.log.diff.hw.gamma.beta <- HoltWinters(mc.log.diff, gamma = T, beta = T)


## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff.hw.gamma.beta.forecast <- forecast(lc.log.diff.hw.gamma.beta, h=100)
bc.log.diff.hw.gamma.beta.forecast <- forecast(bc.log.diff.hw.gamma.beta, h=100)
pc.log.diff.hw.gamma.beta.forecast <- forecast(pc.log.diff.hw.gamma.beta, h=100)
mc.log.diff.hw.gamma.beta.forecast <- forecast(mc.log.diff.hw.gamma.beta, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_diff_gamma_beta.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.diff.hw.gamma.beta.forecast,lc_test.log.diff)
print("LC")
forecast::accuracy(bc.log.diff.hw.gamma.beta.forecast,bc_test.log.diff)
print("LC")
forecast::accuracy(pc.log.diff.hw.gamma.beta.forecast,pc_test.log.diff)
print("LC")
forecast::accuracy(mc.log.diff.hw.gamma.beta.forecast,mc_test.log.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_log_diff_gamma_beta_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.diff.hw.gamma.beta.forecast$residuals,"Litecoin")
plotForecastErrors(bc.log.diff.hw.gamma.beta.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.log.diff.hw.gamma.beta.forecast$residuals,"Peercoin")
plotForecastErrors(mc.log.diff.hw.gamma.beta.forecast$residuals,"Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
# lc.t = BoxCox.ar(lc, method='yule-walker')
# bc.t = BoxCox.ar(bc, method='yule-walker')
# pc.t = BoxCox.ar(pc, method='yule-walker')
# mc.t = BoxCox.ar(mc, method='yule-walker')
# 
# 
# ## -------------------------------------------------------------------------------------------------------------------------------
# lc.t$ci
# bc.t$ci
# pc.t$ci
# mc.t$ci


## -------------------------------------------------------------------------------------------------------------------------------
lc.lambda = BoxCox.lambda(lc[1:length(lc)])
bc.lambda = BoxCox.lambda(bc[1:length(bc)])
pc.lambda = BoxCox.lambda(pc[1:length(pc)])
mc.lambda = BoxCox.lambda(mc[1:length(mc)])


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox <- BoxCox(lc, lambda = BoxCox.lambda(lc[1:length(lc)]))
bc.boxcox <- BoxCox(bc, lambda = BoxCox.lambda(bc[1:length(bc)]))
pc.boxcox <- BoxCox(pc, lambda = BoxCox.lambda(pc[1:length(pc)]))
mc.boxcox <- BoxCox(mc, lambda = BoxCox.lambda(mc[1:length(mc)]))

lc_test.boxcox <- BoxCox(lc_test, lambda = BoxCox.lambda(lc_test[1:length(lc_test)]))
bc_test.boxcox <- BoxCox(bc_test, lambda = BoxCox.lambda(bc_test[1:length(bc_test)]))
pc_test.boxcox <- BoxCox(pc_test, lambda = BoxCox.lambda(pc_test[1:length(pc_test)]))
mc_test.boxcox <- BoxCox(mc_test, lambda = BoxCox.lambda(mc_test[1:length(mc_test)]))


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.hw <- HoltWinters(lc.boxcox, gamma=F, beta=F)
bc.boxcox.hw <- HoltWinters(bc.boxcox, gamma=F, beta=F)
pc.boxcox.hw <- HoltWinters(pc.boxcox, gamma=F, beta=F)
mc.boxcox.hw <- HoltWinters(mc.boxcox, gamma=F, beta=F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.hw.forecast <- forecast(lc.boxcox.hw,h=100)
bc.boxcox.hw.forecast <- forecast(bc.boxcox.hw,h=100)
pc.boxcox.hw.forecast <- forecast(pc.boxcox.hw,h=100)
mc.boxcox.hw.forecast <- forecast(mc.boxcox.hw,h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.hw.forecast,lc_test.boxcox)
print("LC")
forecast::accuracy(bc.boxcox.hw.forecast,bc_test.boxcox)
print("LC")
forecast::accuracy(pc.boxcox.hw.forecast,pc_test.boxcox)
print("LC")
forecast::accuracy(mc.boxcox.hw.forecast,mc_test.boxcox)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.hw.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.hw.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.hw.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.hw.forecast$residuals,"Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.hw.gamma <- HoltWinters(lc.boxcox, gamma = T, beta = F)
bc.boxcox.hw.gamma <- HoltWinters(bc.boxcox, gamma = T, beta = F)
pc.boxcox.hw.gamma <- HoltWinters(pc.boxcox, gamma = T, beta = F)
mc.boxcox.hw.gamma <- HoltWinters(mc.boxcox, gamma = T, beta = F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.hw.gamma.forecast <- forecast(lc.boxcox.hw.gamma, h=100)
bc.boxcox.hw.gamma.forecast <- forecast(bc.boxcox.hw.gamma, h=100)
pc.boxcox.hw.gamma.forecast <- forecast(pc.boxcox.hw.gamma, h=100)
mc.boxcox.hw.gamma.forecast <- forecast(mc.boxcox.hw.gamma, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_gamma.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_gamma_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.hw.gamma.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.hw.gamma.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.hw.gamma.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.hw.gamma.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.hw.gamma.forecast,lc_test.boxcox)
print("LC")
forecast::accuracy(bc.boxcox.hw.gamma.forecast,bc_test.boxcox)
print("LC")
forecast::accuracy(pc.boxcox.hw.gamma.forecast,pc_test.boxcox)
print("LC")
forecast::accuracy(mc.boxcox.hw.gamma.forecast,mc_test.boxcox)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.hw.gamma.beta <- HoltWinters(lc.boxcox, gamma = T, beta = T)
bc.boxcox.hw.gamma.beta <- HoltWinters(bc.boxcox, gamma = T, beta = T)
pc.boxcox.hw.gamma.beta <- HoltWinters(pc.boxcox, gamma = T, beta = T)
mc.boxcox.hw.gamma.beta <- HoltWinters(mc.boxcox, gamma = T, beta = T)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.hw.gamma.beta.forecast <- forecast(lc.boxcox.hw.gamma.beta, h=100)
bc.boxcox.hw.gamma.beta.forecast <- forecast(bc.boxcox.hw.gamma.beta, h=100)
pc.boxcox.hw.gamma.beta.forecast <- forecast(pc.boxcox.hw.gamma.beta, h=100)
mc.boxcox.hw.gamma.beta.forecast <- forecast(mc.boxcox.hw.gamma.beta, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_gamma_beta.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.hw.gamma.beta.forecast,lc_test.boxcox)
print("LC")
forecast::accuracy(bc.boxcox.hw.gamma.beta.forecast,bc_test.boxcox)
print("LC")
forecast::accuracy(pc.boxcox.hw.gamma.beta.forecast,pc_test.boxcox)
print("LC")
forecast::accuracy(mc.boxcox.hw.gamma.beta.forecast,mc_test.boxcox)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_gamma_beta_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.hw.gamma.beta.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.hw.gamma.beta.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.hw.gamma.beta.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.hw.gamma.beta.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
ndiffs(lc.boxcox)
ndiffs(bc.boxcox)
ndiffs(pc.boxcox)
ndiffs(mc.boxcox)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff = diff(lc.boxcox ,differences = 1)
bc.boxcox.diff = diff(bc.boxcox ,differences = 1)
pc.boxcox.diff = diff(pc.boxcox ,differences = 1)
mc.boxcox.diff = diff(mc.boxcox ,differences = 1)

lc_test.boxcox.diff = diff(lc_test.boxcox ,differences = 1)
bc_test.boxcox.diff = diff(bc_test.boxcox ,differences = 1)
pc_test.boxcox.diff = diff(pc_test.boxcox ,differences = 1)
mc_test.boxcox.diff = diff(mc_test.boxcox ,differences = 1)


## -------------------------------------------------------------------------------------------------------------------------------
ndiffs(lc.boxcox.diff)
ndiffs(bc.boxcox.diff)
ndiffs(pc.boxcox.diff)
ndiffs(mc.boxcox.diff)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw <- HoltWinters(lc.boxcox.diff, gamma=F, beta=F)
bc.boxcox.diff.hw <- HoltWinters(bc.boxcox.diff, gamma=F, beta=F)
pc.boxcox.diff.hw <- HoltWinters(pc.boxcox.diff, gamma=F, beta=F)
mc.boxcox.diff.hw <- HoltWinters(mc.boxcox.diff, gamma=F, beta=F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.forecast <- forecast(lc.boxcox.diff.hw,h=100)
bc.boxcox.diff.hw.forecast <- forecast(bc.boxcox.diff.hw,h=100)
pc.boxcox.diff.hw.forecast <- forecast(pc.boxcox.diff.hw,h=100)
mc.boxcox.diff.hw.forecast <- forecast(mc.boxcox.diff.hw,h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.hw.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.hw.forecast,lc_test.boxcox.diff)
print("LC")
forecast::accuracy(bc.boxcox.diff.hw.forecast,bc_test.boxcox.diff)
print("LC")
forecast::accuracy(pc.boxcox.diff.hw.forecast,pc_test.boxcox.diff)
print("LC")
forecast::accuracy(mc.boxcox.diff.hw.forecast,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(mc.boxcox.diff.hw.forecast$residuals,"Litecoin")
plotForecastErrors(mc.boxcox.diff.hw.forecast$residuals,"Bitcoin")
plotForecastErrors(mc.boxcox.diff.hw.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.hw.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.gamma <- HoltWinters(lc.boxcox.diff, gamma = T, beta = F)
bc.boxcox.diff.hw.gamma <- HoltWinters(bc.boxcox.diff, gamma = T, beta = F)
pc.boxcox.diff.hw.gamma <- HoltWinters(pc.boxcox.diff, gamma = T, beta = F)
mc.boxcox.diff.hw.gamma <- HoltWinters(mc.boxcox.diff, gamma = T, beta = F)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.gamma.forecast <- forecast(lc.boxcox.diff.hw.gamma, h=100)
bc.boxcox.diff.hw.gamma.forecast <- forecast(bc.boxcox.diff.hw.gamma, h=100)
pc.boxcox.diff.hw.gamma.forecast <- forecast(pc.boxcox.diff.hw.gamma, h=100)
mc.boxcox.diff.hw.gamma.forecast <- forecast(mc.boxcox.diff.hw.gamma, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_gamma.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.hw.gamma.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.hw.gamma.forecast,lc_test.boxcox.diff)
print("BC")
forecast::accuracy(bc.boxcox.diff.hw.gamma.forecast,bc_test.boxcox.diff)
print("PC")
forecast::accuracy(pc.boxcox.diff.hw.gamma.forecast,pc_test.boxcox.diff)
print("MC")
forecast::accuracy(mc.boxcox.diff.hw.gamma.forecast,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff__gamma_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.diff.hw.gamma.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.diff.hw.gamma.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.diff.hw.gamma.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.hw.gamma.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.gamma.beta <- HoltWinters(lc.boxcox.diff, gamma = T, beta = T)
bc.boxcox.diff.hw.gamma.beta <- HoltWinters(bc.boxcox.diff, gamma = T, beta = T)
pc.boxcox.diff.hw.gamma.beta <- HoltWinters(pc.boxcox.diff, gamma = T, beta = T)
mc.boxcox.diff.hw.gamma.beta <- HoltWinters(mc.boxcox.diff, gamma = T, beta = T)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.gamma.beta.forecast <- forecast(lc.boxcox.diff.hw.gamma.beta, h=100)
bc.boxcox.diff.hw.gamma.beta.forecast <- forecast(bc.boxcox.diff.hw.gamma.beta, h=100)
pc.boxcox.diff.hw.gamma.beta.forecast <- forecast(pc.boxcox.diff.hw.gamma.beta, h=100)
mc.boxcox.diff.hw.gamma.beta.forecast <- forecast(mc.boxcox.diff.hw.gamma.beta, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_gamma_beta.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.hw.gamma.beta.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.hw.gamma.beta.forecast,lc_test.boxcox.diff)
print("BC")
forecast::accuracy(bc.boxcox.diff.hw.gamma.beta.forecast,bc_test.boxcox.diff)
print("PC")
forecast::accuracy(pc.boxcox.diff.hw.gamma.beta.forecast,pc_test.boxcox.diff)
print("MC")
forecast::accuracy(mc.boxcox.diff.hw.gamma.beta.forecast,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff__gamma_beta_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.diff.hw.gamma.beta.forecast$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.diff.hw.gamma.beta.forecast$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.diff.hw.gamma.beta.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.hw.gamma.beta.forecast$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.season.add <- HoltWinters(lc.boxcox.diff, gamma = F, beta = F, seasonal = 'additive')
bc.boxcox.diff.hw.season.add <- HoltWinters(bc.boxcox.diff, gamma = F, beta = F, seasonal = 'additive')
pc.boxcox.diff.hw.season.add <- HoltWinters(pc.boxcox.diff, gamma = F, beta = F, seasonal = 'additive')
mc.boxcox.diff.hw.season.add <- HoltWinters(mc.boxcox.diff, gamma = F, beta = F, seasonal = 'additive')


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.season.add.forecast <- forecast(lc.boxcox.diff.hw.season.add, h=100)
bc.boxcox.diff.hw.season.add.forecast <- forecast(bc.boxcox.diff.hw.season.add, h=100)
pc.boxcox.diff.hw.season.add.forecast <- forecast(pc.boxcox.diff.hw.season.add, h=100)
mc.boxcox.diff.hw.season.add.forecast <- forecast(mc.boxcox.diff.hw.season.add, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_season_add.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.hw.season.add.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()
## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.hw.season.add.forecast,lc_test.boxcox.diff)
print("BC")
forecast::accuracy(bc.boxcox.diff.hw.season.add.forecast,bc_test.boxcox.diff)
print("PC")
forecast::accuracy(pc.boxcox.diff.hw.season.add.forecast,pc_test.boxcox.diff)
print("MC")
forecast::accuracy(mc.boxcox.diff.hw.season.add.forecast,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_season_add_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(mc.boxcox.diff.hw.season.add.forecast$residuals,"Litecoin")
plotForecastErrors(mc.boxcox.diff.hw.season.add.forecast$residuals,"Bitcoin")
plotForecastErrors(mc.boxcox.diff.hw.season.add.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.hw.season.add.forecast$residuals,"Namecoin")
dev.off()

##-------------------------------------------------------------------------------
lc.boxcox.diff.hw.season.mul <- HoltWinters(lc.boxcox.diff, gamma = F, beta = F, seasonal = 'multiplicative')
bc.boxcox.diff.hw.season.mul <- HoltWinters(bc.boxcox.diff, gamma = F, beta = F, seasonal = 'multiplicative')
pc.boxcox.diff.hw.season.mul <- HoltWinters(pc.boxcox.diff, gamma = F, beta = F, seasonal = 'multiplicative')
mc.boxcox.diff.hw.season.mul <- HoltWinters(mc.boxcox.diff, gamma = F, beta = F, seasonal = 'multiplicative')


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox.diff.hw.season.mul.forecast <- forecast(lc.boxcox.diff.hw.season.mul, h=100)
bc.boxcox.diff.hw.season.mul.forecast <- forecast(bc.boxcox.diff.hw.season.mul, h=100)
pc.boxcox.diff.hw.season.mul.forecast <- forecast(pc.boxcox.diff.hw.season.mul, h=100)
mc.boxcox.diff.hw.season.mul.forecast <- forecast(mc.boxcox.diff.hw.season.mul, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_season_mul.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.hw.season.mul.forecast, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.hw.season.mul.forecast,lc_test.boxcox.diff)
print("BC")
forecast::accuracy(bc.boxcox.diff.hw.season.mul.forecast,bc_test.boxcox.diff)
print("PC")
forecast::accuracy(pc.boxcox.diff.hw.season.mul.forecast,pc_test.boxcox.diff)
print("MC")
forecast::accuracy(mc.boxcox.diff.hw.season.mul.forecast,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holtwinters/hw_boxcox_diff_season_mul_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(mc.boxcox.diff.hw.season.mul.forecast$residuals,"Litecoin")
plotForecastErrors(mc.boxcox.diff.hw.season.mul.forecast$residuals,"Bitcoin")
plotForecastErrors(mc.boxcox.diff.hw.season.mul.forecast$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.hw.season.mul.forecast$residuals,"Namecoin")
dev.off()

