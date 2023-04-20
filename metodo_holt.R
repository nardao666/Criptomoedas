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
# Sem processo de limpeza
bc.h <- holt(bc, h=100)
pc.h <- holt(pc, h=100)
lc.h <- holt(lc, h=100)
mc.h <- holt(mc, h=100)



## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.h,lc_test)
print("BC")
forecast::accuracy(bc.h,bc_test)
print("PC")
forecast::accuracy(pc.h,pc_test)
print("MC")
forecast::accuracy(mc.h,mc_test)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.h$residuals,"Litecoin")
plotForecastErrors(bc.h$residuals,"Bitcoin")
plotForecastErrors(pc.h$residuals,"Peercoin")
plotForecastErrors(mc.h$residuals,"Namecoin")
dev.off()



## -------------------------------------------------------------------------------------------------------------------------------
lc.diff = diff(lc ,differences = 1)
bc.diff = diff(bc ,differences = 1)
pc.diff = diff(pc ,differences = 1)
mc.diff = diff(mc ,differences = 1)

lc_test.diff = diff(lc_test ,differences = 1)
bc_test.diff = diff(lc_test ,differences = 1)
pc_test.diff = diff(lc_test ,differences = 1)
mc_test.diff = diff(lc_test ,differences = 1)


## -------------------------------------------------------------------------------------------------------------------------------
# Sem processo de limpeza
bc.diff.h <- holt(bc.diff, h=100)
pc.diff.h <- holt(pc.diff, h=100)
lc.diff.h <- holt(lc.diff, h=100)
mc.diff.h <- holt(mc.diff, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.diff.h,lc_test.diff)
print("BC")
forecast::accuracy(bc.diff.h,bc_test.diff)
print("PC")
forecast::accuracy(pc.diff.h,pc_test.diff)
print("MC")
forecast::accuracy(mc.diff.h,mc_test.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.h$residuals,"Litecoin")
plotForecastErrors(bc.h$residuals,"Bitcoin")
plotForecastErrors(pc.h$residuals,"Peercoin")
plotForecastErrors(mc.h$residuals,"Namecoin")
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
forecast::ndiffs(lc.log)
forecast::ndiffs(bc.log)
forecast::ndiffs(pc.log)
forecast::ndiffs(mc.log)


## -------------------------------------------------------------------------------------------------------------------------------
# Sem processo de limpeza
bc.log.h <- holt(bc.log, h=100)
pc.log.h <- holt(pc.log, h=100)
lc.log.h <- holt(lc.log, h=100)
mc.log.h <- holt(mc.log, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt_log.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.h,lc_test.log)
print("BC")
forecast::accuracy(bc.log.h,bc_test.log)
print("PC")
forecast::accuracy(pc.log.h,pc_test.log)
print("MC")
forecast::accuracy(mc.log.h,mc_test.log)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_log_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.h$residuals,"Litecoin")
plotForecastErrors(bc.log.h$residuals,"Bitcoin")
plotForecastErrors(pc.log.h$residuals,"Peercoin")
plotForecastErrors(mc.log.h$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.log.diff = diff(lc.log ,differences = 1)
bc.log.diff = diff(bc.log ,differences = 1)
pc.log.diff = diff(pc.log ,differences = 1)
mc.log.diff = diff(mc.log ,differences = 1)

lc_test.log.diff <- diff(lc_test.log, differences = 1)
bc_test.log.diff <- diff(bc_test.log, differences = 1)
pc_test.log.diff <- diff(pc_test.log, differences = 1)
mc_test.log.diff <- diff(mc_test.log, differences = 1)


## -------------------------------------------------------------------------------------------------------------------------------
forecast::ndiffs(lc.log.diff)
forecast::ndiffs(bc.log.diff)
forecast::ndiffs(pc.log.diff)
forecast::ndiffs(mc.log.diff)


## -------------------------------------------------------------------------------------------------------------------------------
# Sem processo de limpeza
bc.log.diff.h <- holt(bc.log.diff, h=100)
pc.log.diff.h <- holt(pc.log.diff, h=100)
lc.log.diff.h <- holt(lc.log.diff, h=100)
mc.log.diff.h <- holt(mc.log.diff, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt_log_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.log.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.log.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.log.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.log.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.log.diff.h,lc_test.log.diff)
print("BC")
forecast::accuracy(bc.log.diff.h,bc_test.log.diff)
print("PC")
forecast::accuracy(pc.log.diff.h,pc_test.log.diff)
print("MC")
forecast::accuracy(mc.log.diff.h,mc_test.log.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_log_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.log.diff.h$residuals,"Litecoin")
plotForecastErrors(bc.log.diff.h$residuals,"Bitcoin")
plotForecastErrors(pc.log.diff.h$residuals,"Peercoin")
plotForecastErrors(mc.log.diff.h$residuals,"Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
lc.lambda = BoxCox.lambda(lc[1:length(lc)])
bc.lambda = BoxCox.lambda(bc[1:length(bc)])
pc.lambda = BoxCox.lambda(pc[1:length(pc)])
mc.lambda = BoxCox.lambda(mc[1:length(mc)])

lc_test.lambda = BoxCox.lambda(lc_test[1:length(lc_test)])
bc_test.lambda = BoxCox.lambda(bc_test[1:length(bc_test)])
pc_test.lambda = BoxCox.lambda(pc_test[1:length(pc_test)])
mc_test.lambda = BoxCox.lambda(mc_test[1:length(mc_test)])


## -------------------------------------------------------------------------------------------------------------------------------
print(lc.lambda)
print(bc.lambda)
print(pc.lambda)
print(mc.lambda)


## -------------------------------------------------------------------------------------------------------------------------------
lc.boxcox <- BoxCox(lc, lambda = lc.lambda)
bc.boxcox <- BoxCox(bc, lambda = bc.lambda)
pc.boxcox <- BoxCox(pc, lambda = pc.lambda)
mc.boxcox <- BoxCox(mc, lambda = mc.lambda)

lc_test.boxcox <- BoxCox(lc_test, lambda = lc_test.lambda)
bc_test.boxcox <- BoxCox(bc_test, lambda = bc_test.lambda)
pc_test.boxcox <- BoxCox(pc_test, lambda = pc_test.lambda)
mc_test.boxcox <- BoxCox(mc_test, lambda = mc_test.lambda)


## -------------------------------------------------------------------------------------------------------------------------------
# Sem processo de limpeza
bc.boxcox.h <- holt(bc.boxcox, h=100)
pc.boxcox.h <- holt(pc.boxcox, h=100)
lc.boxcox.h <- holt(lc.boxcox, h=100)
mc.boxcox.h <- holt(mc.boxcox, h=100)


## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt_boxcox.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.h,lc_test.boxcox)
print("BC")
forecast::accuracy(bc.boxcox.h,bc_test.boxcox)
print("PC")
forecast::accuracy(pc.boxcox.h,pc_test.boxcox)
print("MC")
forecast::accuracy(mc.boxcox.h,mc_test.boxcox)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_boxcox_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.h$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.h$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.h$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.h$residuals,"Namecoin")
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
# Sem processo de limpeza
bc.boxcox.diff.h <- holt(bc.boxcox.diff, h=100)
pc.boxcox.diff.h <- holt(pc.boxcox.diff, h=100)
lc.boxcox.diff.h <- holt(lc.boxcox.diff, h=100)
mc.boxcox.diff.h <- holt(mc.boxcox.diff, h=100)
## -------------------------------------------------------------------------------------------------------------------------------
tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt_boxcox_diff.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(5,5,2.5,2),oma=c(0,0,0,0))
plot(lc.boxcox.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Litecoin")
plot(bc.boxcox.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Bitcoin")
plot(pc.boxcox.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Peercoin")
plot(mc.boxcox.diff.h, xlab = "ANO", ylab= "CLOSE (USD)", main = "Namecoin")
dev.off()

## -------------------------------------------------------------------------------------------------------------------------------
print("LC")
forecast::accuracy(lc.boxcox.diff.h,lc_test.boxcox.diff)
print("BC")
forecast::accuracy(bc.boxcox.diff.h,bc_test.boxcox.diff)
print("PC")
forecast::accuracy(pc.boxcox.diff.h,pc_test.boxcox.diff)
print("MC")
forecast::accuracy(mc.boxcox.diff.h,mc_test.boxcox.diff)

tiff("C:/Users/leonardoaag/Desktop/TCC/imagens/holt/holt_boxcox_diff_residuos.png", units = "px", width=7000, height=4000, res = 600)
op <- par(mfrow = c(2,2), mar=c(2.5,4,5,2),oma=c(0,0,0,0))
plotForecastErrors(lc.boxcox.diff.h$residuals,"Litecoin")
plotForecastErrors(bc.boxcox.diff.h$residuals,"Bitcoin")
plotForecastErrors(pc.boxcox.diff.h$residuals,"Peercoin")
plotForecastErrors(mc.boxcox.diff.h$residuals,"Namecoin")
dev.off()
