library(forecast)
library(readxl)
library(stats)
library(car)
library(moments)
data<-read_excel("C:/Users/ricar/Desktop/SEMESTRE 7/Series de Tiempo/5. Trabajos/preciocobre.xlsx")
cobre<-ts(data$precio,frequency=12,start=c(2008,1))
plot(cobre,xlab="Tiempo en meses",ylab="Precio del cobre refinado",
main="Precio mensual en USD/libra del cobre refinado entre 2008-2022")

#### ACF
acf(cobre,main="ACF de la serie del cobre refinado")

### TEST DICKEY FULLER AUMENTADO
adf.test(cobre) #no es estacionaria

#serie diferenciada
cobrediff<-ts(diff(cobre),frequency=12,start=c(2008,2))
cobrediff
plot(cobrediff,xlab="Tiempo en meses",ylab="Precio del cobre refinado diferenciado",
     main="Precio mensual en USD/libra de la serie diferenciada del cobre refinado entre 2008-2022",col="blue")

### ACF de la serie diferenciada
acf(cobrediff,main="ACF de la serie diferenciada del cobre refinado",lag.max=36) 
### ADF test
adf.test(cobrediff)

### ACF y PACF serie diferenciada para detectar parámetros AR y MA en modelo heurístico
par(mfrow=c(1,2))
acf(cobrediff,main="ACF de la serie diferenciada del cobre refinado",lag.max=40) 
pacf(cobrediff,main="PACF de la serie diferenciada del cobre refinado",ylab="PACF",lag.max=40)
## ARIMA(1,1,1)x(1,0,0)[12] y ARIMA(1,1,2)x(1,0,0)[12]


#### mejores modelos ARIMA
mejoresmodelos<-auto.arima(cobrediff,trace=T)
summary(mejoresmodelos)
#1. ARIMA(3,0,0)(2,0,0)[12]
#2. ARIMA(2,0,1)(2,0,0)[12]


## modelos propuestos
modelo1<-arima(cobre, order=c(3,1,0),seasonal=c(2,0,0),method="ML",include.mean=FALSE)
summary(modelo1) #AIC:1465.93 LOG-EMV:-726.97 MAE:11.70268 RMSE:16.12335 sigma2:260
modelo2<-arima(cobre, order=c(2,1,1),seasonal=c(2,0,0),method="ML",include.mean=FALSE)
summary(modelo2) #AIC:1467.26 LOG-EMV:-727.63 MAE:11.78073 RMSE:16.18605 sigma2:262

modeloheuristico1<-arima(cobre, order=c(1,1,1),seasonal=c(1,0,0),method="ML",include.mean=FALSE)
summary(modeloheuristico1) #AIC:1466.54 LOG-EMV:-729.27 MAE:12.06829 RMSE:16.37097 sigma2:268

modeloheuristico2<-arima(cobre, order=c(1,1,2),seasonal=c(1,0,0),method="ML",include.mean=FALSE)
summary(modeloheuristico2) #AIC:1472.82 Log-EMV:-731.41 MAE:12.04673 RMSE:16.51157 sigma2:272.6


# residuos modelos
par(mfrow=c(2,2))
plot(residuals(modelo1),type="p",xlab="Tiempo en meses",main="Residuos del modelo ARIMA(3,1,0)(2,0,0)",
     ylab="Residuos")
abline(h=0,col="red",lwd=2)
plot(residuals(modelo2),type="p",xlab="Tiempo en meses",main="Residuos del modelo ARIMA(2,1,1)(2,0,0)",
     ylab="Residuos")
abline(h=0,col="red",lwd=2)
plot(residuals(modeloheuristico1),type="p",xlab="Tiempo en meses",main="Residuos del modelo heurístico ARIMA(1,1,1)(1,0,0)",
     ylab="Residuos")
abline(h=0,col="red",lwd=2)
plot(residuals(modeloheuristico2),type="p",xlab="Tiempo en meses",main="Residuos del modelo heurístico ARIMA(1,1,2)(1,0,0)",
     ylab="Residuos")
abline(h=0,col="red",lwd=2)

#acf de residuos
par(mfrow=c(2,2))
acf(residuals(modelo1),main="ACF de los residuos del modelo ARIMA(3,1,0)x(2,0,0)[12]")
acf(residuals(modelo2),main="ACF de los residuos del modelo ARIMA(2,1,1)x(2,0,0)[12]")
acf(residuals(modeloheuristico1),main="ACF de los residuos del modelo heurístico ARIMA(1,1,1)x(1,0,0)[12]")
acf(residuals(modeloheuristico2),main="ACF de los residuos del modelo heurístico ARIMA(1,1,2)x(1,0,0)[12]")

### LJUNG-BOX
BT1 <- rep(NA,60)
for(i in 1:60){
  BT1[i]=Box.test(residuals(modelo1), lag = i,   #### P-valores del test Ljung-Box
                  type = "Ljung-Box", fitdf = 0)$p.value
}
BT2 <- rep(NA,60)
for(i in 1:60){
  BT2[i]=Box.test(residuals(modelo2), lag = i,   #### P-valores del test Ljung-Box
                  type = "Ljung-Box", fitdf = 0)$p.value
}
BT3 <- rep(NA,60)
for(i in 1:60){
  BT3[i]=Box.test(residuals(modeloheuristico1), lag = i,   #### P-valores del test Ljung-Box
                  type = "Ljung-Box", fitdf = 0)$p.value
}
BT4 <- rep(NA,60)
for(i in 1:60){
  BT4[i]=Box.test(residuals(modeloheuristico2), lag = i,   #### P-valores del test Ljung-Box
                  type = "Ljung-Box", fitdf = 0)$p.value
}
par(mfrow=c(2,2))
plot(BT1, ylim=c(0,1),main="P-valores test Ljung-Box para residuos de ARIMA(3,1,0)x(2,0,0)[12]",
     ylab="P-valores",xlab="Lag")
abline(h=0.05, col="blue", lty=2)
plot(BT2, ylim=c(0,1),main="P-valores test Ljung-Box para residuos de ARIMA(2,1,1)x(2,0,0)[12]",
     ylab="P-valores",xlab="Lag")
abline(h=0.05, col="blue", lty=2)
plot(BT3, ylim=c(0,1),main="P-valores test Ljung-Box para residuos de ARIMA(1,1,1)x(1,0,0)[12]",
     ylab="P-valores",xlab="Lag")
abline(h=0.05, col="blue", lty=2)
plot(BT4, ylim=c(0,1),main="P-valores test Ljung-Box para residuos de ARIMA(1,1,2)x(1,0,0)[12]",
     ylab="P-valores",xlab="Lag")
abline(h=0.05, col="blue", lty=2)


#contraste de normalidad, histograma:
par(mfrow=c(2,2))
hist(residuals(modelo1),breaks=20,xlab="Residuos del modelo",
     ylab="Frecuencia",
     main="Histograma de residuos del modelo ARIMA(3,1,0)x(2,0,0)[12]")
hist(residuals(modelo2),breaks=20,xlab="Residuos del modelo",
     ylab="Frecuencia",
     main="Histograma de residuos del modelo ARIMA(2,1,1)x(2,0,0)[12]")
hist(residuals(modeloheuristico1),breaks=20,xlab="Residuos del modelo",
     ylab="Frecuencia",
     main="Histograma de residuos del modelo heurístico ARIMA(1,1,1)x(1,0,0)[12]")
hist(residuals(modeloheuristico2),breaks=20,xlab="Residuos del modelo",
     ylab="Frecuencia",
     main="Histograma de residuos del modelo heurístico ARIMA(1,1,2)x(1,0,0)[12]")

#test Kolgomorov-Smirnov
ks.test( residuals(modelo1) , "pnorm", mean=mean(residuals(modelo1)), sd=sd(residuals(modelo1)))
ks.test( residuals(modelo2) , "pnorm", mean=mean(residuals(modelo2)) , sd=sd(residuals(modelo2)))
ks.test( residuals(modeloheuristico1) , "pnorm", mean=mean(residuals(modeloheuristico1)) , sd=sd(residuals(modeloheuristico1)))
ks.test( residuals(modeloheuristico2) , "pnorm", mean=mean(residuals(modeloheuristico2)) , sd=sd(residuals(modeloheuristico2)))

#contraste de media cero
t.test(residuals(modelo1),mu=0) #no se rechaza hipótesis nula de que la media es 0
t.test(residuals(modelo2),mu=0) #no se rechaza hipótesis nula de que la media es 0
t.test(residuals(modeloheuristico1),mu=0) #no se rechaza hipótesis nula de que la media es 0
t.test(residuals(modeloheuristico2),mu=0) #no se rechaza hipótesis nula de que la media es 0

########################## Contraste homocedasticidad ########################3
var.test(residuals(modelo1)[1:48], residuals(modelo1)[49:174], 
         null.value=1, alternative='two.sided',
         conf.level=0.95)
var.test(residuals(modelo2)[1:48], residuals(modelo2)[49:174], 
         null.value=1, alternative='two.sided',
         conf.level=0.95)
var.test(residuals(modeloheuristico1)[1:48], residuals(modeloheuristico1)[49:174], 
         null.value=1, alternative='two.sided',
         conf.level=0.95)
var.test(residuals(modeloheuristico2)[1:48], residuals(modeloheuristico2)[49:174], 
         null.value=1, alternative='two.sided',
         conf.level=0.95)

#gráfico ajuste modelo 1
plot(cobre,type="p",xlab="Tiempo en meses",main="Ajuste de ARIMA(3,1,0)x(2,0,0)[12] para la serie cobre"
     ,ylim=c(0,600),ylab="Precio del cobre diferenciado")
lines(fitted.values(modelo1),col="blue",lwd=2)
lines(fitted.values(modelo1)+1.96*sqrt(modelo1$sigma2),col="red",lwd=3)
lines(fitted.values(modelo1)-1.96*sqrt(modelo1$sigma2),col="red",lwd=3)
legend(x="topright",legend=c("Ajuste","I.C al 95%"),fill=c("blue","red"))

#gráfico ajuste modelo 2
plot(cobre,type="p",xlab="Tiempo en meses",main="Ajuste de ARIMA(2,1,1)x(2,0,0)[12] para la serie cobre"
     ,ylim=c(0,600),ylab="Precio del cobre diferenciado")
lines(fitted.values(modelo2),col="blue",lwd=2)
lines(fitted.values(modelo2)+1.96*sqrt(modelo2$sigma2),col="red",lwd=3)
lines(fitted.values(modelo2)-1.96*sqrt(modelo2$sigma2),col="red",lwd=3)
legend(x="topright",legend=c("Ajuste","I.C al 95%"),fill=c("blue","red"))

#gráfico ajuste modelo heuristico 1
plot(cobre,type="p",xlab="Tiempo en meses",main="Ajuste de ARIMA(1,1,1)x(1,0,0)[12] para la serie cobre"
     ,ylim=c(0,600),ylab="Precio del cobre diferenciado")
lines(fitted.values(modeloheuristico1),col="blue",lwd=2)
lines(fitted.values(modeloheuristico1)+1.96*sqrt(modeloheuristico1$sigma2),col="red",lwd=3)
lines(fitted.values(modeloheuristico1)-1.96*sqrt(modeloheuristico1$sigma2),col="red",lwd=3)
legend(x="topright",legend=c("Ajuste","I.C al 95%"),fill=c("blue","red"))

#gráfico ajuste modelo heuristico 2
plot(cobre,type="p",xlab="Tiempo en meses",main="Ajuste de ARIMA(1,1,2)x(1,0,0)[12] para la serie cobre"
     ,ylim=c(0,600),ylab="Precio del cobre diferenciado")
lines(fitted.values(modeloheuristico2),col="blue",lwd=2)
lines(fitted.values(modeloheuristico2)+1.96*sqrt(modeloheuristico2$sigma2),col="red",lwd=3)
lines(fitted.values(modeloheuristico2)-1.96*sqrt(modeloheuristico2$sigma2),col="red",lwd=3)
legend(x="topright",legend=c("Ajuste","I.C al 95%"),fill=c("blue","red"))

### predicción
par(mfrow=c(2,2))
plot(forecast(modelo1,h=6,level=c(95)),
     main="Predicción de ARIMA(3,1,0)x(2,0,0)[12] hasta diciembre 2022",xlab="Tiempo en meses",
     ylab="Precio del cobre refinado")
plot(forecast(modelo2,h=6,level=c(95)),
     main="Predicción de ARIMA(2,1,1)x(2,0,0)[12] hasta diciembre 2022",xlab="Tiempo en meses",
     ylab="Precio del cobre refinado")
plot(forecast(modeloheuristico1,h=6,level=c(95)),
     main="Predicción de ARIMA(1,1,1)x(1,0,0)[12] hasta diciembre 2022",xlab="Tiempo en meses",
     ylab="Precio del cobre refinado")
plot(forecast(modeloheuristico2,h=6,level=c(95)),
     main="Predicción de ARIMA(1,1,2)x(1,0,0)[12] hasta diciembre 2022",xlab="Tiempo en meses",
     ylab="Precio del cobre refinado")

#valores de predicción
forecast(modelo1,h=6,level=c(95))
forecast(modelo2,h=6,level=c(95))
forecast(modeloheuristico1,h=6,level=c(95))
forecast(modeloheuristico2,h=6,level=c(95))