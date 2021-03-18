#EJEMPLO SERIES DE TIEMPO - MINERIA DE DATOS

#PASO 1: INSTALAR LIBRERIAS
library(TSA)
library(tseries)

#PASO 2: ELEGIR SERIE A TRABAJAR A TRABAJAR
data(lynx)
class(lynx)
#Esta serie de tiempo nos muestra los datos anuales de linces capturados en canadá en Canadá


#Grafica de la Serie
plot(lynx,xlab="Año",ylab="Número de Linces",main="Número de Linces Capturados por año en Canada")

#PASO 3: COMPROBAR QUE LA SERIE DE TIEMPO ES ESTACIONARIA

#Estacionariedad en media
summary(lm(lynx~time(lynx),data=lynx))
#p-valor>alpha
#No rechazamos H0
#La serie no muestra tendencia -> La serie es estacionaria en Media

#Estacionariedad en Varianza
adf.test(lynx,alternative = "stationary")
#P-valor<alpha
#Rechazamos h0
#La serie es estacionaria en varianza

#De esta forma comprobamos que nuestra serie es estacionaria

#PASO 4: MODELADO

#Modelo AR
AIC.AR<-Inf
for (i in 1:5){
  
  aicpar<-AIC(arima(lynx,order=c(i,0,0),method = "ML"))
  if (aicpar<AIC.AR){
    AIC.AR<-aicpar
    mejor.AR<-i
  }
}

AIC.AR
mejor.AR

#Modelo MA
AIC.MA<-Inf
for (i in 1:5){
  
  aicpar<-AIC(arima(lynx,order=c(0,0,i),method = "ML"))
  if (aicpar<AIC.MA){
    AIC.MA<-aicpar
    mejor.MA<-i
  }
}

AIC.MA
mejor.MA

#Modelo ARMA

AIC.ARMA<-Inf
for (j in 1:5){
  for (i in 1:5){
    aicpar<-AIC(arima(lynx,order = c(i,0,j),method="ML"))
    if (aicpar<AIC.ARMA){
      AIC.ARMA<-aicpar
      mejor.ARMA<-c(i,j)
    }
  }
}

AIC.ARMA
mejor.ARMA

#PASO 5: ELEGIR EL MEJOR MODELO

#Para elegir el mejor modelo, elegimos el que tenga el AIC mas pequeño
#Comparación AIC
AIC.AR
AIC.MA
AIC.ARMA

#Elegimos el modelo ARMA


#PASO 6: PRONÓSTICOS
#Queremos obtener los pronosticos de el numero de linces que serán atrapados en los proximos 100 años

predARMA<-predict(arima(lynx,order = c(mejorARMA[1],0,mejorARMA[2]),method="ML"),n.ahead=100)$pred
predARMA
plot(lynx,xlab="Año",ylab="Número de Linces",main="Número de Linces Capturados por año en Canadá",xlim=c(1820,2035))
lines(predARMA,col="blue")