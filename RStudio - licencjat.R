###############################################
#Ten plik zawiera comiesieczny wskaznik
#cen towarów i uslug konsumpcyjnych.
#Użyjemy tych danych w celu przewidzenia zmian
#liczbowych wskaznika w kolejnych dwoch latach.
#
#Stworzone przez: Damian Kmiecik 8/28/2020
###############################################

#Wyczyszczenie wszystkich zmiennych
rm(list=ls())

#Zainicjowanie pakietu do forecasting'u
library(fpp2)

#Pobranie danych
data <- read.csv("/Users/Koneczek/Downloads/Retail_Trade_Poland_2000-2020.csv")

#Zadeklarowanie tego jako dane szeregów czasowych
Y <- ts(data[,2],start=c(2000,6),frequency = 12)

###############################################
#Analiza wstepna
###############################################

#Wykres czasowy
autoplot(Y) + 
  ggtitle("Wykres czasowy") +
  ylab("Indeks 2015=100")

#Dane wykazuja trend wzrostowy.
#Badanie transformacji.


#Sprawdzenie różnicy w danych
#w celu pozbycia sie trendu.
DY <- diff(Y)

autoplot(DY) + 
  ggtitle("Wykres zmiany w czasie") +
  ylab("Różnica")

#Trend w serii wydaje sie byc staly.
#Sprawdzenie czy wydoczne sa 
#oznaki sezonowosci w danych.

ggseasonplot(DY) + 
  ggtitle("Wykres sezonowy")

#Alternatywny wykres sezonowosci
ggsubseriesplot(DY)+ 
  ggtitle("Wykres sezonowy")


###################################################
#Seria Y wykazuje trend i sezonowosc.
#Aby zredukowac trend, bierzemy pierwsza roznice.
#Pierwsza seria roznicy ciagle posiada sezonowosc.
#
#Przewidywanie z uzyciem roznych metod.
###################################################

#Uzycie metody benchmarkowej celem prognozy.
#Uzycie sezonowej metody naiwnej jako benchmarku.
# y_t = y(t-s) + e_t

fit <- snaive(DY) #Residual SD = 3.6173 (roznica pomiedzy miesiacami w poszczegolnych latach wynosi srednio 3.6173)
print(summary(fit))
checkresiduals(fit)

#Uzycie modelu wygladzania wykladniczego (ETS)

fit_ets <- ets(Y) #Residual SD = 0.0334
print(summary(fit_ets))
checkresiduals(fit_ets)

#Uzycie zintegrowanego modelu autoregresyjnego ze średnią ruchomą (ARIMA)

fit_arima <- auto.arima(Y,d=1,D=1,stepwise = FALSE, approximation = FALSE, trace = TRUE) #Residual SD = 2.963781
print(summary(fit_arima))
checkresiduals(fit_arima)

#############################################
#Prognoza z wykorzystaniem modelu ARIMA.
#############################################
fcst <- forecast(fit_arima,h=24)
autoplot(fcst,include=60)+ 
  ggtitle("Wartosci przewidywane") +
  ylab("Indeks 2015=100")
print(summary(fcst))
