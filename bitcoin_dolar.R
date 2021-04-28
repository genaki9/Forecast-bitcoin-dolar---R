
library(jsonlite)
bitcoin2019 <- fromJSON("https://mindicador.cl/api/bitcoin/2019")$serie

periodo <- 2009:2020
bitcoin <- lapply(periodo,FUN=
                    function(x){fromJSON(paste0("https://mindicador.cl/api/bitcoin/",x))$serie})

library(dplyr)
bind_rows(bitcoin, .id = "column_label") ->bitcoindf

bitcoindf %>%
  View

library(lubridate)

bitcoindf %>%
  mutate(fecha=ymd(substr(fecha,1,10)))%>%
  select(-column_label)->bitcoindf


bitcoindf %>%
  mutate(semana=isoweek(fecha),
         periodosemana=paste0(year(fecha),"s",semana),
         year=year(fecha))%>%
  group_by(year,semana,periodosemana)%>%
  summarise(valor=mean(valor))->bitcoindfsem

bitcoindfsem %>%
  filter(semana!=53)->bitcoindfsem

bitcoindfsem %>%
  group_by(year)%>%
  summarise(nsemanas=n())


serie.bitcoin <- ts(bitcoindfsem$valor, start =c(2009,1), freq=52)
#serie.dolar <- ts(select(series.INDICADOR), start =c(2004,1), freq=12)

install.packages("forecast")
library(forecast)
install.packages("highcharter")
library(highcharter)
plot(forecast(serie.bitcoin))

dec <- decompose(serie.bitcoin, "multiplicative")

#descomposicion de una serie
plot(dec)
plot(dec$seasonal)
plot(dec$trend)

hchart(acf(serie.bitcoin,100)) #acf = auto correlated function
hchart(pacf(serie.bitcoin))  #pacf = auto correlacion parcial


#### repetir para el dolar


periodo <- 2009:2020
dolar <- lapply(periodo,FUN=
                    function(x){fromJSON(paste0("https://mindicador.cl/api/dolar/",x))$serie})


bind_rows(dolar, .id = "column_label") ->dolardf

dolardf %>%
  mutate(fecha=ymd(substr(fecha,1,10)))%>%
  select(-column_label)->dolardf

dolardf %>%
  mutate(semana=isoweek(fecha),
         periodosemana=paste0(year(fecha),"s",semana),
         year=year(fecha))%>%
  group_by(year,semana,periodosemana)%>%
  summarise(valor=mean(valor))->dolardfsem

dolardfsem %>%
  filter(semana!=53)->dolardfsem

dolardfsem %>%
  group_by(year)%>%
  summarise(nsemanas=n())


serie.dolar <- ts(dolardfsem$valor, start =c(2009,1), freq=52)
#serie.dolar <- ts(select(series.INDICADOR), start =c(2004,1), freq=12)

plot(forecast(serie.dolar))

dec <- decompose(serie.dolar, "multiplicative")
plot(dec)
plot(dec$seasonal)
plot(dec$trend)

hchart(acf(serie.dolar,100)) 
hchart(pacf(serie.dolar))

cbind(serie.dolar, serie.bitcoin)
length(serie.dolar)
length(serie.bitcoin)

data.frame(dolar=serie.dolar[1:(length(serie.dolar))],bitcoin=serie.bitcoin)->dolarbit
#data.frame(dolar=serie.dolar[1:(length(serie.dolar)-1)],bitcoin=serie.bitcoin)->dolarbit
length(dolarbit$dolar)->N

xreg <- serie.dolar[1:(N-4)]
trainbitcoin <- serie.bitcoin[5:N]
xreg.test <- serie.dolar[(N-3):N]

cor(dolarbit$dolar, dolarbit$bitcoin, use="everything")

forecast::auto.arima(
  trainbitcoin,
  #ic ="aic",
  xreg = xreg,
  #allwdrift = T,
  #allowmean = TRUE,
  #d =1,
  #D = 1,
  #max.p = 7,
  #max.q= 7,
  #max.P =12,
  #max.Q =12,
  #max.order=12,
  #max.d =1
)->forDolarbit

pred.reg <- forecast(forDolarbit,xreg=xreg.test)
 
