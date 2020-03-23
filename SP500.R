library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(data.table)

data_master = copy(dadm_data)
data_master

attach(dadm_data)
data_master


sp_500 = ts(data_master$Open, start=c(1995, 1),end = c(2017,3), freq=12)

sp_500
plot(sp_500)

Box.test(sp_500, lag = 20, type = 'Ljung-Box')

adf.test(sp_500)
 sp_500_training = ts(data_master$Open, start=c(1995, 1),end = c(2014,12), freq=12)

 Box.test(sp_500_training, lag = 20, type = 'Ljung-Box')
adf.test(sp_500_training)        

plot_decomp(sp_500_training)

decomp = decompose(sp_500_training)
plot(decomp)

seasonalAdjusted = sp_500_training - decomp$seasonal
plot(seasonalAdjusted)

acf = acf(sp_500_training)
plot(acf)
pacf(sp_500_training)

tsdiff = diff(sp_500_training)
plot(tsdiff)

acf(tsdiff)
pacf(tsdiff)

auto.arima()
fit <- Arima(sp_500_training, order = c(0,1,1),include.drift = TRUE)
summary(fit)


# RESIDUAL DIAGNOSTICS
ggtsdiag(fit) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

residFit <- ggplot(data=fit, aes(residuals(fit))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals")


sp_500_all  = forecast(fit,h =27)
sp_500_test = window(sp_500,2015,c(2017,03))

plot(sp_500_all)

round(accuracy(f = sp_500_all, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

aa = snaive(sp_500_training,h=27)
aa1 = forecast(aa,h=12)
round(accuracy(f = aa1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

bb = meanf(sp_500_training,h=27)
bb1 = forecast(bb,h=27)
round(accuracy(f = bb1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

cc = naive(sp_500_training,h=27)
cc1 = forecast(cc,h=27)
round(accuracy(f = cc1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)



