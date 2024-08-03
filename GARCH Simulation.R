# GARCH (2,2) Simulation

set.seed(101)

omega = 0.5 ; alpha1 = 0.1; alpha2 = 0.2; beta1 = 0.3; beta2 = 0.4
N = 1000
volatility = c()
series_values = c()
volatility[1] = 1
volatility[2] = 1
series_values[1] = rnorm(1) ; series_values[2] = rnorm(1)
for(i in 3:N){
  volatility[i] = sqrt(omega + (alpha1 * (series_values[i-1]^2)) + 
                         (alpha2 * (series_values[i-2]^2)) + (beta1 * (volatility[i-1])^2) + (beta2 * (volatility[i-2])^2))
  series_values[i] = rnorm(1) * volatility[i]
}

plot(series_values, type = "l", col = "blue", lwd = 2, pch = 19, xlab = "Days", 
     ylab = "Simulated returns", main = "Simulation from GARCH(2,2)")

lines(volatility, col = "red", lwd = 2)
legend("topleft", col = c("blue", "red"), pch = c(16,16), legend = c("Series", "Volatility"))

#PACF of the data
pacf(series_values^2)

train_size = 0.90
train = series_values[1:(train_size*N)] ; test = series_values[(1+(train_size*N)):N]
train_vol = volatility[1:(train_size*N)] ; test_vol = volatility[(1+(train_size*N)):N]
library(rugarch)
garch_spec = ugarchspec(mean.model = list(armaOrder = c(0,0),  include.mean = FALSE),
                        variance.model = list(model = "sGARCH", garchOrder = c(2,2)))
garch_model = ugarchfit(data = train, spec = garch_spec)
garch_model
garch_forecast = ugarchforecast(garch_model, n.ahead = length(test))
volatility_forecast = garch_forecast@forecast$sigmaFor
plot(test_vol, col = "blue", type = "l", main = "Actual vs Forecasted Volatility from GARCH(2,2) | Rolling = False",
     xlab = "Time", ylab = "Volatility", lwd = 2)
lines(volatility_forecast, col = "red", lwd = 2)
legend("topleft", col = c("blue", "red"), pch = c(16,16), legend = c("Actual Volatility", "Forecasted Volatility"))


# Rolling predictions : Future window size 1
rolling_sigma_predictions = c()
rolling_sigma_predictions[1] = volatility_forecast[1]
test_size = length(test)
for(i in 2:test_size){
  train_size_rolling = (train_size * N)+(i-1)
  train_rolling = series_values[1:train_size_rolling]
  garch_model_rolling = ugarchfit(data = train_rolling, spec = garch_spec)
  garch_forecast_rolling = ugarchforecast(garch_model_rolling, n.ahead = 1)
  volatility_forecast_rolling = garch_forecast_rolling@forecast$sigmaFor
  rolling_sigma_predictions[i] = volatility_forecast_rolling
}

plot(test_vol, col = "blue", type = "l", main = "Actual vs Forecasted Volatility from GARCH(2,2) | Rolling = True",
     xlab = "Time", ylab = "Volatility", lwd = 2)
lines(rolling_sigma_predictions, col = "red", lwd = 2)
legend("topleft", col = c("blue", "red"), pch = c(16,16), legend = c("Actual Volatility", "Forecasted Volatility"))

df = data.frame(test_vol, rolling_sigma_predictions)



