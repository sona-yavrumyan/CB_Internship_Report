library(tidyverse)
library(readxl)
library(forecast)

                            #### TIME SERIES PLOTS ####
curr_data <- read_excel("CB_CURRENCY.xls", sheet="Sheet2", range="A2:G1157") %>% slice(-1)

curr_data$date <- as.Date(with(curr_data, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

curr_data_clean <- curr_data %>% select(4:8) %>% select(date, everything())

ggplot(curr_data_clean, aes(x = date)) +
  geom_line(aes(y = USD, color = "USD")) +
  geom_line(aes(y = EUR, color = "EUR")) +
  geom_line(aes(y = RUB*50, color = "RUB")) +
  geom_line(aes(y = GBP, color = "GBP")) +
  labs(x = "Date", y = "Exchange Rate", color = "Currency") +
  theme_minimal()

weekly_data <- curr_data_clean %>%
  mutate(week = week(date), year = year(date)) %>%
  group_by(year, week) %>%
  slice(n()) %>%
  ungroup() %>%
  select(-week, -year)

ggplot(weekly_data, aes(x = date)) +
  geom_line(aes(y = USD, color = "USD")) +
  geom_line(aes(y = EUR, color = "EUR")) +
  geom_line(aes(y = RUB*50, color = "RUB")) +
  geom_line(aes(y = GBP, color = "GBP")) +
  labs(x = "Date", y = "Exchange Rate", color = "Currency") +
  theme_minimal()
 

                          #### ARIMA AND RESIDUALS ####
ts_usd <- ts(curr_data_clean$USD, start = c(as.numeric(format(min(curr_data_clean$date), "%Y")), as.numeric(format(min(curr_data_clean$date), "%j"))), frequency = 252)
fit2 <- auto.arima(ts_usd, approximation = T)
checkresiduals(fit2, approximation=FALSE)
autoplot(forecast(fit2))

index_of_spike <- which.max(abs(fit2$residuals))

date_of_spike <- curr_data_clean$date[index_of_spike]

Box.test(residuals(fit),
         fitdf=5, lag=10, type="Ljung")


                                  #### DYNMAIC ####

event_date <- as.Date("2022-02-24")
curr_data_clean$event_indicator <- ifelse(curr_data_clean$date >= event_date, 1, 0)
fit_dynamic <- auto.arima(curr_data_clean$USD, xreg = curr_data_clean$event_indicator, approximation = FALSE)
future_length <-365
checkresiduals(fit_dynamic)
forecast_dynamic <- forecast(fit_dynamic, xreg = rep(1, future_length))  # Adjust future_length as needed
autoplot(forecast_dynamic)

forecast_df <- data.frame(
  Date = time(forecast_dynamic$mean),
  Forecast = forecast_dynamic$mean,
  Lower80 = forecast_dynamic$lower[,1],
  Upper80 = forecast_dynamic$upper[,1],
  Lower95 = forecast_dynamic$lower[,2],
  Upper95 = forecast_dynamic$upper[,2]
)

ts_df <- data.frame(
  Date = time(ts_usdevent),
  Value = as.numeric(ts_usdevent)
)

ggplot() +
  geom_line(data = ts_df, aes(x = Date, y = Value, color = "USD"), linetype = "solid") +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "blue", linetype = "dashed") +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95), alpha = 0.2, fill = "blue") +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80), alpha = 0.5, fill = "blue") +
  labs(title = "Dynamic Forecast",
       x = "Date",
       y = "USD") +
  theme_minimal()



#lambda <- BoxCox.lambda(curr_data_clean$USD)
#usd_transformed <- BoxCox(curr_data_clean$USD, lambda)
#plot(curr_data_clean$date, usd_transformed, type = 'l', main = 'Transformed USD Exchange Rate', xlab = 'Date', ylab = 'Transformed USD')

#ts_usd_transformed <- ts(usd_transformed, start = c(year(min(curr_data_clean$date)), month(min(curr_data_clean$date))), frequency = 365)

# Fit the ARIMA model on the transformed data
#fit_transformed <- auto.arima(ts_usd_transformed)

# Forecast
forecast_transformed <- forecast(fit_transformed)

# Plot the forecast
#autoplot(forecast_transformed) + 
 # labs(title = "Forecast of Transformed USD Exchange Rate", x = "Date", y = "Transformed USD") +
  #theme_minimal()

#forecast_original_scale <- InvBoxCox(forecast_transformed$mean, lambda)

# Plot the original data with the forecast on the original scale
#plot(curr_data_clean$date, curr_data_clean$USD, type = 'l', main = 'USD Exchange Rate with Forecast', xlab = 'Date', ylab = 'USD')
#lines(seq.Date(from = max(curr_data_clean$date) + 1, by = "day", length.out = length(forecast_original_scale)), forecast_original_scale, col = 'red')


