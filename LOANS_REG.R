library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(forecast)
library(gridExtra)
library(scales)
library(lmtest)
library(vars)

loans_data <- read_excel("7_loans by branches_eng.xlsx", sheet="branches", range="A34:KI45") %>% dplyr::select(1, 122:294)
loans_data_long <- as.data.frame(t(loans_data))
view(loans_data_long)

new_col_names <- as.character(loans_data_long[1, ])

loans_data_long <- loans_data_long %>% slice(-1)  # Remove the first row

colnames(loans_data_long) <- new_col_names
view(loans_data_long)
loans_data_long
loans_data_long$Date <- as.Date(as.numeric(loans_data_long$Date), origin = "1900-01-01")
view(loans_data_long)

ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = as.numeric(Industry), color = "Industry")) +
  geom_line(aes(y = as.numeric(Agriculture), color = "Agriculture")) +
  geom_line(aes(y = as.numeric(Construction), color = "Construction")) +
  geom_line(aes(y = as.numeric(Communications), color = "Communications")) +
  geom_line(aes(y = as.numeric(Trade), color = "Trade")) +
  geom_line(aes(y = as.numeric(Service), color = "Service")) +
  geom_line(aes(y = as.numeric(Consumer), color = "Consumer")) +
  geom_line(aes(y = as.numeric(Mortgage), color = "Mortgage")) +
  geom_line(aes(y = as.numeric(Other), color = "Other")) +
  labs(x = "Date", y = "Loan Amount", color = "Sector") +
  theme_minimal()


loans_data_long <- loans_data_long %>%
  mutate(Diff_Industry = as.numeric(Industry) - lag(as.numeric(Industry)),
         Diff_Agriculture = as.numeric(Agriculture) - lag(as.numeric(Agriculture)),
         Diff_Construction = as.numeric(Construction)-lag(as.numeric(Construction)),
         Diff_Communications =as.numeric(Communications)-lag(as.numeric(Communications)),
         Diff_Trade = as.numeric(Trade)-lag(as.numeric(Trade)),
         Diff_Service= as.numeric(Service)-lag(as.numeric(Service)),
         Diff_Consumer = as.numeric(Consumer)-lag(as.numeric(Consumer)),
         Diff_Mortgage = as.numeric(Mortgage)-lag(as.numeric(Mortgage)),
         Diff_Other = as.numeric(Other)-lag(as.numeric(Other)))



x_range <- range(loans_data_long$Date, na.rm = TRUE)
x_breaks <- seq(from = x_range[1], to = x_range[2], by = "12 month") 


plot_ind <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Industry, color = "Industry")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  scale_color_manual(values = c("Industry" = "brown")) +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" ) # Position the legend at the bottom)

plot_agr <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Agriculture, color = "Agriculture")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  scale_color_manual(values = c("Agriculture" = "magenta")) +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" ) # Position the legend at the bottom)

plot_constr <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Construction, color = "Construction")) +
  scale_color_manual(values = c("Construction" = "darkgreen")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" )

plot_comm <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Communications, color = "Communications")) +
  scale_color_manual(values = c("Communications" = "skyblue")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" )


plot_trade <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Trade, color = "Trade")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  scale_color_manual(values = c("Trade" = "red")) +  # Set color here
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" )

plot_service <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Service, color = "Service")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  scale_color_manual(values = c("Service" = "blue")) + 
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" )

plot_consumer <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Consumer, color = "Consumer")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  scale_color_manual(values = c("Consumer" = "orange")) +
  theme(legend.position = "bottom" )

plot_mortgage <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Mortgage, color = "Mortgage")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  scale_color_manual(values = c("Mortgage" = "lightgreen")) +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" )

plot_other <- ggplot(loans_data_long, aes(x = Date)) +
  geom_line(aes(y = Diff_Other, color = "Other")) +
  labs(x = "Date", y = "Lagged Loan Amount", color = "Sector") +
  scale_color_manual(values = c("Other" = "navyblue")) +
  theme_minimal() + scale_x_date(limits = x_range, breaks = x_breaks) +
  theme(legend.position = "bottom" )

grid.arrange(plot_ind, plot_agr, plot_constr, plot_comm, plot_trade, plot_service,
             plot_consumer, plot_mortgage, plot_other, nrow=3, ncol=3)



# CPI
# Exchange rate
# Economic Activity Indicator
# Policy rate