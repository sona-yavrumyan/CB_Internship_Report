# load the U.S. macroeconomic data set
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(forecast)
library(gridExtra)
library(scales)
library(lmtest)
library(vars)
library(dynlm)
USMacroSWQ <- read_xlsx("us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))

# Change the name of the first column
colnames(USMacroSWQ)[1] <- "Date"


# format the date column
USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y:0%q")

# define GDP as ts object
GDP <- ts(USMacroSWQ$GDPC96,
          start = c(1957, 1), 
          end = c(2013, 4), 
          frequency = 4)

# define GDP growth as a ts object
GDPGrowth <- ts(400*log(GDP[-1]/GDP[-length(GDP)]),
                start = c(1957, 2), 
                end = c(2013, 4), 
                frequency = 4)

# 3-months Treasury bill interest rate as a 'ts' object
TB3MS <- ts(USMacroSWQ$TB3MS,
            start = c(1957, 1), 
            end = c(2013, 4), 
            frequency = 4)

# 10-years Treasury bonds interest rate as a 'ts' object
TB10YS <- ts(USMacroSWQ$GS10, 
             start = c(1957, 1), 
             end = c(2013, 4), 
             frequency = 4)

# generate the term spread series
TSpread <- TB10YS - TB3MS


# Estimate both equations using 'dynlm()'
VAR_EQ1 <- dynlm(GDPGrowth ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2), 
                 start = c(1981, 1), 
                 end = c(2012, 4))

VAR_EQ2 <- dynlm(TSpread ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2),
                 start = c(1981, 1),
                 end = c(2012, 4))

# rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","Growth_t-1", 
                                 "Growth_t-2", "TSpread_t-1", "TSpread_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)

# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> Intercept    0.516344   0.524429  0.9846 0.3267616    
#> Growth_t-1   0.289553   0.110827  2.6127 0.0101038 *  
#> Growth_t-2   0.216392   0.085879  2.5197 0.0130255 *  
#> TSpread_t-1 -0.902549   0.358290 -2.5190 0.0130498 *  
#> TSpread_t-2  1.329831   0.392660  3.3867 0.0009503 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(VAR_EQ2, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error t value  Pr(>|t|)    
#> Intercept    0.4557740  0.1214227  3.7536 0.0002674 ***
#> Growth_t-1   0.0099785  0.0218424  0.4568 0.6485920    
#> Growth_t-2  -0.0572451  0.0264099 -2.1676 0.0321186 *  
#> TSpread_t-1  1.0582279  0.0983750 10.7571 < 2.2e-16 ***
#> TSpread_t-2 -0.2191902  0.1086198 -2.0180 0.0457712 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(GDPGrowth, TSpread), start = c(1980, 3), end = c(2012, 4))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 2)
VAR_est
#> 
#> VAR Estimation Results:
#> ======================= 
#> 
#> Estimated coefficients for equation GDPGrowth: 
#> ============================================== 
#> Call:
#> GDPGrowth = GDPGrowth.l1 + TSpread.l1 + GDPGrowth.l2 + TSpread.l2 + const 
#> 
#> GDPGrowth.l1   TSpread.l1 GDPGrowth.l2   TSpread.l2        const 
#>    0.2895533   -0.9025493    0.2163919    1.3298305    0.5163440 
#> 
#> 
#> Estimated coefficients for equation TSpread: 
#> ============================================ 
#> Call:
#> TSpread = GDPGrowth.l1 + TSpread.l1 + GDPGrowth.l2 + TSpread.l2 + const 
#> 
#> GDPGrowth.l1   TSpread.l1 GDPGrowth.l2   TSpread.l2        const 
#>  0.009978489  1.058227945 -0.057245123 -0.219190243  0.455773969