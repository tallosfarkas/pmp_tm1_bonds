setwd("C:/Users/Student1.AzureAD/Documents/R")

getwd()

load("BBG_BondTicker.RData")

install.packages("corrplot", dependencies = TRUE)

library(openxlsx)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(rlang)
library(gridExtra)
library(corrplot)
library(lubridate)




# connect to BBG (Terminal must be open)
library(Rblpapi)
blpConnect()

# Only look at the G10 Countries:
unique(DT$country)

G10 <- c("AUSTRALIA","CANADA","GERMANY","JAPAN","NEW ZEALAND","NORWAY","SWEDEN","SWITZERLAND","BRITAIN","UNITED STATES")

Tickers <- DT$ticker[DT$country %in% G10]

Tickers

# Only look at Gov. Bond 1-Month, and 10-Y

Tickers <- Tickers[grepl("3M|10Y",Tickers)]

Tickers

opt <- c("periodicitySelection"="DAILY")
Gov_Bonds <- bdh(Tickers, c("PX_LAST"),
                 as.Date("1995-01-01"), as.Date("2023-01-01"), options=opt)

##########################################################################################################################

# Create synthetic prices

Gov_Bonds_Prices <- 100 / (1+ Gov_Bonds/100)^(1/4)

exchange_tickers <- c("AUDUSD Curncy", # Australia
                      "CADUSD Curncy", # Canada
                      "EURUSD Curncy", # Germany (Eurozone)
                      "JPYUSD Curncy", # Japan
                      "NZDUSD Curncy", # New Zealand
                      "NOKUSD Curncy", # Norway
                      "SEKUSD Curncy", # Sweden
                      "CHFUSD Curncy", # Switzerland
                      "GBPUSD Curncy" # Britain
) 

Exchange_rate <- bdh(exchange_tickers, c("PX_LAST"),
                     as.Date("1995-01-01"), as.Date("2023-01-01"), options=opt)

# Create a new workbook
wb <- createWorkbook()

# Add data for Exchange Rates
addWorksheet(wb, "Exchange_Rates")
writeData(wb, "Exchange_Rates", Exchange_rate)

# Add data for Government Bonds
addWorksheet(wb, "Government_Bonds")
writeData(wb, "Government_Bonds", Gov_Bonds)

# Save the Excel file
saveWorkbook(wb, "Exchange_and_Bonds_Data.xlsx", overwrite = TRUE)

save(Gov_Bonds, Exchange_rate, Tickers, file="Bond_Project_v2.RData")

str(Gov_Bonds)
str(Exchange_rate)

# For Exchange Rates (e.g., first ticker)
diff_dates_exchange <- diff(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
table(diff_dates_exchange)

# For Government Bonds (e.g., first ticker)
diff_dates_bonds <- diff(as.Date(Gov_Bonds[["I02203M Index"]][["date"]]))
table(diff_dates_bonds)


# Expected daily sequence
start_date <- min(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
end_date <- max(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
expected_dates <- seq.Date(start_date, end_date, by = "day")

# Check for missing dates
actual_dates <- as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]])
missing_dates <- setdiff(expected_dates, actual_dates)

missing_dates


# Range of dates
range(as.Date(Exchange_rate[["GBPUSD Curncy"]][["date"]]))
range(as.Date(Gov_Bonds[["I02203M Index"]][["date"]]))

# Plot all Government Bonds
par(mfrow = c(2, 2)) # Arrange plots in a grid
for (ticker in names(Gov_Bonds)[1:4]) { # Plot first 4 as an example
  plot(as.Date(Gov_Bonds[[ticker]]$date),
       Gov_Bonds[[ticker]]$PX_LAST,
       type = "l",
       main = paste("Price Over Time:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Plot all Exchange Rates
par(mfrow = c(2, 2)) # Arrange plots in a grid
for (ticker in names(Exchange_rate)[1:4]) { # Plot first 4 as an example
  plot(as.Date(Exchange_rate[[ticker]]$date),
       Exchange_rate[[ticker]]$PX_LAST,
       type = "l",
       main = paste("Exchange Rate:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Government Bonds: Check date ranges
for (ticker in names(Gov_Bonds)) {
  cat("\nTicker:", ticker)
  cat("\nDate Range:", range(as.Date(Gov_Bonds[[ticker]]$date)), "\n")
}

# Exchange Rates: Check date ranges
for (ticker in names(Exchange_rate)) {
  cat("\nTicker:", ticker)
  cat("\nDate Range:", range(as.Date(Exchange_rate[[ticker]]$date)), "\n")
}

# Loop through all tickers in Exchange_rate
for (ticker in names(Exchange_rate)) {
  # Extract dates
  dates <- as.Date(Exchange_rate[[ticker]]$date)
  
  # Create expected daily date sequence
  expected_dates <- seq.Date(min(dates), max(dates), by = "day")
  
  # Check for missing dates
  missing_dates <- setdiff(expected_dates, dates)
  
  # Print results for the ticker
  cat("\nTicker:", ticker)
  cat("\nMissing Dates:", length(missing_dates), "\n")
  print(missing_dates)
}

# Plot all exchange rate data
par(mfrow = c(3, 3)) # Arrange plots in a grid (3x3 for 9 currencies)

for (ticker in names(Exchange_rate)) {
  plot(as.Date(Exchange_rate[[ticker]]$date),
       Exchange_rate[[ticker]]$PX_LAST,
       type = "l",
       col = "blue",
       main = paste("Exchange Rate:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Reset plotting layout
par(mfrow = c(1, 1))
# Plot all government bond data
par(mfrow = c(4, 5)) # Arrange plots in a grid (4x5 for 20 bonds)

for (ticker in names(Gov_Bonds)) {
  plot(as.Date(Gov_Bonds[[ticker]]$date),
       Gov_Bonds[[ticker]]$PX_LAST,
       type = "l",
       col = "red",
       main = paste("Gov Bond:", ticker),
       xlab = "Date",
       ylab = "Price")
}

# Reset plotting layout
par(mfrow = c(1, 1))



#Data Preparation
################################################################################

#Setting the names from the Bloomberg Output
Countries <- c("BRITIAN","UNITED STATES","AUSTRALIA","CANADA","GERMANY","JAPAN","NEW ZEALAND","NORWAY","SWEDEN","SWITZERLAND")
Bond_names_3M <- rep("3M",10)
Bond_names_10Y <-rep("10Y",10)
Bond_names_3M <- paste(Countries,Bond_names_3M)
Bond_names_10Y <- paste(Countries,Bond_names_10Y)
FX_names <- c("GBPUSD Curncy",
              "USDUSD Curncy",
              "AUDUSD Curncy",
              "CADUSD Curncy",
              "EURUSD Curncy",
              "JPYUSD Curncy",
              "NZDUSD Curncy", 
              "NOKUSD Curncy",
              "SEKUSD Curncy", 
              "CHFUSD Curncy")

#Create the Ticker sequence for the Loop
Ticker_seq <- seq(from = 1, to = 19, by = 2)
FX_Tickers <- c(rep(FX_names,each = 2))
length(FX_Tickers)
B_3M_Tickers <- c(rep(Bond_names_3M,each = 2))
typeof(B_3M_Tickers)
B_10Y_Tickers <- c(rep(Bond_names_10Y,each = 2))
typeof(B_10Y_Tickers)


#Setting the time frame
#United States: largest data range
time_frame_raw <- Gov_Bonds[[Tickers[19]]][1]
time_frame <- as.Date(time_frame_raw[[1]])
time_frame
length(time_frame)
length(unique(time_frame))

#Add USD currency list to exchange rate
USDUSD_Curncy <- data.frame(date = time_frame,PX_LAST = rep(1,length(time_frame)))
exchange_rate_complete <- append(Exchange_rate,list(USDUSD_Curncy))
names(exchange_rate_complete)[length(exchange_rate_complete)] <- "USDUSD Curncy"

#Create data frames for end result:
Bonds_yields_3M <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_yields_3M) <- c(Bond_names_3M)
Bonds_yields_10Y <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Bonds_yields_10Y) <-c(Bond_names_10Y)
Exchange_rates <- data.frame(matrix(NA, nrow = length(time_frame), ncol = 10))
colnames(Exchange_rates) <- c(FX_names)
rownames(Bonds_yields_3M) <- time_frame
rownames(Bonds_yields_10Y) <- time_frame
rownames(Exchange_rates) <- time_frame


#Setting the same time frame for every data and interpolate missing values
for(i in Ticker_seq){
  #Data for each Country
  df1 <- data.frame(date = as.Date(Gov_Bonds[[Tickers[i]]][[1]]),data = Gov_Bonds[[Tickers[i]]][[2]])
  df2 <- data.frame(date = as.Date(Gov_Bonds[[Tickers[i+1]]][[1]]),data = Gov_Bonds[[Tickers[i+1]]][[2]])
  df3 <- data.frame(date = as.Date(exchange_rate_complete[[FX_Tickers[i]]][[1]]),data = exchange_rate_complete[[FX_Tickers[i]]][[2]])
  
  zoo_df1 <- zoo(df1$data, order.by = df1$date)
  zoo_df2 <- zoo(df2$data, order.by = df2$date)
  zoo_df3 <- zoo(df3$data, order.by = df3$date)
  
  # Reindex each zoo object to match the target date vector and set missing values to NA
  zoo_reindexed_1 <- merge(zoo_df1, zoo(, time_frame), all = TRUE)
  zoo_reindexed_2 <- merge(zoo_df2, zoo(, time_frame), all = TRUE)
  zoo_reindexed_3 <- merge(zoo_df3, zoo(, time_frame), all = TRUE)
  
  zoo_interpolated_1 <- na.approx(zoo_reindexed_1, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_2 <- na.approx(zoo_reindexed_2, xout = time_frame, na.rm = FALSE)
  zoo_interpolated_3 <- na.approx(zoo_reindexed_3, xout = time_frame, na.rm = FALSE)
  
  # Perform linear interpolation for missing values for each sequence
  Bonds_yields_3M[[B_3M_Tickers[i]]] <- coredata(zoo_interpolated_1)
  Bonds_yields_10Y[[B_10Y_Tickers[i]]] <- coredata(zoo_interpolated_2)
  Exchange_rates[[FX_Tickers[i]]] <- coredata(zoo_interpolated_3)
}

#Create synthetic prices from Yields

Bonds_prices_3M <- 100 / (1+ Bonds_yields_3M/100)^(1/4)  

Bonds_prices_10Y <- 100 / (1+ Bonds_yields_10Y/100)^(10)  

Bonds_prices_10Y3M <- 100/ (1+ (Bonds_yields_10Y)/100)^(9+11/12)

#Add a date column
Bonds_prices_10Y3M <- Bonds_prices_10Y3M   %>% 
  mutate( 
    Bonds_prices_10Y["date"]
  )


#Plotting and Observing the Data
################################################################################

#Add new column for the date
Bonds_prices_3M$date <- as.Date(rownames(Bonds_prices_3M))
Bonds_prices_10Y$date <- as.Date(rownames(Bonds_prices_10Y))
Exchange_rates$date <-  as.Date(rownames(Exchange_rates))

##Correcting Outliers:
#Japan One Year Bond Price --> Possible Missing Data 
outlier_Jap_date <- Bonds_prices_10Y$date[Bonds_prices_10Y$`JAPAN 10Y`== 100]
#Use the mean of the last 10 days to replace the outlier/missing data
index <- which(Bonds_prices_10Y$date == outlier_Jap_date)
Replacement <- mean(Bonds_prices_10Y$`JAPAN 10Y`[(index-10):(index-1)])
Bonds_prices_10Y$`JAPAN 10Y`[index] <- Replacement
  
##Plots with corrected values:
plot_list_3M <- list()
plot_list_10Y <- list()
plot_list_FX <- list()

for (i in 1:10) {
  column_name_3M <- Bond_names_3M[i]
  column_name_10Y <- Bond_names_10Y[i]
  column_name_FX <- FX_names[i]
  plot_list_3M[[i]] <- ggplot(Bonds_prices_3M, aes(x = date, y = !!sym(column_name_3M))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_3M))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7)     
    )
  plot_list_10Y[[i]] <- ggplot(Bonds_prices_10Y, aes(x = date, y = !!sym(column_name_10Y))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_10Y))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7)     
    )
  plot_list_FX[[i]] <- ggplot(Exchange_rates, aes(x = date, y = !!sym(column_name_FX))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Price", title = paste( column_name_FX))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7)     
    )
}
grid.arrange(grobs = plot_list_3M, ncol = 2)
grid.arrange(grobs = plot_list_10Y, ncol = 2)
grid.arrange(grobs = plot_list_FX, ncol = 5)


##Correlation Matrix:

correlation_matrix_3M <- cor(Bonds_prices_3M[,1:10])
correlation_matrix_10Y <- cor(Bonds_prices_10Y[,1:10])

par(mar = c(1, 5, 2, 2))
corrplot(correlation_matrix_3M, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         tl.cex = 0.7, # Adjust font size of text labels
         cl.cex = 0.6, # Adjust font size of color legend
         number.cex = 0.6) # Adjust font size of correlation coefficients
title("Correlation Matrix of 3M Bond Prices", cex.main = 1)

corrplot(correlation_matrix_10Y, method = "color", type = "upper", 
        col = colorRampPalette(c("blue", "white", "red"))(200), 
        tl.col = "black", tl.srt = 45, addCoef.col = "black", 
        tl.cex = 0.7, # Adjust font size of text labels
        cl.cex = 0.6, # Adjust font size of color legend
        number.cex = 0.6) # Adjust font size of correlation coefficients

title("Correlation Matrix of 10Y Bond Prices", cex.main = 1)

#Saving the data:

save(Bonds_prices_3M, Bonds_prices_10Y,Exchange_rates,B_10Y_Tickers,B_3M_Tickers,FX_names,Tickers, file = "Bond_PX_FX.RData")
save(plot_list_3M,plot_list_10Y,plot_list_FX, file ="Bond_PX_FX_Plots.RData")

# Creating the Bond Excess Returns
################################################################################

# "We calculate time series of excess returns as defined in Equation (6) at monthly frequency,
# using data from the last day of each month"

## SORT THE FX NAMES AND COLUMNS TO MATCH THE BONDS

# 1. Filter the last day of each month from the data

eomonth_data <- function(data){
  data %>%
    mutate(date = as.Date(date)) %>%            
    group_by(month = floor_date(date, "month")) %>% 
    filter(date == max(date)) %>%               
    ungroup() %>%
    select(-month)   
}


Bonds_3M_eom <- eomonth_data(Bonds_prices_3M)
Bonds_10Y_eom <- eomonth_data(Bonds_prices_10Y)
Bonds_10Y3M_eom <- eomonth_data(Bonds_prices_10Y3M)
FX_eom <- eomonth_data(Exchange_rates)

colnames(Bonds_3M_eom) <- c(Countries,"date")
colnames(Bonds_10Y_eom) <- c(Countries,"date")
colnames(Bonds_10Y3M_eom) <- c(Countries,"date")
colnames(FX_eom) <- c(Countries,"date")


#2. Reshape data into long format and merge by data and country

Bonds_3M_eom_long <- Bonds_3M_eom %>% pivot_longer(-date, names_to = "Country", values_to = "P_3M")
Bonds_10Y_eom_long <- Bonds_10Y_eom %>% pivot_longer(-date, names_to = "Country", values_to = "P_10Y")
Bonds_10Y3M_eom_long <- Bonds_10Y3M_eom %>% pivot_longer(-date, names_to = "Country", values_to = "P_10Y3M")
FX_eom_long <- FX_eom %>% pivot_longer(-date, names_to = "Country", values_to = "FX")


#3. Merge

Merged_Bonds_FX <- Bonds_3M_eom_long %>%
  inner_join(Bonds_10Y_eom_long, by = c("date", "Country")) %>%
  inner_join(Bonds_10Y3M_eom_long, by = c("date", "Country")) %>%
  inner_join(FX_eom_long, by = c("date", "Country"))


#3. Lagging and Sorting by Country

Merged_Bonds_FX <- Merged_Bonds_FX %>%
  arrange(Country, date) %>%  # Sort by Country and Date
  group_by(Country) %>%       # Group by Country for lagging
  mutate(
    P_10Y_prev = lag(P_10Y), # 10-year bond price at t-1
    FX_prev = lag(FX),
    P_3M_prev = lag(P_3M)# Exchange rate at t-1
  ) %>%
  ungroup()

#4. 

Merged_Bonds_FX <- Merged_Bonds_FX %>%
  mutate(
    excess_return_Total = (((P_10Y3M) / P_10Y_prev) - (1 / P_3M_prev)) * (FX / FX_prev)
  )

# Isolate Returns from Total Excess Return

Merged_Bonds_FX <- Merged_Bonds_FX %>%
  mutate(
    excess_return = ((((P_10Y3M) / P_10Y_prev) - (1 / P_3M_prev)) * (FX / FX_prev)) -1
  )



################################################################### added by Farkas ###################################################################
# # Example: floor small values in P_3M, P_10Y columns
# # Suppose you have these columns in your final data frames:
# #   P_3M, P_10Y, P_3M_prev, P_10Y_prev, etc.
# 
# Merged_Bonds_FX <- Merged_Bonds_FX %>%
#   mutate(
#     # If any of these are smaller than 1e-8 in absolute value, set them to 1e-8
#     P_3M      = if_else(abs(P_3M) < 1e-8, 1e-8, P_3M),
#     P_10Y     = if_else(abs(P_10Y) < 1e-8, 1e-8, P_10Y),
#     P_3M_prev = if_else(abs(P_3M_prev) < 1e-8, 1e-8, P_3M_prev),
#     P_10Y_prev= if_else(abs(P_10Y_prev) < 1e-8, 1e-8, P_10Y_prev)
#   ) %>%
#   # then (re)compute your excess_return
#   mutate(
#     excess_return = (((P_10Y - P_3M) / P_10Y_prev) - (1 / P_3M_prev)) * (FX / FX_prev)
#   )


##########################################################################


Bonds_ex_ret_long <- Merged_Bonds_FX %>%
  select(
    Country, date, excess_return
    )

Bonds_ex_ret_wide <- Bonds_ex_ret_long %>%
  pivot_wider(names_from = Country, values_from = excess_return)

annualized_returns <- Bonds_ex_ret_wide %>%
  mutate(across(-date, ~ (1 + .)^12 - 1))

#Calculate the Mean Excess Gov Bond Return

colMeans(annualized_returns[,-1], na.rm = TRUE)*100


# #######################################################################
# library(dplyr)
# library(tidyr)
# library(zoo)
# 
# # Assume 'annualized_returns' has columns: date, plus one column per country
# # e.g. date, UNITED STATES, GERMANY, JAPAN, etc.
# 
# annualized_returns_chopped <- annualized_returns %>%
#   # 1) Pivot to long format
#   pivot_longer(
#     cols = -date,
#     names_to = "Country",
#     values_to = "AnnReturn"
#   ) %>%
#   # 2) Arrange by date & group by country
#   group_by(Country) %>%
#   arrange(date, .by_group = TRUE) %>%
#   # 3) Chop values above +500% or below -500% (±5 in decimal), replace w/ NA
#   mutate(
#     AnnReturn = if_else(abs(AnnReturn) > 10, NA_real_, AnnReturn)
#   ) %>%
#   # 4) Fill newly introduced NAs forward, then backward
#   mutate(
#     AnnReturn = na.locf(AnnReturn, na.rm = FALSE),          # forward fill
#     AnnReturn = na.locf(AnnReturn, fromLast = TRUE, na.rm = FALSE) # backward fill
#   ) %>%
#   ungroup() %>%
#   # 5) Pivot back to wide format (optional)
#   pivot_wider(
#     names_from = "Country",
#     values_from = "AnnReturn"
#   )
# 
# # Now 'annualized_returns_chopped' looks like the original
# # but with outliers replaced and filled.


########################################################################################

#5. Plot Excess Returns

plot_list_ex_ret<- list()

for (i in 1:10) {
  column_name <- Countries[i]
  plot_list_ex_ret[[i]] <- ggplot(annualized_returns, aes(x = date, y = !!sym(column_name))) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Bond Excess Return", title = paste(column_name))+
    theme(
      axis.title.x = element_text(size = 5),  
      axis.title.y = element_text(size = 5),  
      axis.text.x = element_text(size = 4),    
      axis.text.y = element_text(size = 4),    
      plot.title = element_text(size = 7) 
    )
}

grid.arrange(grobs = plot_list_ex_ret, ncol = 2)



###################### explore data anomalies ######################




