# Required Libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")




# Load Files: We only need Close Price and Date for at least 2 years
AAL <- read.csv("./data/AAL.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
  select(Date, Close) %>% 
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= "2017-01-01" & Date < "2019-04-01")

DAL <- read.csv("./data/DAL.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
  select(Date, Close) %>% 
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= "2017-01-01" & Date < "2019-04-01")




# Data Analysis
str(AAL)
str(DAL)
# The variable Date is the date type and the variable Close is the numeric type

head(AAL)
head(DAL)
# Not all dates are registered

summary(AAL)
# Observations begin on 2017-01-03 and end on 2019-03-29
# The minimum closing price was 29.72 and the maximum was 58.47
# The mean is 43.89

summary(DAL)
# Observations begin on 2017-01-03 and end on 2019-03-29
# The minimum closing price was 44.03 and the maximum was 60.71
# The mean is 51.95






# Data Visualization
plot(AAL, type = "l")
title(main = "AAL Close Price")

ggplot(data = AAL, aes(x = Date, y = Close))+
  geom_line(color = "red", size = 1) + 
  stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") +
  ggtitle("AAL Close Price")

plot(DAL, type = "l")
title(main = "DAL Close Price")

ggplot(data = DAL, aes(x = Date, y = Close))+
  geom_line(color = "blue", size = 1) + 
  stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") +
  ggtitle("DAL Close Price")

ggplot(AAL, aes(Date, Close)) +
  geom_line(aes(color="AAL")) + 
  geom_line(data = DAL, aes(color = "DAL")) +
  ggtitle("Close price") +
  labs(color="Legend") +
  scale_color_manual("", 
                     breaks = c("AAL", "DAL"),
                     values = c("red", "blue"))







#The first thing we must do with the data in converting them into time series using the ts function

# But not all observations are registered, we complete all days
AAL.AllDates <- data.frame("Date" = seq(AAL$Date[1], as.Date("2019-03-31"), "day"))
DAL.AllDates <- data.frame("Date" = seq(DAL$Date[1], as.Date("2019-03-31"), "day"))

AAL <- full_join(AAL.AllDates, AAL, by="Date")
DAL <- full_join(DAL.AllDates, DAL, by="Date")
summary(AAL)
summary(DAL)
# Now is AAL and DAL completed, but with 255 NA's each one

# Cleaning
rm(AAL.AllDates, DAL.AllDates)



# Day of the year
AAL.j <- as.numeric(format(AAL$Date[1], "%j"))
DAL.j <- as.numeric(format(DAL$Date[1], "%j"))

# Convert to Time Series
AAL <- ts(AAL$Close, start = c(as.numeric(format(AAL$Date[1], "%Y")), AAL.j), frequency = 365)
DAL <- ts(DAL$Close, start = c(as.numeric(format(DAL$Date[1], "%Y")), DAL.j), frequency = 365)

# Cleaning
rm(AAL.j, DAL.j)








# But not all dates are registered
# Complete all days with interpolated values
AAL <- round(na.interp(AAL), 2)
DAL <- round(na.interp(DAL), 2)

summary(AAL)
summary(DAL)

plot(AAL)
plot(DAL)






#decompose
#Classical Seasonal Decomposition by Moving Averages
AAL.decompose <- decompose(AAL)
DAL.decompose <- decompose(DAL)

plot(AAL.decompose)
plot(DAL.decompose)


#log(a*b) = log (a) + log(b)
#Seasonal Decomposition of Time Series by Loess
AAL.l <- log(AAL)
DAL.l <- log(DAL)

AAL.stl <- stl(AAL, s.window = "periodic")
DAL.stl <- stl(DAL, s.window = "periodic")

plot(AAL.stl)
plot(DAL.stl)



#SES
AAL.ses <- ses(AAL, h = 5)
DAL.ses <- ses(DAL, h = 5)

plot(AAL.ses)
plot(DAL.ses)

#holt
AAL.holt <- holt(AAL, h = 5)
DAL.holt <- holt(DAL, h = 5)

plot(AAL.holt)
plot(DAL.holt)


#modelo autorregresivo integrado de media móvil
AAL.arima <- auto.arima(AAL)
DAL.arima <- auto.arima(DAL)

summary(AAL.arima)
summary(DAL.arima)

AAL.fore.ari <- forecast(AAL.arima, h = 90)
DAL.fore.ari <- forecast(DAL.arima, h = 90)

plot(AAL.fore.ari, col = "red", fcol = "green")
plot(DAL.fore.ari, col = "blue", fcol = "green")



#HoltWinters
AAL.hw <- HoltWinters(AAL)
AAL.hw.l <- HoltWinters(AAL.l)
DAL.hw <- HoltWinters(DAL)

plot(AAL.hw, col = "red", col.predicted = "grey")
plot(AAL.hw.l, col = "red", col.predicted = "grey")

plot(DAL.hw, col = "blue", col.predicted = "grey")




#Forecast 30 days
AAL.fore <- forecast(AAL.hw, h=30)
DAL.fore <- forecast(DAL.hw, h=30)

plot(AAL.fore)
plot(DAL.fore)


AAL.fore$lower[1:5,]
AAL.fore$upper[1:5,]
