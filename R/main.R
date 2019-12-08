library(data.table)
library(tidyverse)

zip1 <- 'https://drive.google.com/uc?id=0B3o2JsiUpwEvLTRDNkEyZmZZM1U&export=download'
zip1csv <- data.table(read.table(unz(zip1, 'trip_fare_4.csv')))
?fread


temp <- tempfile(fileext = ".zip")
download.file(zip1, temp)
out <- unzip(temp, exdir = tempdir())

bank <- read.csv(out[14], sep = ";")
str(bank)

getwd()


trip_fare <- fread('Data/trip_fare_4.csv')
trip_data <- fread('Data/trip_data_4.csv')

trip_fare
hist(trip_data$passenger_count)


p1 <- trip_data %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

p1

summary(trip_data)
ggplot(data=trip_data, aes(x=passenger_count)) + 
  geom_histogram(breaks=seq(0, 10, by=1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Passenger Count", x="Passenger Count", y="Count") + 
  xlim(c(-1,10)) 
 #+ 
#  ylim(c(0,30))

library(lubridate)

p4 <- trip_data %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")

p4


p5 <- trip_data %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")



