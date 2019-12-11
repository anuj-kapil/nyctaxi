library(data.table)
library(tidyverse)
library(lubridate)
zip1 <- 'https://drive.google.com/uc?id=0B3o2JsiUpwEvLTRDNkEyZmZZM1U&export=download'
zip1csv <- data.table(read.table(unz(zip1, 'trip_fare_4.csv')))
?fread

con <- gzcon(url(paste("http://dumps.wikimedia.org/other/articlefeedback/",
                       "aa_combined-20110321.csv.gz", sep="")))
txt <- readLines(con)
dat <- read.csv(textConnection(txt))


temp <- tempfile(fileext = ".zip")
download.file(zip1, temp)
out <- unzip(temp, exdir = tempdir())

bank <- read.csv(out[14], sep = ";")
str(bank)

getwd()

gc()

trip_fare <- fread('Data/trip_fare_4.csv')
trip_data <- fread('Data/trip_data_4.csv')

join_keycols <- names(trip_fare)[1:4]
setkeyv(trip_fare, join_keycols)
setkeyv(trip_data, join_keycols)

trip_fare[, .N]
#15100468

uniqueN(trip_fare, by = key(trip_fare))
#15099816

uniqueN(trip_fare)
#15100414

trip_fare <- unique(trip_fare, by = key(trip_fare))


trip_data[,.N]
#15100468

uniqueN(trip_data, by = key(trip_data))

uniqueN(trip_data)
#15100459

trip_data <- unique(trip_data, by = key(trip_fare))


trip_combined <- trip_fare[trip_data, nomatch = 0]
trip_combined[, .N]
#15101772

uniqueN(trip_combined)
#15101654

?setkeyv
set.seed(131)
trip_combined[, sample_flg := sample(c(TRUE, FALSE), size = .N, replace = TRUE, prob = c(0.1, 0.9))]

trip_combined_sample <- trip_combined[sample_flg == T]

fwrite(trip_combined_sample, 'Data/trip_combined_sample.csv')

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


p1_sample <- trip_combined_sample %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

p1_sample

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

p4_sample <- trip_combined_sample %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")

p4_sample



p5 <- trip_data %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

p5

p5_sample <- trip_combined_sample %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

p5_sample


p6 <- trip_data %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick) %>%
  count() %>%
  ggplot(aes(reorder(hpick, -n), n, fill = reorder(hpick, -n))) +
  geom_col() +
  scale_y_sqrt() +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

p6

p6_sample <- trip_combined_sample %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick) %>%
  count() %>%
  ggplot(aes(reorder(hpick, -n), n, fill = reorder(hpick, -n))) +
  geom_col() +
  scale_y_sqrt() +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

p6_sample


trip_data
  


