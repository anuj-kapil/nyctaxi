library(data.table)
library(tidyverse)
library(lubridate)
# zip1 <- 'https://drive.google.com/uc?id=0B3o2JsiUpwEvLTRDNkEyZmZZM1U&export=download'
# zip1csv <- data.table(read.table(unz(zip1, 'trip_fare_4.csv')))
# ?fread
# 
# url_csv_gz <- 'http://github.com/anuj-kapil/nyctaxi/blob/master/Data/trip_combined_sample.csv.gz?raw=true'
# 
# con <- gzcon(url(url_csv_gz))
# txt <- readLines(con)
# dat <- read.csv(textConnection(txt))
# 
# 
# temp <- tempfile(fileext = ".gz")
# download.file(url_csv_gz, temp)
# out <- unzip(temp, exdir = tempdir())
# 
# bank <- read.csv(out[14], sep = ";")
# str(bank)
# 
# getwd()
# 
# gc()

###########################################
# Prepare sample dataset

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

set.seed(131)
trip_combined[, sample_flg := sample(c(TRUE, FALSE), size = .N, replace = TRUE, prob = c(0.1, 0.9))]

trip_combined_sample <- trip_combined[sample_flg == T]

fwrite(trip_combined_sample, 'Data/trip_combined_sample.csv')
###########################################

trip_combined_sample <- fread('Data/trip_combined_sample.csv.gz')

hist(trip_combined_sample$passenger_count)

# p1 <- trip_data %>%
#   group_by(passenger_count) %>%
#   count() %>%
#   ggplot(aes(passenger_count, n, fill = passenger_count)) +
#   geom_col() +
#   scale_y_sqrt() +
#   theme(legend.position = "none")
# 
# p1

trip_combined_sample$passenger_count <- as.factor(trip_combined_sample$passenger_count)
p1_sample <- trip_combined_sample %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

p1_sample

# summary(trip_combined_sample)
# ggplot(data=trip_combined_sample, aes(x=passenger_count)) + 
#   geom_histogram(breaks=seq(0, 10, by=1), 
#                  col="red", 
#                  fill="green", 
#                  alpha = .2) + 
#   labs(title="Histogram for Passenger Count", x="Passenger Count", y="Count") + 
#   xlim(c(-1,10)) 
#+ 
#  ylim(c(0,30))

# 
# p4 <- trip_data %>%
#   mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
#   group_by(wday, vendor_id) %>%
#   count() %>%
#   ggplot(aes(wday, n, colour = vendor_id)) +
#   geom_point(size = 4) +
#   labs(x = "Day of the week", y = "Total number of pickups") +
#   theme(legend.position = "none")
# 
# p4


trip_combined_sample$payment_type <- as.factor(trip_combined_sample$payment_type)
p2_sample <- trip_combined_sample %>%
  group_by(payment_type) %>%
  count() %>%
  ggplot(aes(payment_type, n, fill = payment_type)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

p2_sample


summary(trip_combined_sample)
hist(trip_combined_sample$fare_amount)
p3_sample <- trip_combined_sample %>%
  ggplot(aes(fare_amount)) +
  geom_histogram(binwidth = 20) +
  #scale_y_sqrt() +
  theme(legend.position = "none")

p3_sample

p3_filtered_sample <- trip_combined_sample %>%
  filter(fare_amount<=75) %>%
  ggplot(aes(fare_amount)) +
  geom_histogram(binwidth = 5) +
  #scale_y_sqrt() +
  theme(legend.position = "none")

p3_filtered_sample


hist(trip_combined_sample$tip_amount)


p4_sample <- trip_combined_sample %>%
  ggplot(aes(tip_amount)) +
  geom_histogram(binwidth = 10) +
  #scale_y_sqrt() +
  theme(legend.position = "none")

p4_sample

p4_filtered_sample <- trip_combined_sample %>%
  filter(tip_amount<=20) %>%
  ggplot(aes(tip_amount)) +
  geom_histogram(binwidth = 2.5) +
  #scale_y_sqrt() +
  theme(legend.position = "none")

p4_filtered_sample



hist(trip_combined_sample$total_amount)
p5_sample <- trip_combined_sample %>%
  ggplot(aes(total_amount)) +
  geom_histogram(binwidth = 20) +
  #scale_y_sqrt() +
  theme(legend.position = "none")

p5_sample

p5_filtered_sample <- trip_combined_sample %>%
  filter(total_amount<=75) %>%
  ggplot(aes(total_amount)) +
  geom_histogram(binwidth = 5) +
  #scale_y_sqrt() +
  theme(legend.position = "none")

p5_filtered_sample

summary(trip_combined_sample$trip_time_in_secs)

hist(trip_combined_sample$trip_time_in_secs)


ggplot(data=trip_combined_sample, aes(x=trip_time_in_secs)) +
  geom_histogram(breaks=seq(0, 10, by=1),
                 col="red",
                 fill="green",
                 alpha = .2) +
  labs(title="Histogram for Passenger Count", x="Passenger Count", y="Count") +
  xlim(c(-1,10))

10000/60/60


plot_sd <- trip_combined_sample[, sd(trip_time_in_secs), by = cut_interval(trip_time_in_secs/60/60, 12)]

ggplot(data=plot_sd, aes(x=cut_interval, y = V1, fill = cut_interval)) +
  geom_col() +
  labs(title="Histogram", x="Time interval", y="Count")

trip_combined_sample[trip_time_in_secs <= 15*60, sd(trip_time_in_secs)]

trip_combined_sample[trip_time_in_secs <= 30*60, sd(trip_time_in_secs)]

trip_combined_sample[trip_time_in_secs > 30*60, sd(trip_time_in_secs)]

trip_combined_sample[trip_time_in_secs > 4*30*60, sd(trip_time_in_secs)]



plot_sd_fare <- trip_combined_sample[, sd(total_amount), by = cut_interval(total_amount, 20)]

ggplot(data=plot_sd_fare, aes(x=cut_interval, y = V1, fill = cut_interval)) +
  geom_col() +
  labs(title="Histogram", x="Fare interval", y="Count")








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


trip_combined_sample[, .N]

hist(trip_combined_sample$pickup_latitude)


ggplot(head(trip_combined_sample, 1000), aes(x=pickup_longitude, y= pickup_latitude), size=2200) +
  geom_point()


library(plotly)


#plot_ly(data = trip_combined_sample, x = ~pickup_longitude, y = ~pickup_latitude)

# oultier analysis

ggplot(trip_combined_sample, aes(x="pickup_latitude", y=pickup_latitude))+geom_boxplot()

trip_combined_sample <- trip_combined_sample[pickup_latitude %between% c(-90, 90) & pickup_longitude %between% c(-180, 180)]
trip_combined_sample[, .N]

trip_combined_sample <- trip_combined_sample[pickup_latitude %between% c(30, 50) & pickup_longitude %between% c(-84, -64)]


ggplot(trip_combined_sample, aes(x="pickup_longitude", y=pickup_longitude))+geom_boxplot()

boxplot.stats(trip_combined_sample$pickup_longitude)
ggplot(trip_combined_sample, aes(x=pickup_latitude, y=pickup_longitude))+geom_point()

summary(trip_combined_sample)

