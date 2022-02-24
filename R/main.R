
# Install CRAN packages
list.of.packages <- c(
  "data.table",
  "tidyverse",
  "lubridate",
  "plotly",
  "leaflet",
  "geosphere",
  "ggthemes"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, repos = "https://cran.rstudio.com/", dependencies = T)


library(data.table)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(geosphere)
library(ggthemes)

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

# Trips
trip_combined_sample[,.N]

# Vendor
trip_combined_sample[, uniqueN(vendor_id)]

# Medallions
trip_combined_sample[, uniqueN(medallion)]

# Drivers
trip_combined_sample[, uniqueN(hack_license)]

# Trip distance total
trip_combined_sample[, sum(trip_distance)]


# Missing data
glimpse(trip_combined_sample)

# Let's convert date strings to date time format
trip_combined_sample[, pickup_datetime := as.POSIXct(pickup_datetime,format="%Y-%m-%d %H:%M:%OS")]
trip_combined_sample[, dropoff_datetime := as.POSIXct(dropoff_datetime,format="%Y-%m-%d %H:%M:%OS")]

summary(trip_combined_sample)

# One month of data from April 2013

# Lets remove the null values from lat and long
trip_combined_sample <- trip_combined_sample[!is.na(dropoff_longitude) & !is.na(dropoff_latitude)]

# Also, looking at the range of the values of lat and long, it appears the data is either incorrect or captured in a different format
# Assuming lat and long are in degrees. For our analysis we will remove an erroneous data

# Lat long range (lat from -90 to 90 and long from -180 to 180)

trip_combined_sample <- trip_combined_sample[pickup_latitude %between% c(-90, 90) & pickup_longitude %between% c(-180, 180)]
trip_combined_sample <- trip_combined_sample[dropoff_latitude %between% c(-90, 90) & dropoff_longitude %between% c(-180, 180)]

summary(trip_combined_sample$pickup_latitude)
summary(trip_combined_sample$pickup_longitude)
summary(trip_combined_sample$dropoff_latitude)
summary(trip_combined_sample$dropoff_longitude)

# Since we are dealing with New York data, the lat and long can further be limited to New York and nearby 

# outlier analysis

hist(trip_combined_sample$pickup_latitude,
      main="Histogram for Pickup Latitude", 
      xlab="Pickup Latitude", 
      border="black", 
      col="lightblue")

ggplot(trip_combined_sample, aes(x="pickup_latitude", y=pickup_latitude)) +
  geom_boxplot() +
  labs(x = "", y = "")+
  ggtitle("Box Plot for Pickup Latitude") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_tableau()

# We know that NYC is in northern hemisphere. Some of the pickups are from equator (latitude = 0) and some are from southern hemisphere
# Clearly data is wrongly captured or there were errors in capturing the data

hist(trip_combined_sample$pickup_longitude,
     main="Histogram for Pickup Longitude", 
     xlab="Pickup Longitude", 
     border="black", 
     col="lightblue")

ggplot(trip_combined_sample, aes(x="pickup_longitude", y=pickup_longitude)) +
  geom_boxplot() +
  labs(x = "", y = "")+
  ggtitle("Box Plot for Pickup Longitude") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_tableau()

hist(trip_combined_sample$dropoff_latitude,
     main="Histogram for Dropoff Latitude", 
     xlab="Dropoff Latitude", 
     border="black", 
     col="lightblue")

ggplot(trip_combined_sample, aes(x="dropoff_latitude", y=dropoff_latitude)) +
  geom_boxplot() +
  labs(x = "", y = "")+
  ggtitle("Box Plot for Dropoff Latitude") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_tableau()


hist(trip_combined_sample$dropoff_longitude,
     main="Histogram for Dropoff Longitude", 
     xlab="Dropoff Longitude", 
     border="black", 
     col="lightblue")

ggplot(trip_combined_sample, aes(x="dropoff_longitude", y=dropoff_longitude)) +
  geom_boxplot() +
  labs(x = "", y = "")+
  ggtitle("Box Plot for Dropoff Longitude") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_tableau()

# NYC range within 1000 kms radius (1 degree equal approx 111 kms) and NYC coordinates are 40.7141667 lat and -74.0063889 long
trip_combined_sample <- trip_combined_sample[pickup_latitude %between% c(30, 50) & pickup_longitude %between% c(-84, -64)]
trip_combined_sample <- trip_combined_sample[dropoff_latitude %between% c(30, 50) & dropoff_longitude %between% c(-84, -64)]


trip_combined_sample[, .N]
boxplot.stats(trip_combined_sample$pickup_longitude)

# Let's visualize the lat longs

ggplot(head(trip_combined_sample, 1000), aes(x=pickup_longitude, y= pickup_latitude)) +
  geom_point()

leaflet(data = head(trip_combined_sample, 1000)) %>%
  addTiles()%>%
  #addProviderTiles("Stamen.TonerLite")%>%
  #addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "navy", fillOpacity = 0.3)


# Remove zero distance trips
summary(trip_combined_sample)
trip_combined_sample <- trip_combined_sample[trip_distance > 0]

# Remove zero time trips
trip_combined_sample <- trip_combined_sample[trip_time_in_secs > 0]

summary(trip_combined_sample$trip_time_in_secs)

trip_combined_sample[trip_time_in_secs>1849.257][order(-trip_time_in_secs)]

ggplot(trip_combined_sample, aes(x="trip_time_in_secs", y=trip_time_in_secs))+geom_boxplot()

trip_combined_sample[,.(min(trip_time_in_secs), max(trip_time_in_secs),mean(trip_time_in_secs) + 2 * sd(trip_time_in_secs),  mean(trip_time_in_secs) - 2 * sd(trip_time_in_secs))]


hist(trip_combined_sample$trip_time_in_secs)

hist(trip_combined_sample$fare_amount)

trip_combined_sample[fare_amount>=500]


# Univariate Analysis

plot_passenger_dist <- trip_combined_sample[, .N, by = list(passenger_count)]

plot_passenger_dist$passenger_count <- as.factor(plot_passenger_dist$passenger_count)
ggplot(plot_passenger_dist, aes(passenger_count, N, fill = passenger_count)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Number of Passengers", y = "Number of Trips (April 2013)")+
  ggtitle("Number of Passengers/Trip") +
  theme_bw() +
  theme(panel.border = element_blank(),  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  scale_colour_tableau()

# Count trips group by payment type
plot_payment_type_dist <- trip_combined_sample[, .N, by = list(payment_type)]

# Convert the payment type to a categorical variable
plot_payment_type_dist$payment_type <- as.factor(plot_payment_type_dist$payment_type)

# Visualize
ggplot(plot_payment_type_dist, aes(payment_type, N/sum(N), fill = payment_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Payment Type", y = "Number of Trips")+
  ggtitle("Payment Type/Trip") +
  theme_bw() +
  theme(panel.border = element_blank(),  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  scale_colour_tableau()

### Distribution of Fare Amount
ggplot(trip_combined_sample, aes(fare_amount)) +
  geom_histogram(binwidth = 20,
                 col="black", 
                 fill="light blue") +
  labs(x = "Fare Amount", y = "Number of Trips")+
  ggtitle("Histogram of Fare Amount") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  scale_colour_tableau()

hist(trip_combined_sample$tip_amount)

### Distribution of Tip Amount

ggplot(trip_combined_sample, aes(tip_amount)) +
  geom_histogram(binwidth = 10,
                 col="black", 
                 fill="light blue") +
  labs(x = "Tip Amount", y = "Number of Trips")+
  ggtitle("Histogram of Tip Amount") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  scale_colour_tableau()


hist(trip_combined_sample$total_amount)

ggplot(trip_combined_sample, aes(total_amount)) +
  geom_histogram(binwidth = 20,
                 col="black", 
                 fill="light blue") +
  labs(x = "Total Amount", y = "Number of Trips")+
  ggtitle("Histogram of Total Amount") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") + 
  scale_colour_tableau()

# summary(trip_combined_sample$trip_time_in_secs)
# 
# hist(trip_combined_sample$trip_time_in_secs)
# 
# 
# ggplot(data=trip_combined_sample, aes(x=trip_time_in_secs)) +
#   geom_histogram(breaks=seq(0, 10000, by=500),
#                  col="red",
#                  fill="green",
#                  alpha = .2) +
#   labs(title="Histogram for Passenger Count", x="Passenger Count", y="Count") 
# 
# 10000/60/60
# 
# 

trip_combined_sample[, qtr_hr_bin := cut(trip_time_in_secs/60/60, 
                                        seq(0,3, by = .25), 
                                        include.lowest = TRUE, 
                                        right = FALSE)]

plot_sd <- trip_combined_sample[, .(sd_trip_duration = sd(trip_time_in_secs)), by = list(qtr_hr_bin)]

ggplot(plot_sd, aes(reorder(qtr_hr_bin, -sd_trip_duration), sd_trip_duration, fill = reorder(qtr_hr_bin, -sd_trip_duration))) +
  geom_col() +
  scale_y_sqrt() +
  labs(x = "Trip Duration (in hrs) quarter hour bins", y = "Standard Deviation") +
  ggtitle("Trip Duration Standard Deviation by quarter hour bins") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

ggplot(data=plot_sd, aes(x=cut_interval, y = V1, fill = cut_interval)) +
  geom_col() +
  labs(title="Histogram", x="Time interval", y="Count")
# 
# trip_combined_sample[trip_time_in_secs <= 15*60, sd(trip_time_in_secs)]
# 
# trip_combined_sample[trip_time_in_secs <= 30*60, sd(trip_time_in_secs)]
# 
# trip_combined_sample[trip_time_in_secs > 30*60, sd(trip_time_in_secs)]
# 
# trip_combined_sample[trip_time_in_secs > 4*30*60, sd(trip_time_in_secs)]
# 
# 
# 
# plot_sd_fare <- trip_combined_sample[, sd(total_amount), by = cut_interval(total_amount, 20)]
# 
# ggplot(data=plot_sd_fare, aes(x=cut_interval, y = V1, fill = cut_interval)) +
#   geom_col() +
#   labs(title="Histogram", x="Fare interval", y="Count")
# 

# Feature Engineering

trip_combined_sample[, fare_per_dist := (fare_amount/trip_distance)]

# Calculate 2 standard deviations
trip_combined_sample[,.(min_fare = min(fare_per_dist), 
                        max_fare = max(fare_per_dist),
                        sd_2_fare_upper = mean(fare_per_dist) + 2 * sd(fare_per_dist), 
                        sd_2_fare_lower = mean(fare_per_dist) - 2 * sd(fare_per_dist)
)
]

# Visualize if the trip duration has an impact on the high fare_per_dist for lower trip distances

ggplot(head(trip_combined_sample, 10000)) + 
  geom_point(aes(x=trip_distance, y=fare_per_dist, col = (trip_time_in_secs/60))) + 
  labs(x = "Trip Distance", y = "Fare amount per unit distance") +
  labs(col = "Trip Time (in mins)") +
  ggtitle("Fare amount per unit distance Vs trip distance & time") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))


trip_combined_sample <- trip_combined_sample[fare_per_dist < (mean(fare_per_dist) + 2 * sd(fare_per_dist))]

# Weekday
trip_combined_sample[, pickup_wday := wday(pickup_datetime, label = TRUE)]

plot_wday_trips <- trip_combined_sample[, .N, by = list(pickup_wday, vendor_id)]

ggplot(plot_wday_trips, aes(pickup_wday, N, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  labs(col = "Vendor") +
  ggtitle("Taxi trips by day of the week") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))


# Hour of the day

trip_combined_sample[, pickup_hour := hour(pickup_datetime)]

plot_hourly_trips <- trip_combined_sample[, .N, by = list(pickup_hour)]

ggplot(plot_hourly_trips, aes(reorder(pickup_hour, -N), N, fill = reorder(pickup_hour, -N))) +
  geom_col() +
  scale_y_sqrt() +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  ggtitle("Taxi trips by hour of the day") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")


plot_hourly_fares <- trip_combined_sample[, .(mean_hourly_fare = mean(fare_amount)), by = list(pickup_hour)]

ggplot(plot_hourly_fares, aes(reorder(pickup_hour, -mean_hourly_fare), mean_hourly_fare, fill = reorder(pickup_hour, -mean_hourly_fare))) +
  geom_col() +
  scale_y_sqrt() +
  labs(x = "Hour of the day", y = "Average Fare Amount") +
  ggtitle("Fare amount by hour of the day") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

plot_hourly_trip_duration <- trip_combined_sample[, .(mean_hourly_trip_duration = mean(trip_time_in_secs)), by = list(pickup_hour)]

ggplot(plot_hourly_trip_duration, aes(reorder(pickup_hour, -mean_hourly_trip_duration), mean_hourly_trip_duration, fill = reorder(pickup_hour, -mean_hourly_trip_duration))) +
  geom_col() +
  scale_y_sqrt() +
  labs(x = "Hour of the day", y = "Average Trip Duration") +
  ggtitle("Trip Duration by hour of the day") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# Mean Fare amount and mean distance
trip_combined_sample[ , .(mean(fare_amount), mean(trip_time_in_secs)), by = list(pickup_hour)]

# Mean fare amount per unit distance
trip_combined_sample[ , .(sum(fare_amount)/sum(trip_distance)), by = list(passenger_count)]

# Plotting fare against distance
ggplot(head(trip_combined_sample, 10000), aes(x=fare_amount, y=trip_distance))+geom_point()

# Linear relationship

# The vertical line in the plot might indicate again the fixed fare trips to/from JFK airport.

jfk = c(-73.7822222222, 40.6441666667)
nyc = c(-74.0063889, 40.7141667)
ewr = c(-74.175, 40.69) # Newark Liberty International Airport, see https://www.travelmath.com/airport/EWR
lgr = c(-73.87, 40.77) # LaGuardia Airport, see https://www.travelmath.com/airport/LGA

# hist where pickup from JFK
hist(trip_combined_sample[round(pickup_latitude,2) == 40.64 & round(pickup_longitude,2) == -73.78]$fare_amount)

# hist where drop-off JFK
hist(trip_combined_sample[round(dropoff_latitude,2) == 40.64 & round(dropoff_longitude,2) == -73.78]$fare_amount)

trip_combined_sample[, fare_per_dist := NULL]
trip_combined_sample[, fare_per_dist := (fare_amount/trip_distance)]

trip_combined_sample[,.(min(fare_per_dist), max(fare_per_dist),mean(fare_per_dist) + 4 * sd(fare_per_dist),  mean(fare_per_dist) - 4 * sd(fare_per_dist))]

trip_combined_sample[fare_per_dist > 48][order(-fare_per_dist)]

trip_combined_sample[fare_amount >= 500]

trip_combined_sample[fare_per_dist == 250]

trip_combined_sample$trip_time_in_secs
ggplot(head(trip_combined_sample, 10000), aes(x=trip_distance, y=fare_per_dist, col = (trip_time_in_secs/60)))+geom_point()


summary(trip_combined_sample)

trip_combined_sample[, pickup_datetime_dt := as.POSIXct(pickup_datetime,format="%Y-%m-%d %H:%M:%OS")]

plot_mean_fare_per_dist <- trip_combined_sample[, .(mean_fare_per_dist = mean(fare_per_dist)) , by = hour(pickup_datetime_dt)]

ggplot(plot_mean_fare_per_dist, aes(x=hour, y=mean_fare_per_dist))+geom_line(color = 'red')

plot_mean_time <- trip_combined_sample[, .(mean_time = mean(trip_time_in_secs)) , by = hour(pickup_datetime_dt)]

ggplot(plot_mean_time, aes(x=hour, y=mean_time))+geom_line(color = 'red')


plot_mean_time_fare <- trip_combined_sample[, .(mean_fare_per_dist = mean(fare_per_dist), mean_time = mean(trip_time_in_secs/60)) , by = hour(pickup_datetime)]

ggplot(plot_mean_time_fare) +
  geom_line(aes(x=hour, y=mean_fare_per_dist), color = 'red') + 
  geom_line(aes(x=hour, y=mean_time), color = 'blue')

trip_combined_sample[, list(pickup_longitude, pickup_latitude)]
trip_combined_sample$pickup_dist_from_city <- distm(trip_combined_sample[,list(pickup_longitude, pickup_latitude)], nyc, fun = distHaversine)
?distm
hist(trip_combined_sample[pickup_dist_from_city<25000]$pickup_dist_from_city)

summary(trip_combined_sample)

trip_combined_sample <- trip_combined_sample[pickup_dist_from_city<100000]
trip_combined_sample[, .N]

trip_combined_sample$fare_amount

ggplot(head(trip_combined_sample, 50000)) +
  geom_point(aes(x=pickup_dist_from_city, y=trip_distance, col = fare_amount)) +
  scale_color_gradientn(colours = rainbow(5))


?cut
