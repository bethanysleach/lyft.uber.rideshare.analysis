library(readr)
library(rgdal)
library(broom)
library(ggplot2)
library(sf)
library(ggmap)
library(rstudioapi)
library(RColorBrewer)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(calendR)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(ggsn)
library(ggrepel)
library(grid)
library(forcats)
library(magrittr)


lyft.uber.dataset <- read_csv("/Users/bethanyleach/Downloads/rideshare_kaggle.csv")
lyft.uber.revised <- as_tibble(lyft.uber.dataset)
str(lyft.uber.revised)

lyft.uber.remove.na <- lyft.uber.revised[complete.cases(lyft.uber.revised), ]
View(lyft.uber.remove.na)

register_google("AIzaSyBZx2Za3bJfe2OY0QxgPgef-4GX9jd61Pg")

#b+w boston map with sources 
BostonMap <- qmap("boston", zoom = 12, color = "bw", legend = "topright")
combined_map_boston <- BostonMap + geom_point(aes(x=longitude, y=latitude), data=lyft.uber.remove.na)

#coloredBostonmap
boston2 <- get_map("boston", zoom=12)
ggmap(boston2)

#further detail
extent <- bb2bbox(attr(boston2, "bb"))
ggmap(get_stamenmap(extent, zoom=12))

#boston map with labeled center
boston_bb <- c(
        left=-71.1319,
        bottom = 42.3267,
        right = -71.0028,
        top = 42.3752
)
boston_stamen <- get_stamenmap(
        bbox = boston_bb,
        zoom = 14
)
ggmap(boston_stamen)

#less detailed map
boston_center <- c(lon=-71.0789, lat=42.3480)
boston_google <- get_googlemap(center=boston_center)
ggmap(boston_google)

get_googlemap(
        center = boston_center, 
        zoom = 12
) %>%
        ggmap()

#using stamen map to get map background
#density maps
boston3 <- boston_stamen
ggmap(boston3)
neighborhood_map <- ggmap(boston3) + geom_point(data=lyft.uber.remove.na, aes(x=longitude, y=latitude), size=0.05, alpha =0.05)
ggmap(boston3) + geom_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude))
ggmap(boston3) + stat_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude, fill = stat(level)), geom="polygon")
ggmap(boston3) + stat_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude, fill = stat(level)), alpha = 0.05, bins = 50, geom="polygon") + 
        scale_fill_gradientn(colors = brewer.pal(7, "Greens"))


#labeled neighborhood map
lat <- c(42.364, 42.352, 42.3398, 42.3503, 42.3505, 42.3505, 42.3519, 42.3559, 42.3588, 42.3647, 42.3661, 42.3661)
lon <- c(-71.060, -71.065, -71.0892, -71.0810, -71.1054, -71.1054, -71.0551, -71.0550, -71.0707, -71.0542, -71.0631, -71.0631)
labels <- c("Haymarket Square", "Theatre District", "Northeastern University", "Back Bay", "Boston University",
            "Fenway", "South Station", "Financial District", "Beacon Hill", "North End", "North Station", "West End")

combined_df <- data.frame(lat, lon, labels)
neighborhood_map_adj <- ggmap(boston3) + geom_point(data=combined_df, aes(x=lon, y=lat), 
                                                    size=0.5, alpha =0.5, fill="red") + labs(x = 'Longitude', y = 'Latitude') +
                                                    geom_label_repel(data=combined_df, aes(x = lon, y= lat,
                                                                                           label = labels), fill = "white",
                                                                                           box.padding = unit(.4, "lines"),
                                                                                           label.padding = unit(.15, "lines"),
                                                                                           segment.color = "red", segment.size = 1)


#avg price by source & neighborhood map
avg_price_simulator <- lyft.uber.remove.na %>%
        group_by(source) %>%
        mutate(average_price=mean(price))
avg_price_simulator$average_price

View(avg_price_simulator[c(8, 16, 17, 58)])
unique(avg_price_simulator$average_price)

neighborhood_avg_price <- avg_price_simulator[c(1, 8, 16, 17, 58)]
neighborhood_avg_price
neighborhood_avg_price_final <- unique(neighborhood_avg_price)

remove_dups <- neighborhood_avg_price_final[!duplicated(neighborhood_avg_price_final$source), ]

lat <- c(42.364, 42.352, 42.3398, 42.3503, 42.3505, 42.3505, 42.3519, 42.3559, 42.3588, 42.3647, 42.3661, 42.3661)
lon <- c(-71.060, -71.065, -71.0892, -71.0810, -71.1054, -71.1054, -71.0551, -71.0550, -71.0707, -71.0542, -71.0631, -71.0631)
labels_price <- c("Haymarket Square = $13.60", "Theatre District = $16.60", "Northeastern University = $17.90", "Back Bay = $16.00", 
            "Boston University = $18.90", "Fenway = $18.40", "South Station = $15.70", "Financial District = $18.20", "Beacon Hill = $15.70", 
            "North End = $15.20", "North Station = $16.40", "West End = $16.10")

combined_df_price <- data.frame(lat, lon, labels_price)
neighborhood_map_adj_price <- ggmap(boston3) + geom_point(data=combined_df_price, aes(x=lon, y=lat), 
                                                    size=0.5, alpha =0.5, fill="red") + labs(x = 'Longitude', y = 'Latitude') +
                                                    geom_label_repel(data=combined_df_price, aes(x = lon, y= lat,
                                                                                           label = labels_price), fill = "white",
                                                                                           size = 4,
                                                                                           box.padding = unit(.4, "lines"),
                                                                                           label.padding = unit(.15, "lines"),
                                                                                           segment.color = "red", segment.size = 1) +
                                                                                           ggtitle("Average Price Per Station in Boston, MA")

#total number of rides by hour 
hour_rides_data <- lyft.uber.remove.na %>%
        dplyr::group_by(hour) %>%
        summarize(total_rides = n())
datatable(hour_rides_data)
hour_rides_data

ggplot(hour_rides_data, aes(hour, total_rides)) + 
        geom_bar(stat="identity", fill="blue", color="orange") + 
        theme(legend.position="none") + ggtitle("Total Rides Per Hour") + 
        scale_y_continuous(labels=comma) + ylab("Total Rides") + 
        theme(plot.title = element_text(hjust=0.5))


#separating datetime column in month being November
uber_lyft_rides_per_day_november$Date <- as.Date(uber_lyft_rides_per_day_november$datetime)
uber_lyft_rides_per_day_november$Time <- format(as.POSIXct(uber_lyft_rides_per_day_november$datetime), 
                                                format = "%H:%M:%S") 
unique(uber_lyft_rides_per_day_november$day)

begin_nov <- as.POSIXct("2018-11-26")
finish_nov <- as.POSIXct("2018-11-30")

dat3 <- data.frame(Date = seq.POSIXt(from = begin_nov, to = finish_nov, by ="DSTday"))

dat3$weekday1 <- as.numeric(format(dat3$Date, format = "%u"))
dat3$weekday2 <- format(dat3$Date, format = "%a")
dat3$weekday3 <- format(dat3$Date, format = "%A")

uber_lyft_november_merge_dates <- uber_lyft_rides_per_day_november %>%
        full_join(dat3, by="Date")

uber_lyft_november_merge_dates$weekday3 <- ordered(uber_lyft_november_merge_dates$weekday3, 
                                                   levels=c("Monday","Tuesday", "Wednesday", 
                                                            "Thursday", "Friday", "Saturday", "Sunday"))

uber_lyft_november_merge_dates <- uber_lyft_november_merge_dates %>%
        select(-datetime)
uber_lyft_november_merge_dates

uber_lyft_november_merge_dates <- uber_lyft_november_merge_dates %>%
        filter(source != "NA")
unique(uber_lyft_november_merge_dates$source)

ggplot(uber_lyft_november_merge_dates, aes(source, fill=weekday3)) + geom_bar(position="dodge") + 
        scale_y_continuous(labels=comma) + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
        ggtitle("Daily Number of Total Rides Per Station - November") + scale_fill_discrete(name = "Day of the Week") +
        xlab("Station") + ylab("Total Rides")

uber_lyft_november_merge_dates_imp_info<- uber_lyft_november_merge_dates %>%
        dplyr::group_by(Date, weekday1, weekday2, weekday3) %>%
        summarize(total_rides=n())
uber_lyft_november_merge_dates_imp_info


#separating datetime column in month being December
sort(uber_lyft_rides_per_day_december$datetime, decreasing = FALSE)

uber_lyft_rides_per_day_december_altered_date <- uber_lyft_rides_per_day_december 

uber_lyft_rides_per_day_december_altered_date$Date <- as.Date(uber_lyft_rides_per_day_december_altered_date$datetime)
uber_lyft_rides_per_day_december_altered_date$Time <- format(as.POSIXct(uber_lyft_rides_per_day_december_altered_date$datetime), format = "%H:%M:%S")

begin <- as.POSIXct("2018-12-01")
finish <- as.POSIXct("2018-12-18")
dat2 <- data.frame(Date = seq.POSIXt(from = begin, to = finish, by ="DSTday"))

dat2$weekday1 <- as.numeric(format(dat2$Date, format = "%u"))
dat2$weekday2 <- format(dat2$Date, format = "%a")
dat2$weekday3 <- format(dat2$Date, format = "%A")

uber_lyft_december_merge_dates <- uber_lyft_rides_per_day_december_altered_date %>%
        full_join(dat2, by="Date")

uber_lyft_december_merge_dates$weekday3 <- ordered(uber_lyft_december_merge_dates$weekday3, 
                                                   levels=c("Monday","Tuesday", "Wednesday", 
                                                            "Thursday", "Friday", "Saturday", "Sunday"))

uber_lyft_december_merge_dates <- uber_lyft_december_merge_dates %>%
        select(-datetime)
uber_lyft_december_merge_dates

uber_lyft_december_merge_dates <- uber_lyft_december_merge_dates %>%
        filter(source != "NA")
unique(uber_lyft_december_merge_dates$source)

ggplot(uber_lyft_december_merge_dates, aes(source, fill=weekday3)) + geom_bar(position="dodge") + 
        scale_y_continuous(labels=comma) + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) + 
        ggtitle("Daily Number of Total Rides Per Station - December") + xlab("Station") + ylab("Total Rides") +
        scale_fill_discrete(name = "Day of the Week")

uber_lyft_december_merge_dates_imp_info<- uber_lyft_december_merge_dates %>%
        dplyr::group_by(Date, weekday1, weekday2, weekday3) %>%
        summarize(total_rides=n())
uber_lyft_december_merge_dates_imp_info

#total rides by hour and day
my_months_name <- month.name[lyft.uber.remove.na$month] 
my_months_name

lyft.uber.remove.na_months <- lyft.uber.remove.na %>%
        mutate(month_name = my_months_name)
lyft.uber.remove.na_months

uber_lyft_rides_per_day_november <- lyft.uber.remove.na_months %>%
        filter(month_name == "November")

ber_lyft_rides_per_day_december <- lyft.uber.remove.na_months %>%
        filter(month_name == "December")

full_nov_dec_df <- rbind(uber_lyft_november_merge_dates, uber_lyft_december_merge_dates)

day_Hour <- full_nov_dec_df %>%
        filter(hour != "NA") %>%
        group_by(day, weekday3, hour) %>%
        dplyr::summarize(total_rides=n())

ggplot(day_Hour, aes(hour, total_rides, fill=weekday3)) + geom_bar(stat = "identity") + 
        ggtitle("Rides by Hour and Day of the Week") + 
        theme(plot.title = element_text(hjust=0.5)) +
        scale_fill_discrete(name = "Day of the Week") + ylab("Total Rides") + xlab("Hour")


#total rides by day
rides_total_day <- full_nov_dec_df %>%
        group_by(weekday3) %>%
        dplyr::summarize(total_rides = n())

ggplot(rides_total_day, aes(weekday3, total_rides)) + 
        geom_bar(stat = "identity", fill = "purple",color ="yellow") + 
        ggtitle("Total Rides Per Weekday") + theme(legend.position = "none") +
        scale_y_continuous(labels = comma) + xlab("Day of the Week") +
        ylab("Total Rides") + theme(plot.title = element_text(hjust=0.5)) + 
        theme(plot.title = element_text(hjust=0.5))

#total rides by day and month and colored by day of the week 
day_month_ride_total <- full_nov_dec_df %>%
        filter(month_name != "NA") %>%
        group_by(weekday3, month_name) %>%
        dplyr::summarize(total_rides = n())

ggplot(day_month_ride_total, aes(x=reorder(month_name, total_rides), y=total_rides, fill=weekday3)) + 
        geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = comma) + 
        xlab("Month") + ylab("Total Rides") + ggtitle("Total Rides by Day of the Week and Month") +
        scale_fill_discrete(name = "Day of the Week")

#total lyft rides by hour
hour_lyft_data <- lyft.uber.remove.na %>%
        filter(cab_type == "Lyft") %>%
        dplyr::group_by(hour) %>%
        summarize(total_rides = n())
datatable(hour_lyft_data)
hour_lyft_data

ggplot(hour_lyft_data, aes(hour, total_rides)) + 
        geom_bar(stat="identity", fill="blue", color="orange") + 
        theme(legend.position="none") + ggtitle("Lyft Rides Per Hour") + 
        scale_y_continuous(labels=comma) + theme(plot.title = element_text(hjust=0.5)) + 
        xlab("Hour") + ylab("Total Rides")


#total uber rides by hour
hour_uber_data <- lyft.uber.remove.na %>%
        filter(cab_type == "Uber") %>%
        dplyr::group_by(hour) %>%
        summarize(total_rides = n())
datatable(hour_uber_data)
hour_uber_data

ggplot(hour_uber_data, aes(hour, total_rides)) + 
        geom_bar(stat="identity", fill="blue", color="orange") + 
        theme(legend.position="none") + ggtitle("Uber Rides Per Hour") + 
        scale_y_continuous(labels=comma) + theme(plot.title = element_text(hjust=0.5)) +
        xlab("Hour") + ylab("Total Rides")


#rides filtering for total rides/day in month being November
uber_lyft_rides_per_day_november <- lyft.uber.remove.na_months %>%
        group_by(month_name, day) %>%
        summarize(total_rides=n()) %>%
        filter(month_name == "November")

ggplot(uber_lyft_rides_per_day_november, aes(day, total_rides)) + geom_bar(stat = "identity", fill = "purple") + 
        ggtitle("Total Rides Per Day in November 2018") + theme(legend.position = "none") + 
        scale_y_continuous(labels=comma) + xlab("Day") + ylab("Total Rides")

#rides filtering for total rides/day in month being December
uber_lyft_rides_per_day_december <- lyft.uber.remove.na_months %>%
        group_by(month_name, day) %>%
        summarize(total_rides=n()) %>%
        filter(month_name == "December")

ggplot(uber_lyft_rides_per_day_december, aes(day, total_rides)) + geom_bar(stat = "identity", fill = "purple") + 
        ggtitle("Total Rides Per Day in December 2018") + theme(legend.position = "none") + 
        scale_y_continuous(labels=comma) + xlab("Day") + ylab("Total Rides")

#surge exploration COME BACK COME BACK 
lyft.uber.remove.na_surge <- lyft.uber.remove.na %>%
        filter(cab_type == "Lyft") %>%
        dplyr::group_by(hour, surge_multiplier) %>%
        summarize(total_rides = n())

ggplot(lyft.uber.remove.na_surge, aes(hour, total_rides, fill = surge_multiplier)) + geom_bar(stat = "identity") + 
        scale_y_continuous(labels = comma)

#Price vs hour - uber - distance/price to start
lyft.uber.price.vs.hour <- lyft.uber.remove.na %>%
        group_by(price, distance) %>%
        dplyr::select(-apparentTemperatureMaxTime, -apparentTemperatureMax, -apparentTemperatureMinTime, -apparentTemperatureMin,
                      -temperatureMaxTime, -temperatureMax, -temperatureMinTime, -temperatureMin, -uvIndexTime, -precipIntensityMax,
                      -moonPhase, -sunsetTime, -sunriseTime, -ozone, -visibility.1, -uvIndex, -cloudCover, -windBearing, -pressure, 
                      -dewPoint, -icon, -apparentTemperatureLowTime, -apparentTemperatureLow, -apparentTemperatureHighTime, 
                      -apparentTemperatureHigh, -temperatureLowTime, -temperatureLow, -temperatureHighTime, -temperatureHigh, -visibility,
                      -windGustTime, -windGust, -windSpeed, -humidity, -precipProbability, -precipIntensity, -long_summary, -short_summary,
                      -apparentTemperature) %>%
        filter(surge_multiplier <= 1) 

lyft.uber.avg.dist.price <- lyft.uber.price.vs.hour %>%
        group_by(distance, cab_type) %>%
        summarize_at(vars(price), list(name = mean))

ggplot(lyft.uber.avg.dist.price, aes(distance, name, color=cab_type)) + geom_line() + ggtitle("Average Price by Distance for both Lyft and Uber")+
        scale_fill_discrete("Type of Cab") + xlab("Distance") + ylab("Average Price")

lyft.uber.avg.dist.hour <- lyft.uber.price.vs.hour %>%
        group_by(hour) %>%
        summarize_at(vars(distance), list(name = mean))



# Specify data frame
GFG%>%                                        
        
        # Specify group indicator, column, function
        group_by(Category) %>%                        
        summarise_at(vars(Frequency),
                     list(name = mean))



dt[ ,list(mean=mean(col_to_aggregate)), by=col_to_group_by]


#average price per hour no surge - Uber
lyft.uber.price.per.mile <- lyft.uber.remove.na %>%
        filter(cab_type == "Uber") %>%
        mutate(price_dist = price/distance)

ggplot(lyft.uber.price.per.mile, aes(hour, price_dist)) + geom_point()



___________________________________________________________________
uber_lyft_rides_per_day_november <- lyft.uber.remove.na_months %>%
        group_by(month_name, day) %>%
        summarize(total_rides=n()) %>%
        filter(month_name == "November")

ggplot(uber_lyft_rides_per_day_november, aes(day, total_rides)) + geom_bar(stat = "identity", fill = "purple") + 
        ggtitle("Total Rides Per Day in November 2018") + theme(legend.position = "none") + 
        scale_y_continuous(labels=comma) + xlab("Day") + ylab("Total Rides")
