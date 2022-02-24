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
library(corrgram)
library(corrplot)
library(viridis)
library(palmerpenguins)
library(plyr)

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

stat_density_boston_map <- ggmap(boston3) + stat_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude, fill = stat(level)), geom="polygon") +
        ggtitle("Density Map of Cab Rides in Boston Neighborhoods")


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
unique(uber_lyft_november_merge_dates)


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
uber_lyft_rides_per_day_december_altered_date$Time <- format(as.POSIXct(uber_lyft_rides_per_day_december_altered_date$datetime), 
                                                             format = "%H:%M:%S")

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

uber_lyft_rides_per_day_december <- lyft.uber.remove.na_months %>%
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


#daily number of rides per station 
daily_rides_data <- full_nov_dec_df %>%
        group_by(source, hour, weekday3) %>%
        dplyr::summarize(total_rides = n())

ggplot(daily_rides_data, aes(source, total_rides)) + 
        geom_bar(stat = "identity", position = "dodge", aes(fill = weekday3)) + 
        theme(axis.text.x = element_text(angle=90)) + xlab("Source") + ylab("Total Rides") +
        ggtitle("Total Number of Rides pet Day of the Week by Source") +
        scale_fill_discrete(name = "Day of the Week")
        

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

ggplot(lyft.uber.avg.dist.hour, aes(hour, name)) + geom_line() + ggtitle("Average Ride Distance by Hour") + xlab("Hour of the Day") + 
        ylab("Average Distance (Miles)")

#Correlation between Price, Distance, Surge Multiplier (Lyft)
lyft.uber.remove.na_UBER <- lyft.uber.remove.na %>%
        filter(cab_type == "Uber")
mydata_UBER <- lyft.uber.remove.na_UBER[, c(13,14)]
corrplot(cor(mydata_UBER), tl.col = "brown", tl.srt = 30, bg = "White",
         title = "\n\n Uber: Correlation Between Price and Distance",
         type = "full", col=brewer.pal(n=8, name="RdYlBu"))
cor(lyft.uber.remove.na_UBER[, c(13,14)])


lyft.uber.remove.na_LYFT <- lyft.uber.remove.na %>%
        filter(cab_type == "Lyft")
mydata_LYFT <- lyft.uber.remove.na_LYFT[, c(13,14,15)]
corrplot(cor(mydata_LYFT), tl.col = "brown", tl.srt = 30, bg = "White",
         title = "\n\n Lyft: Correlation Between Price, Distance, and Surge Multiplier",
         type = "full")
cor(lyft.uber.remove.na_LYFT[, c(13,14,15)])

#Lyft - Surge Per Hour
lyft.uber.remove.na_surge <- lyft.uber.remove.na %>%
        filter(cab_type == "Lyft", surge_multiplier > 1.00) %>%
        dplyr::group_by(hour, surge_multiplier) %>%
        summarize(total_rides = n())

lyft.uber.remove.na_surge$surge_multiplier <- as.factor(lyft.uber.remove.na_surge$surge_multiplier)

ggplot(lyft.uber.remove.na_surge, aes(hour, total_rides, color = surge_multiplier)) + 
        geom_point(alpha=0.8, size=1, aes(color = surge_multiplier)) + 
        geom_line(aes(color = surge_multiplier)) + ggtitle("Lyft: Total Rides vs Hour of the Day Per Surge Multiplier") +
        facet_wrap(~surge_multiplier, ncol=1, scales="free") + xlab("Hour") + ylab("Total Rides") +
        guides(color=guide_legend(ncol=1)) + theme(legend.position="none",
                                                   panel.border = element_blank(), 
                                                   panel.spacing.x = unit(0,"line"))



#Surge-Day of Week-Total Rides Lyft
day_Hour_lyft_surge <- full_nov_dec_df %>%
        filter(weekday3 != "NA", surge_multiplier > 1.00) %>%
        group_by(day, weekday3, surge_multiplier) %>%
        dplyr::summarize(total_rides=n())

ggplot(day_Hour_lyft_surge, aes(weekday3, total_rides, fill=surge_multiplier)) + geom_bar(stat = "identity", position="dodge") + 
        ggtitle("Relationship Between Total Rides and Day of the Week With Respect to Surge Multiplier") + 
        theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 45)) +
        xlab("Day of the Week") + ylab("Total Rides") +
        scale_fill_gradient(low="pink",high="darkred") +
        facet_wrap(~surge_multiplier, ncol=2, scales="free") + guides(color=guide_legend(ncol=1)) + theme(legend.position="none",
                                                                                  panel.border = element_blank(), 
                                                                                  panel.spacing.x = unit(0,"line"))

ggplot(day_Hour_lyft_surge, aes(weekday3, total_rides, fill=surge_multiplier)) + geom_bar(stat = "bin") + 
        ggtitle("Relationship Between Total Rides and Day of the Week With Respect to Surge Multiplier") + 
        theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 45)) +
        xlab("Day of the Week") + ylab("Total Rides") +
        scale_fill_gradient(low="pink",high="darkred") +
        facet_wrap(~surge_multiplier, ncol=2, scales="free") + guides(color=guide_legend(ncol=1)) + theme(legend.position="none",
                                                                                                          panel.border = element_blank(), 
                                                                                                          panel.spacing.x = unit(0,"line"))
# Surge Day of Week - Lyft - Simplified

ggplot(data = day_Hour_lyft_surge, aes(x = weekday3, y = total_rides, fill = factor(surge_multiplier))) + 
        geom_bar(stat = "identity", position = "dodge") + 
        xlab("Day of the Week") + ylab("Total Rides") + ggtitle("Total Rides vs Day of the Week Per Surge Multiplier") + 
        labs(fill="Surge Multiplier")

#Surge Rides Source - Lyft
source_destination_lyft_surge <- full_nov_dec_df %>%
        filter(source != "NA", destination != "NA", surge_multiplier > 1.00) %>%
        group_by(source, destination, surge_multiplier, weekday3) %>%
        dplyr::summarize(total_rides = n()) %>%
        arrange(desc(total_rides))

source_destination_lyft_surge_combine <- source_destination_lyft_surge %>%
        unite('Route', source:destination, remove = FALSE)

data_source_destination_route <- source_destination_lyft_surge_combine[with(source_destination_lyft_surge_combine,order(-total_rides)),]

data_source_destination_route_2 <- data_source_destination_route
        
total_rides_route <- ddply(data_source_destination_route_2,"Route",numcolwise(sum))
total_rides_route_desc <- total_rides_route %>%
        arrange(desc(total_rides))
data_source_destination_route <- total_rides_route_desc[1:10,]
data_source_destination_route_lowest <- total_rides_route_desc[63:72,]


ggplot(data = data_source_destination_route, aes(forcats::fct_reorder(Route, desc(total_rides)), total_rides)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + ggtitle("Surge: Relationship Between Lyft Rides and Routes - Upper Bound") +
        scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600))

ggplot(data = data_source_destination_route_lowest, aes(forcats::fct_reorder(Route, desc(total_rides)), total_rides)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + ggtitle("Surge: Relationship Between Lyft Rides and Routes - Lower Bound") +
        scale_y_continuous(breaks = c(20, 40, 60, 80))


#avg price by source & neighborhood map
#Both cab types
#Both Surge and no Surge
avg_price_simulator <- full_nov_dec_df
avg_price_simulator <- avg_price_simulator[-c(1:2, 4:6, 10:11, 15:50)]
avg_price_simulator_reduced <- avg_price_simulator[-c(8:18)]

avg_price_simulator_reduced_2 <- avg_price_simulator_reduced 

source_destination_lyft_surge_2 <- avg_price_simulator_reduced_2 %>%
        unite('Route', source:destination, remove = FALSE)

source_destination_lyft_surge_2_adj<- source_destination_lyft_surge_2 %>%
        group_by(Route) %>%
        dplyr::summarize(total_rides = n(), mean_price = mean(price))
source_destination_lyft_surge_2_adj


price_rides_route <- source_destination_lyft_surge_2_adj %>%
        arrange(desc(mean_price))
price_rides_route_desc <- price_rides_route[1:5,]
price_rides_route_asc <- price_rides_route[68:72,]
price_rides_route_top_lowest <- rbind(price_rides_route_desc, price_rides_route_asc)

ggplot(data = price_rides_route_desc, aes(forcats::fct_reorder(Route, desc(mean_price)), mean_price)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + ggtitle("Mean Ride Price vs Cab Route") + 
        geom_text(aes(label=round(mean_price, 1)))

ggplot(data = price_rides_route_top_lowest, aes(forcats::fct_reorder(Route, desc(mean_price)), mean_price)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + ggtitle("Five Priciest and Five Least Priciest Ride Price vs Cab Route") + 
        geom_text(aes(label=round(mean_price, 1)))

#hour of day shortest ride and longest ride
#previous graph shows most expensive avg ride being financial district to boston university
#and least expensive being #All Cab Types
source_destination_lyft_surge_2_adj_hour_shortest_ride <- source_destination_lyft_surge_2 %>%
        filter(Route=="Haymarket Square_North Station") %>%
        group_by(hour) %>%
        dplyr::summarize(total_rides = n())

source_destination_lyft_surge_2_adj_hour_longest_ride <- source_destination_lyft_surge_2 %>%
        filter(Route=="Financial District_Boston University") %>%
        group_by(hour) %>%
        dplyr::summarize(total_rides = n())

longest_shortest_combined <- rbind(source_destination_lyft_surge_2_adj_hour_shortest_ride,
                                   source_destination_lyft_surge_2_adj_hour_longest_ride)

ggplot(data = source_destination_lyft_surge_2_adj_hour_shortest_ride, aes(hour, total_rides, color="green")) +
        geom_bar(stat = "identity", position = "dodge", fill="purple") + xlab("Hour") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + 
        ggtitle("Haymarket Sqare to North Station - Number of Rides Per Hour")

ggplot(data = source_destination_lyft_surge_2_adj_hour_longest_ride, aes(hour, total_rides, color="green")) +
        geom_bar(stat = "identity", position = "dodge", fill="purple") + xlab("Hour") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + 
        ggtitle("Financial District to Boston University - Number of Rides Per Hour") +
        theme(legend.position="none")

#Uber Avg Price vs Route Info
avg_price_simulator_reduced_3 <- avg_price_simulator_reduced
source_destination_lyft_surge_3 <- avg_price_simulator_reduced_3 %>%
        unite('Route', source:destination, remove = FALSE)

source_destination_lyft_surge_3_adj<- source_destination_lyft_surge_3 %>%
        filter(cab_type == "Uber") %>%
        group_by(Route) %>%
        dplyr::summarize(total_rides = n(), mean_price = mean(price))
source_destination_lyft_surge_3_adj

price_rides_route_SURGE <- source_destination_lyft_surge_3_adj %>%
        arrange(desc(mean_price))

price_rides_route_desc_SURGE <- price_rides_route_SURGE[1:5,]
price_rides_route_asc_SURGE <- price_rides_route_SURGE[68:72,]
price_rides_route_top_lowest_SURGE <- rbind(price_rides_route_desc_SURGE, price_rides_route_asc_SURGE)


ggplot(data = price_rides_route_top_lowest_SURGE, aes(forcats::fct_reorder(Route, desc(mean_price)), mean_price)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + 
        ggtitle("Uber: Five Priciest and Five Least Priciest Ride Price vs Cab Route") + 
        geom_text(aes(label=round(mean_price, 1)))

#Lyft Avg Price vs Route (no surge)
avg_price_simulator_reduced_4 <- avg_price_simulator_reduced
source_destination_lyft_surge_4 <- avg_price_simulator_reduced_4 %>%
        unite('Route', source:destination, remove = FALSE)

source_destination_lyft_surge_4_adj<- source_destination_lyft_surge_4 %>%
        filter(cab_type == "Lyft", surge_multiplier == 1) %>%
        group_by(Route) %>%
        dplyr::summarize(total_rides = n(), mean_price = mean(price))
source_destination_lyft_surge_4_adj

lyft_no_surge_price_rides_route <- source_destination_lyft_surge_4_adj %>%
        arrange(desc(mean_price))

lyft_no_surge_price_rides_route_asc <- lyft_no_surge_price_rides_route[1:5,]
lyft_no_surge_price_rides_route_desc <- lyft_no_surge_price_rides_route[68:72,]
lyft_no_surge_price_rides_route_top_lowest <- rbind(lyft_no_surge_price_rides_route_asc, lyft_no_surge_price_rides_route_desc)


ggplot(data = lyft_no_surge_price_rides_route_top_lowest, aes(forcats::fct_reorder(Route, desc(mean_price)), mean_price)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + 
        ggtitle("Lyft (No Surge): Five Priciest and Five Least Priciest Ride Price vs Cab Route") + 
        geom_text(aes(label=round(mean_price, 1)))

#Lyft Avg Price vs Route (SURGE = 1.25-1.75)
avg_price_simulator_reduced_1.25.75 <- avg_price_simulator_reduced
source_destination_lyft_surge_1.25.75 <- avg_price_simulator_reduced_1.25.75 %>%
        unite('Route', source:destination, remove = FALSE)

source_destination_lyft_surge_1.25.75_adj<- source_destination_lyft_surge_1.25.75 %>%
        filter(cab_type == "Lyft", surge_multiplier >= 1.25, surge_multiplier <= 1.75) %>%
        group_by(Route) %>%
        dplyr::summarize(total_rides = n(), mean_price = mean(price))
source_destination_lyft_surge_1.25.75_adj

lyft_1.25.75_surge_price_rides_route <- source_destination_lyft_surge_1.25.75_adj %>%
        arrange(desc(mean_price))

lyft_1.25.75_surge_price_rides_route_asc <- lyft_1.25.75_surge_price_rides_route[1:5,]
lyft_1.25.75_surge_price_rides_route_desc <- lyft_1.25.75_surge_price_rides_route[68:72,]
lyft_1.25.75_surge_price_rides_route_top_lowest <- rbind(lyft_1.25.75_surge_price_rides_route_asc, lyft_1.25.75_surge_price_rides_route_desc)


ggplot(data = lyft_1.25.75_surge_price_rides_route_top_lowest, aes(forcats::fct_reorder(Route, desc(mean_price)), mean_price)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + 
        ggtitle("1.25-1.75 Surge Data: Five Priciest and Five Least Priciest Ride Price vs Cab Route") + 
        geom_text(aes(label=round(mean_price, 1)))

#Lyft Avg Price vs Route (SURGE = 2.00 - 3.00)
avg_price_simulator_reduced_2.3 <- avg_price_simulator_reduced
source_destination_lyft_surge_2.3 <- avg_price_simulator_reduced_2.3 %>%
        unite('Route', source:destination, remove = FALSE)

source_destination_lyft_surge_2.3_adj<- source_destination_lyft_surge_2.3 %>%
        filter(cab_type == "Lyft", surge_multiplier >= 2.00, surge_multiplier <= 3.00) %>%
        group_by(Route) %>%
        dplyr::summarize(total_rides = n(), mean_price = mean(price))
source_destination_lyft_surge_2.3_adj

lyft_2.3_surge_price_rides_route <- source_destination_lyft_surge_2.3_adj %>%
        arrange(desc(mean_price))

lyft_2.3_surge_price_rides_route_asc <- lyft_2.3_surge_price_rides_route[1:5,]
lyft_2.3_surge_price_rides_route_desc <- lyft_2.3_surge_price_rides_route[50:54,]
lyft_2.3_surge_price_rides_route_top_lowest <- rbind(lyft_2.3_surge_price_rides_route_asc, lyft_2.3_surge_price_rides_route_desc)


ggplot(data = lyft_2.3_surge_price_rides_route_top_lowest, aes(forcats::fct_reorder(Route, desc(mean_price)), mean_price)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill=Route)) + xlab("Route") + ylab("Total Rides") +
        theme(axis.text.x = element_text(angle=90)) + 
        ggtitle("2.00-3.00 Surge: Five Priciest and Five Least Priciest Ride Price vs Cab Route") + 
        geom_text(aes(label=round(mean_price, 1)))

#Surge Experiment - Lyft (Days) - November
uber_lyft_rides_per_day_november_LYFT <- lyft.uber.remove.na_months

uber_lyft_rides_per_day_november_LYFT <- lyft.uber.remove.na_months %>%
        filter(cab_type=="Lyft", month_name=="November", surge_multiplier > 1.00)

uber_lyft_rides_per_day_november_LYFT <- uber_lyft_rides_per_day_november_LYFT %>%
        group_by(surge_multiplier, day)%>%
        summarize(total_rides=n())

ggplot(uber_lyft_rides_per_day_november_LYFT, aes(x=day, y=total_rides, fill=surge_multiplier))+ geom_bar(stat="identity", position="dodge") + 
        scale_fill_gradient(low="orange",high="darkblue") + 
        ggtitle("Total Rides Per Day (November) With Respect to Surge Multiplier") + 
        xlab("Day (November)") + ylab("Total Rides") + facet_wrap(~surge_multiplier)

#Surge Experiment - Lyft (Days) - December
uber_lyft_rides_per_day_december_LYFT <- lyft.uber.remove.na_months

uber_lyft_rides_per_day_december_LYFT <- lyft.uber.remove.na_months %>%
        filter(cab_type=="Lyft", month_name=="December", surge_multiplier > 1.25)

uber_lyft_rides_per_day_december_LYFT <- uber_lyft_rides_per_day_december_LYFT %>%
        group_by(surge_multiplier, day)%>%
        summarize(total_rides=n())

ggplot(uber_lyft_rides_per_day_december_LYFT, aes(x=day, y=total_rides, fill=surge_multiplier))+ geom_bar(stat="identity", position="dodge") + 
        scale_fill_gradient(low="orange",high="darkblue") +
        ggtitle("Total Rides Per Day (December) With Respect to Surge Multiplier") + xlab("Day (December)") + ylab("Total Rides")
