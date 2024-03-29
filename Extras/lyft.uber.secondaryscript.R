lyft.uber.dataset <- read_csv("/Users/bethanyleach/Downloads/rideshare_kaggle.csv")
lyft.uber.revised <- as_tibble(lyft.uber.dataset)
str(lyft.uber.revised)

install.packages("calendR")

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

lyft.uber.remove.na <- lyft.uber.revised[complete.cases(lyft.uber.revised), ]
View(lyft.uber.remove.na)

register_google("AIzaSyBZx2Za3bJfe2OY0QxgPgef-4GX9jd61Pg")


#my_spdf_regions <- readOGR( 
#        dsn= "/Users/bethanyleach/Downloads/Boston_Neighborhoods/Boston_Neighborhoods.shp", 
#        verbose=FALSE)


#lyft.uber.dataset <- read_csv("/Users/bethanyleach/Downloads/rideshare_kaggle.csv")
#lyft.uber.revised <- as_tibble(lyft.uber.dataset)
#str(lyft.uber.revised)

#b+w boston map with sources 
BostonMap <- qmap("boston", zoom = 12, color = "bw", legend = "topright")
combined_map_boston <- BostonMap + geom_point(aes(x=longitude, y=latitude), data=lyft.uber.remove.na)

#coloredBostonmap
boston2 <- get_map("boston", zoom=12)
ggmap(boston2)

#further detail
extent <- bb2bbox(attr(boston2, "bb"))
ggmap(get_stamenmap(extent, zoom=12))

#ggmap(get_stamenmap(extent, zoom = 12, maptype = "toner-background"))

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

#get_googlemap(
#        center = boston_center, 
#        zoom = 8
#        ) %>%
#          ggmap()
#using stamen map to get map background
#density maps
boston3 <- boston_stamen
ggmap(boston3)
ggmap(boston3) + geom_point(data=lyft.uber.remove.na, aes(x=longitude, y=latitude), size=0.05, alpha =0.05)
ggmap(boston3) + geom_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude))
ggmap(boston3) + stat_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude, fill = stat(level)), geom="polygon")


ggmap(boston3) + stat_density_2d(data=lyft.uber.remove.na, aes(x=longitude, y=latitude, fill = stat(level)), alpha = 0.05, bins = 50, geom="polygon") + 
                                         scale_fill_gradientn(colors = brewer.pal(7, "Greens"))
#[1] "Haymarket Square"        "Back Bay"                "North End"               "North Station"          
#[5] "Beacon Hill"             "Boston University"       "Fenway"                  "South Station"          
#[9] "Theatre District"        "West End"                "Financial District"      "Northeastern University"



##COME BACK TO THIS
ggmap(boston3) + stat_density_2d(data = lyft.uber.remove.na %>% 
                                         filter(`source` %in% c(
                                                 "Lyft", "Uber"
                                                 )), aes(x=longitude, y=latitude, fill = stat(level)), alpha = 0.05, bins =50, geom ="polygon") +
        scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd")) +
        facet_wrap(facets = vars(`cab_type`))


unique(lyft.uber.remove.na$name)
#[1] "Shared"       "Lux"          "Lyft"         "Lux Black XL" "Lyft XL"      "Lux Black"    "UberXL"      
#[8] "Black"        "UberX"        "WAV"          "Black SUV"    "UberPool"    

lyft_car_type_test <- lyft.uber.remove.na %>%
        filter(cab_type=="Lyft")
lyft_car_type_test
unique(lyft_car_type_test$name)
#[1] "Shared"       "Lux"          "Lyft"         "Lux Black XL" "Lyft XL"      "Lux Black"  

ggmap(boston3) + stat_density_2d(data = lyft_car_type_test %>% 
                                         filter(`name` %in% c(
                                                 "Shared", "Lux", "Lyft", "Lux Black XL", "Lyft XL", "Lux Black"
                                         )), aes(x=longitude, y=latitude, fill = stat(level)), alpha = 0.05, bins =50, geom ="polygon") +
        scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd")) +
        facet_wrap(facets = vars(`name`))

avg_price_simulator <- lyft.uber.remove.na %>%
        group_by(source) %>%
        mutate(average_price=mean(price))

avg_price_simulator
avg_price_simulator$average_price
View(avg_price_simulator[c(8, 58)])

unique(avg_price_simulator$average_price)
neighborhood_avg_price <- avg_price_simulator[c(8, 58)]
neighborhood_avg_price
unique(neighborhood_avg_price)
neighborhood_avg_price_final <- unique(neighborhood_avg_price)

#total lyft rides by hour
hour_lyft_data <- lyft.uber.remove.na %>%
        filter(cab_type == "Lyft") %>%
        dplyr::group_by(hour) %>%
        summarize(total_rides = n())
datatable(hour_lyft_data)
hour_lyft_data

ggplot(hour_lyft_data, aes(hour, total_rides)) + geom_bar(stat="identity", fill="blue",
                                                          color="orange") + theme(legend.position="none")+
                                                          ggtitle("Lyft Rides Per Hour")+scale_y_continuous(labels=comma)
#COME BACK TO THIS
my_months_name <- month.name[lyft.uber.remove.na$month] 
my_months_name

lyft.uber.remove.na_months <- lyft.uber.remove.na %>%
        mutate(month_name = my_months_name)
lyft.uber.remove.na_months

month_hour_uber_lyft_data <- lyft.uber.remove.na_months %>%
        group_by(month_name, hour) %>%
        summarize(total_rides = n())

ggplot(hour_uber_lyft_data, aes(x=hour, y=total_rides)) + 
        geom_bar(stat="identity", fill="purple", color="yellow") + 
        theme(legend.position = "none") + ggtitle("Uber and Lyft Rides Per Hour") +
        scale_y_continuous(labels = comma)

ggplot(month_hour_uber_lyft_data, aes(hour, total_rides, fill = month_name)) + 
        geom_bar(stat = "identity") + ggtitle("Total Rides by Hour and Month") +
        scale_y_continuous(labels = comma)

#uber_lyft_rides_per_day_november <- lyft.uber.remove.na_months %>%
#        filter(month_name == "November") %>%
#        dplyr::group_by(day) %>%
#        summarize(total_rides = n())

uber_lyft_rides_per_day_december <- lyft.uber.remove.na_months %>%
        filter(month_name == "December") %>%
        dplyr::group_by(day) %>%
        summarize(total_rides = n())

ggplot(uber_lyft_rides_per_day_november, aes(day, total_rides)) + geom_bar(stat = "identity", fill = "purple") + 
        ggtitle("Total Rides Per Day") + theme(legend.position = "none") + scale_y_continuous(labels=comma)

ggplot(uber_lyft_rides_per_day_december, aes(day, total_rides)) + geom_bar(stat = "identity", fill = "purple") + 
        ggtitle("Total Rides Per Day") + theme(legend.position = "none") + scale_y_continuous(labels=comma)

uber_lyft_rides_per_day_november %>%
        filter(month=="November")

uber_lyft_rides_per_day_november$Date <- as.Date(uber_lyft_rides_per_day_november$datetime)
uber_lyft_rides_per_day_november$Time <- format(as.POSIXct(uber_lyft_rides_per_day_november$datetime), 
                                                format = "%H:%M:%S") 
                                                
unique(uber_lyft_rides_per_day_november$day)

begin_nov <- as.POSIXct("2018-11-26")
finish_nov <- as.POSIXct("2018-11-30")

dat3 <- november(Date = seq.POSIXt(from = begin_nov, to = finish_nov, by ="DSTday"))

dat3$weekday1 <- as.numeric(format(dat3$Date, format = "%u"))
dat3$weekday2 <- format(dat3$Date, format = "%a")
dat3$weekday3 <- format(dat3$Date, format = "%A")

uber_lyft_november_merge_dates <- uber_lyft_rides_per_day_november %>%
        full_join(dat3, by="Date")

ggplot(uber_lyft_november_merge_dates, aes(source, fill=weekday3)) + geom_bar(position="dodge") + 
        scale_y_continuous(labels=comma) + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

days_of_week_november <- lyft.uber.remove.na_months %>%
        filter(month_name =="November")

days_of_week_november  <- november(day=26:30)
days_of_week_november$day <- recode(days_of_week_november$day,
                                    "26"="Monday",
                                    "27"="Tuesday",
                                    "28"="Wednesday",
                                    "29"="Thursday",
                                    "30"="Friday")

days_of_week_november$day <- recode(uber_lyft_rides_per_day_november$day, 
          "26"="Monday",
          "27"="Tuesday",
          "28"="Wednesday",
          "29"="Thursday",
          "30"="Friday")
uber_lyft_rides_per_day_november %>%
        mutate(name_day = days_of_week_november)
-----------------------------------------------------
uber_lyft_rides_per_day_december <- lyft.uber.remove.na_months %>%
        filter(month_name == "December")

sort(uber_lyft_rides_per_day_december$datetime, decreasing = FALSE)

uber_lyft_rides_per_day_december_altered_date <- uber_lyft_rides_per_day_december 

uber_lyft_rides_per_day_december_altered_date$Date <- as.Date(uber_lyft_rides_per_day_december_altered_date$datetime)
uber_lyft_rides_per_day_december_altered_date$Time <- format(as.POSIXct(uber_lyft_rides_per_day_december_altered_date$datetime), format = "%H:%M:%S") 
#uber_lyft_rides_per_day_december_altered_date <- uber_lyft_rides_per_day_december_altered_date
#      select(-datetime)
begin <- as.POSIXct("2018-12-01")
finish <- as.POSIXct("2018-12-18")
dat2 <- november(Date = seq.POSIXt(from = begin, to = finish, by ="DSTday"))

dat2$weekday1 <- as.numeric(format(dat2$Date, format = "%u"))
dat2$weekday2 <- format(dat2$Date, format = "%a")
dat2$weekday3 <- format(dat2$Date, format = "%A")

uber_lyft_december_merge_dates <- uber_lyft_rides_per_day_december_altered_date %>%
        full_join(dat2, by="Date")

uber_lyft_december_merge_dates <- uber_lyft_december_merge_dates %>%
        select(-datetime)
uber_lyft_december_merge_dates

uber_lyft_december_merge_dates <- uber_lyft_december_merge_dates %>%
        filter(source != "NA")
unique(uber_lyft_december_merge_dates$source)

#number of rides per day of week from each source
ggplot(uber_lyft_december_merge_dates, aes(source, fill=weekday3)) + geom_bar(position="dodge") + 
        scale_y_continuous(labels=comma) + theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

uber_lyft_december_merge_dates <- uber_lyft_december_merge_dates[uber_lyft_december_merge_dates$source != "NA"]

day_hour_uber_lyft <- lyft.uber.remove.na_months %>%
        group_by(day, hour) %>%
        dplyr::summarize(Total_rides_heat_map = n())
day_hour_uber_lyft

ggplot(day_hour_uber_lyft, aes(weekday3, hour, fill=Total_rides_heat_map)) + geom_tile(color='white')
#uber_lyft_december_merge_dates <- uber_lyft_december_merge_dates %>%
#        dplyr::group_by(Date, weekday1, weekday2, weekday3) %>%
#        summarize(total_rides=n())
#uber_lyft_december_merge_dates


uber_lyft_december_merge_dates$weekday3 <- ordered(uber_lyft_december_merge_dates$weekday3, 
                                                   levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))



uber_lyft_december_merge_dates_imp_info<- uber_lyft_december_merge_dates %>%
        dplyr::group_by(Date, weekday1, weekday2, weekday3) %>%
        summarize(total_rides=n())
uber_lyft_december_merge_dates_imp_info

ggplot(uber_lyft_december_merge_dates_imp_info, aes(weekday3, total_rides, fill=weekday3)) + 
        geom_bar(stat="identity")+theme(legend.position="none")+scale_y_continuous(labels=comma)


#days_of_week_december <- november(day=1:18)
#dec_weekdays <- factor(sample(c("Saturday", "Sunday", "Monday", "Tuesday","Wednesday", "Thursday",
#                  "Friday"), 18, TRUE))

#dec_weekdays_factor <- factor(dec_weekdays, levels = c("Saturday", "Sunday", "Monday", "Tuesday",
#                                                       "Wednesday", "Thursday","Friday"), ordered=TRUE)

#dec_weekdays_do <- november(dec_weekdays, do)
#days_of_week_december$day_name <- factor(days_of_week_december$day_name, levels=1:18,
#                                         labels=c("Saturday", "Sunday", "Monday",
#                                                  "Tuesday","Wednesday", "Thursday",
#                                                  "Friday"))

#days_of_week_december$day <- recode(uber_lyft_rides_per_day_december$day,
#           "1"="Saturday",
#           "2"="Sunday",
#           "3"="Monday",
#           "4"="Tuesday",
#           "5"="Wednesday",
#           "6"="Thursday",
#           "7"="Friday")

uber_lyft_rides_per_day_november <- lyft.uber.remove.na_months %>%
        filter(month_name == "November") %>%
        mutate(name_day = days_of_week_november) %>%
        dplyr::group_by(day) %>%
        summarize(total_rides = n())


uber_lyft_rides_per_day_november_DAY <- lyft.uber.remove.na_months %>%
        filter(month_name == "November") %>%
        dplyr::group_by(day) %>%
        summarize(total_rides = n())

---------------------------------------------------------
neighborhood_avg_price_final <- unique(neighborhood_avg_price)
lyft.uber.remove.na_source <- lyft.uber.remove.na %>%
        filter(source=="Boston University")
selected_dist_columns <- lyft.uber.remove.na_source[c(8, 9, 14)]
View(selected_dist_columns)

downtown_boston <- subset(lyft.uber.remove.na, -71.1054 <= longitude & longitude <= -71.033 & 42.2148 <= latitude & latitude <= 42.3661)
qmplot(longitude, latitude, data=downtown_boston, maptype="toner-background", color=I("red"))



plot_locations_spdf_lyft_uber <- st_as_sf(lyft.uber.remove.na, coords = c("longitude", "latitude"), crs = utm19nCRS)
st_crs(plot_locations_spdf_lyft_uber)
ggplot() + geom_sf(data=plot_locations_spdf_lyft_uber)+ggtitle("Map of Plot locations")

spdf_regions_cleaned<- tidy(my_spdf_regions)
spdf_regions_cleaned_boston_map <- ggplot() + geom_polygon(data = spdf_regions_cleaned, 
                                                           aes( x = long, y = lat, group = group),
                                                           fill="#69b3a2", color="white") +theme_void() + 
        geom_sf(data=plot_locations_spdf_lyft_uber)

spdf_regions_cleaned_boston_map


spdf_google_boston_uber <- ggmap(get_googlemap(center = c(lon = -71.1, lat = 42.4), zoom = 100, scale = 2,
                                               maptype='terrain', color = 'color')) + 
                                               stat_density2d(aes(x=longitude, y=latitude, fill=..level..),
                                                              alpha=0.1, bins = 10, data = lyft.uber.remove.na, geom="polygon")

spdf_regions_cleaned_boston_map_2 <- ggplot() + stat_density2d(data = plot_locations_spdf_lyft_uber,
                                                             mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
                                                                           y = purrr::map_dbl(geometry, ~.[2]),
                                                                           fill = stat(density)),
                                                             geom = 'tile', 
                                                             contour = FALSE, 
                                                             alpha = 0.8) + geom_sf(data = spdf_regions_cleaned, 
                                                                                    fill = NA, geometry=geometry) + 
                                                                            geom_sf(data = plot_locations_spdf_lyft_uber, 
                                                                                    color = 'red', geometry=geometry) + scale_fill_viridis_c(option='magma', direction = -1) + theme_test()
                                                                  
        
        geom_polygon(data = spdf_regions_cleaned, 
                                                           aes( x = long, y = lat, group = group),
                                                           fill="#69b3a2", color="white") +theme_void() + 
        stat_density2d(data=plot_locations_spdf_lyft_uber, geom="polygon")
        
        
        
        #boston.map <- get_map("Boston",zoom=13)
        #ggmap(boston.map)
        
        
HEAT MAP
ggplot(day_Hour,aes(day, hour, fill = Total)) +
        geom_tile(color="White") +
        ggtitle("Heat Map By Hour/Day")





#Lyft.Uber.TotalRides vs Temperature + Wind Gust
temp_rides_data <- lyft.uber.remove.na %>%
        dplyr::group_by(temperature, cab_type) %>%
        dplyr::summarize(total_rides = n())

ggplot(temp_rides_data, aes(temperature, total_rides, color=cab_type)) + 
        geom_line() +
        theme(legend.position="right") + ggtitle("Total Rides vs Temperature (F)") + 
        ylab("Total Rides") + 
        theme(plot.title = element_text(hjust=0.5)) + xlim(30,50) + ylim(0,2000)

wind_gust_rides_data <- lyft.uber.remove.na %>%
        dplyr::group_by(windGust, temperature, cab_type) %>%
        dplyr::summarize(total_rides = n())

ggplot(wind_gust_rides_data, aes(temperature, total_rides, color=windGust)) + 
        geom_line() + geom_point() +
        theme(legend.position="right") + ggtitle("Total Rides vs Temperature (F) Colored by Wind Gust (mph") + 
        ylab("Total Rides") + xlab("Temperature") +
        theme(plot.title = element_text(hjust=0.5)) + xlim(30,50) + ylim(0,2000)

cor(wind_gust_rides_data$windGust, wind_gust_rides_data$total_rides)