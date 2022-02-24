lyft.uber.dataset <- read_csv("/Users/bethanyleach/Downloads/rideshare_kaggle.csv")
lyft.uber.revised <- as_tibble(lyft.uber.dataset)
str(lyft.uber.revised)


lyft.uber.remove.na <- lyft.uber.revised[complete.cases(lyft.uber.revised), ]
View(lyft.uber.remove.na)


library(rgdal)

my_spdf_regions <- readOGR( 
        dsn= "/Users/bethanyleach/Downloads/Boston_Neighborhoods/Boston_Neighborhoods.shp", 
        verbose=FALSE)

library(broom)
library(ggplot2)
library(sf)

utm19nCRS <- st_crs(my_spdf_regions)
class(utm19nCRS)

plot_locations_spdf_lyft_uber <- st_as_sf(lyft.uber.remove.na, coords = c("longitude", "latitude"), crs = utm19nCRS)
st_crs(plot_locations_spdf_lyft_uber)
ggplot() + geom_sf(data=plot_locations_spdf_lyft_uber)+ggtitle("Map of Plot locations")

spdf_regions_cleaned<- tidy(my_spdf_regions)
spdf_regions_cleaned_boston_map <- ggplot() + geom_polygon(data = spdf_regions_cleaned, 
                                                           aes( x = long, y = lat, group = group),
                                                           fill="#69b3a2", color="white") +theme_void() + 
        geom_sf(data=plot_locations_spdf_lyft_uber)

spdf_regions_cleaned_boston_map







lyft.uber.revised <- lyft.uber.revised %>%
        select(-id, -timestamp, -hour, -day, -timezone, -latitude, -longitude, 
               -apparentTemperature, -long_summary, -precipIntensity, -precipProbability,
               -windSpeed, -windGust, -windGustTime, -visibility, -temperatureHigh, 
               -temperatureHighTime, -temperatureLow, -temperatureLowTime, apparentTemperatureHigh,
               -apparentTemperatureHighTime, -apparentTemperatureLow, -apparentTemperatureLowTime, 
               -icon, -dewPoint, -pressure, -windBearing, -cloudCover, -uvIndex, -visibility.1,
               -ozone, -sunriseTime, -sunsetTime, -moonPhase, -precipIntensityMax, -uvIndexTime,
               -temperatureMin, -temperatureMinTime, -temperatureMax, -temperatureMaxTime,
               -apparentTemperatureMin, -apparentTemperatureMinTime, -apparentTemperatureMax, 
               -apparentTemperatureMaxTime)

sort(lyft.uber.revised$datetime, decreasing = FALSE)


lyft.uber.revised <- lyft.uber.revised %>%
        select(-apparentTemperatureHigh)

lyft.uber.revised$Date <- as.Date(lyft.uber.revised$datetime)
lyft.uber.revised$Time <- format(as.POSIXct(lyft.uber.revised$datetime), format = "%H:%M:%S") 

lyft.uber.revised <- lyft.uber.revised %>%
        select(-datetime)

lyft.uber.revised_surge <- lyft.uber.revised %>%
        filter(surge_multiplier >= 2.00) %>%
        select(-apparentTemperatureHigh)


#map experimental data
#mapdata <- map_data("world") %>%
#        filter(region=="USA")
#View(mapdata)

#/Users/bethanyleach/Downloads/Boston_Neighborhoods/Boston_Neighborhoods.shp

boston.neighborhood.shapefile <- "/Users/bethanyleach/Downloads/Boston_Neighborhoods/Boston_Neighborhoods.shp"
boston.neighborhood.geo <- read_shape(file=boston.neighborhood.shapefile, as.sf = TRUE)

neighborhood.boston.shp.exp <- shapefile("/Users/bethanyleach/Downloads/Boston_Neighborhoods/Boston_Neighborhoods.shp")
View(neighborhood.boston.shp.exp)
qtm(neighborhood.boston.shp.exp)
tmap_options(check.and.fix = st_is_valid) 

qtm(neighborhood.boston.shp.exp)


library(rgdal)

my_spdf_regions <- readOGR( 
        dsn= "/Users/bethanyleach/Downloads/Boston_Neighborhoods/Boston_Neighborhoods.shp", 
        verbose=FALSE)

library(broom)
library(ggplot2)

utm19nCRS <- st_crs(my_spdf_regions)
class(utm19nCRS)

plot_locations_spdf_lyft_uber <- st_as_sf(lyft.uber.revised, coords = c("longitude", "latitude"), crs = utm19nCRS)
st_crs(plot_locations_spdf_lyft_uber)
ggplot() + geom_sf(data=plot_locations_spdf_lyft_uber)+ggtitle("Map of Plot locations")

spdf_regions_cleaned<- tidy(my_spdf_regions)
spdf_regions_cleaned_boston_map <- ggplot() + geom_polygon(data = spdf_regions_cleaned, 
                                                           aes( x = long, y = lat, group = group),
                                                           fill="#69b3a2", color="white") +theme_void() + 
                                                           geom_sf(data=plot_locations_spdf_lyft_uber)

spdf_regions_cleaned_boston_map


lyft.uber.remove.na <- lyft.uber.revised[complete.cases(lyft.uber.revised), ]
View(lyft.uber.remove.na)



lyft.uber.reduced.columns <- lyft.uber.revised %>%
        dplyr::select(-id, -timestamp, -hour, -day, -timezone, -apparentTemperature, -long_summary, 
               -precipIntensity, -precipProbability,
               -windSpeed, -windGust, -windGustTime, -visibility, -temperatureHigh, 
               -temperatureHighTime, -temperatureLow, -temperatureLowTime, apparentTemperatureHigh,
               -apparentTemperatureHighTime, -apparentTemperatureLow, -apparentTemperatureLowTime, 
               -icon, -dewPoint, -pressure, -windBearing, -cloudCover, -uvIndex, -visibility.1,
               -ozone, -sunriseTime, -sunsetTime, -moonPhase, -precipIntensityMax, -uvIndexTime,
               -temperatureMin, -temperatureMinTime, -temperatureMax, -temperatureMaxTime,
               -apparentTemperatureMin, -apparentTemperatureMinTime, -apparentTemperatureMax, 
               -apparentTemperatureMaxTime)


lyft.uber.reduced.columns <- lyft.uber.reduced.columns %>%
        dplyr::select(-apparentTemperatureHigh)

lyft.uber.reduced.columns$Date <- as.Date(lyft.uber.reduced.columns$datetime)
lyft.uber.reduced.columns$Time <- format(as.POSIXct(lyft.uber.reduced.columns$datetime), format = "%H:%M:%S") 

lyft.uber.reduced.columns <- lyft.uber.reduced.columns %>%
        dplyr::select(-datetime)

lyft.uber.reduced.columns_surge <- lyft.uber.reduced.columns %>%
        filter(surge_multiplier >= 2.00) %>%
        dplyr::select(-apparentTemperatureHigh)





#unique(lyft.uber.revised$source)
#[1] "Haymarket Square"        "Back Bay"                "North End"               "North Station"          
#[5] "Beacon Hill"             "Boston University"       "Fenway"                  "South Station"          
#[9] "Theatre District"        "West End"                "Financial District"      "Northeastern University"


#lyft.uber.map <- ggplot() + geom_polygon(data = lyft.uber.revised, 
#                                                          aes( x = longitude, y = latitude, group = group), 
#                                                           fill="#69b3a2", color="white") +theme_void()



#ggplot(data = lyft.uber.revised, aes(longitude, latitude)) +
#        +     geom_polygon(data = spdf_regions_cleaned, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#       +     theme_void() + geom_point()
#ggplot(data = lyft.uber.revised, aes(longitude, latitude)) + geom_point()

spdf_regions_cleaned_boston_map <- ggplot(spdf_regions_cleaned, aes(x=long, y=lat, group=group))+
        geom_polygon(color="blue", size=0.1, fill="lightgrey")+coord_equal()+theme_minimal()
spdf_regions_cleaned_boston_map


plot.locations.uber.lyft.revised <- data.frame(lyft.uber.revised$longitude, lyft.uber.revised$latitude)
plot.locations.uber.lyft.revised


prj4string<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
prj<- st_crs(prj4string)

plot_locations<- st_as_sf(plot.locations.uber.lyft.revised, coords = c("WGS84_LON","WGS84_LAT"), crs=prj)
st_crs(plot_locations)



library(sp)
library(rgdal)
#coordinates(lyft.uber.revised) <- ~longitude+latitude

#class(lyft.uber.revised)
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"

