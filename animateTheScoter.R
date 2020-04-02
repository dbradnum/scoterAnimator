library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(tmaptools)
library(sp)
library(ggmap)

Sys.setenv(TZ='UTC')


# Get basemap -------------------------------------------------------------

uk = map_data("world",region = c("uk","ireland"))

# TODO: try getting a nicer base map on which to overlay points

# bounds = bbox(uk %>% select(long,lat) %>% as.matrix())

# base_map = get_stamenmap(bounds, maptype = "toner-lite",zoom = 6)
# ggmap(base_map)

# sample = read_csv("input/sampleData.csv") %>% 
#   mutate(date = ymd_hms(paste(today(), time)) 


# Load and clean input data -----------------------------------------------

# TODO: pull data direct with googlesheets package - needs to be "Published to web"?

rawScoter = read_csv("input/Common Scoter passage record sheet_2020 - Sheet1.csv")

cleanScoter = rawScoter %>% 
  mutate(obsID = row_number(),
         ObsTime = dmy_hm(paste(Date,Time_flock_first_encountered)),
         # would be better to have country as a separate column - this is a hack
         locationToSearch = paste(Location,County,
                                  if_else(str_detect(County,"IE"),"", "UK"),
                                  sep = ", "),
         locationToSearch = str_trim(locationToSearch)) %>%
  filter(`Recorded/Heard` != "0")


# Geocoding sites ---------------------------------------------------------

# only geocode each site once, so list the distinct ones
sites = cleanScoter %>% 
  distinct(locationToSearch) 
  
# create a little function that geocodes and waits (to play nicely with API terms)
geocoder = function(location) {
  print(location)
  geo = geocode_OSM(location,as.data.frame = T)
  Sys.sleep(1)
  geo
}

# this will take a minute or two...
geocoded = sites %>% 
  mutate(geocode = map(locationToSearch,
                       # handle geocoding errors
                        possibly(geocoder,otherwise = list()))) 

geocoded = geocoded %>% 
  unnest_wider(geocode) %>% 
  select(locationToSearch,lat,lon)

# cache results for future use
write_csv(geocoded,"output/geocodedSites.csv")

# geocoded = read_csv("output/geocodedSites.csv")


geoCodedScoter = cleanScoter %>% 
  left_join(geocoded) %>% 
  select(obsID,ObsTime,locationToSearch, lon,lat,)


# Filter data for specified date range ------------------------------------

baseDate = dmy("31-03-2020")
dusk = baseDate + hours(18)
dawn = baseDate + days(1) + hours(6)


# Generate a static plot --------------------------------------------------

toPlot = geoCodedScoter %>% 
  filter(ObsTime >= dusk,
         ObsTime <= dawn,
         !is.na(lat))

static = ggplot(toPlot) + 
  geom_polygon(data = uk , aes(x=long, y = lat, group = group),fill = "grey90") +
  geom_point(aes(x = lon,y = lat,group = obsID),color = "red",size = 3) +
  coord_map() +
  theme_void() 

static


# ... and animate it! -----------------------------------------------------


anim = animate(static + 
          transition_components(ObsTime,
                                enter_length = as_datetime(hm("0:5")),
                                exit_length = as_datetime(hm("0:15"))) + 
          enter_fade() +
          exit_fade() +
          ggtitle("Common Scoter nocturnal migration tracker",
                  subtitle = "Time: {format(frame_time, '%d %B %y, %H:%M')}"),
        nframes = 200,
        fps = 5
        
)

anim_save(str_glue("output/scoter_{baseDate}.gif"))

