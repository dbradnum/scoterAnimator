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
# TODO: data cleaning:
#         quite a few typos in site names; 
#         some gaps in cols etc; 
#         dodgy date format

rawScoter = read_csv("input/Common Scoter passage record sheet_2020 - Sheet1.csv")

cleanScoter = rawScoter %>% 
  mutate(obsID = row_number(),
         ObsTime = dmy_hm(paste(Date,Time)),
         # create a variable indicating which night we're plotting; use 6pm threshold
         baseDate = if_else(hour(ObsTime) >= 18, 
                            date(ObsTime),
                            date(ObsTime) - days(1)),
         # would be better to have country as a separate column - this is a hack
         locationToSearch = paste(Location,County,
                                  if_else(str_detect(County,"IE"),"", "UK"),
                                  sep = ", "),
         locationToSearch = str_trim(locationToSearch)) %>%
  mutate(includeInVis = toupper(`Scoter present`) == "Y")

# check number of records per night
cleanScoter %>% count(baseDate) %>% arrange(-n)

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

# aim to top up existing list of geocoded sites
allGeo = read_csv("output/geocodedSites.csv")

newSites = sites %>% anti_join(allGeo)

if (nrow(newSites) > 0){
  # this will take a minute or two...
  newGeo = newSites %>% 
    mutate(geocode = map(locationToSearch,
                         # handle geocoding errors
                         possibly(geocoder,otherwise = list()))) 
  
  newGeo = newGeo %>% 
    unnest_wider(geocode) %>% 
    select(locationToSearch,lat,lon)
  
  allGeo = rbind(existingGeoCodes,newGeo)
  
  
  # cache results for future use
  write_csv(allGeo,"output/geocodedSites.csv")
  }

geoCodedScoter = cleanScoter %>% 
  left_join(allGeo) %>% 
  select(obsID,baseDate,ObsTime, locationToSearch, lon,lat,includeInVis)


# Filter data for specified date range ------------------------------------

nightToPlot = dmy("02-04-2020")

# Generate a static plot --------------------------------------------------

toPlot = geoCodedScoter %>% 
  filter(baseDate == nightToPlot,
         (hour(ObsTime) >=18 || hour(ObsTime) < 7),
         !is.na(lat),
         lat != 0,
         includeInVis)

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
               fps = 5)

anim

anim_save(str_glue("output/scoter_{baseDate}.gif"))

