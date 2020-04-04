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
         ObsTime = dmy_hm(paste(Date,Time)), # actual reported time
         roundedTime = floor_date(ObsTime,"15 minutes"), # rounded down to 15 min increments
         isEarlyAM = hour(ObsTime) < 18,
         # create a variable indicating which night we're plotting; use 6pm threshold
         baseDate = if_else(!isEarlyAM, 
                            date(ObsTime),
                            date(ObsTime) - days(1)),
         # variable for actual time appended to single consistent date- to combine all points
         standardisedTime = as_datetime(today()) + 
           hours(hour(ObsTime)) + minutes(minute(ObsTime)) + 
           days(if_else(isEarlyAM,1,0)), 
         # would be better to have country as a separate column - bit of a hack here
         locationToSearch = paste(Location,County,
                                  if_else(str_detect(County,"IE"),"", "UK"),
                                  sep = ", "),
         locationToSearch = str_trim(locationToSearch),
         includeInVis = toupper(`Scoter present`) == "Y",
         invalidDateTime = is.na(ObsTime) | ObsTime < dmy("01-mar-2020") | ObsTime > today() + days(1))

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
  
  allGeo = rbind(allGeo,newGeo)
  
  # cache successful results for future use
  write_csv(allGeo %>% filter(!is.na(lat)),
            "output/geocodedSites.csv")
}

geoCodedScoter = cleanScoter %>% 
  left_join(allGeo) 

# spit out a file with dodgy dates or missing geocode
dataToFix = geoCodedScoter %>% 
  mutate(noGeoCode = is.na(lat)) %>% 
  filter(includeInVis) %>% 
  filter(noGeoCode | invalidDateTime) %>% 
  write_csv("output/dataQualityIssues.csv")



# Theme for plots ---------------------------------------------------------

theme_black <- function (base_size = 16, base_family = ""){
  theme_minimal() %+replace% 
    theme(
      line = element_line(colour = "white", size = 0.5, linetype = 1, 
                          lineend = "butt"), 
      rect = element_rect(fill = "white", 
                          colour = "white", size = 0.5, linetype = 1), 
      text = element_text(family = base_family, 
                          face = "plain", colour = "white", size = base_size,
                          angle = 0, lineheight = 0.9, hjust = 0, vjust = 0),
      plot.background = element_rect(colour = 'black', fill = 'black'),
      plot.title = element_text(size = rel(1.2)),
      # panel.border = element_rect(fill = NA, colour = "white"), 
      # panel.grid.major = element_line(colour = "grey20", size = 0.2), 
      # panel.grid.minor = element_line(colour = "grey5", size = 0.5),
      strip.background = element_rect(fill = "grey30", colour = "grey30"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}



# Generate plots for specified date range ------------------------------------

nights = seq.Date(dmy("31-03-2020"),
                dmy("02-04-2020"),
                by = "day")

for (i in 1:length(nights)) {
  
  nightToPlot = nights[i]
  
  nightStr = as.character(nightToPlot,format = "%d-%b-%y") 
  print(nightStr)
  # nightToPlot = dmy("31-03-2020")
  
  # Generate a static plot --------------------------------------------------
  
  toPlot = geoCodedScoter %>% 
    filter(baseDate == nightToPlot,
           (hour(ObsTime) >=18 | hour(ObsTime) < 3),
           !is.na(lat),
           lat != 0,
           includeInVis)
  
  static = ggplot(toPlot) + 
    geom_polygon(data = uk , aes(x=long, y = lat, group = group),fill = "white") +
    geom_point(aes(x = lon,y = lat,group = obsID),color = "orange",
               size = 4) +
    coord_map(xlim = c(-14,4)) +
    theme_black() + 
    ggtitle("Common Scoter Nocturnal Migration \n")
  
  static
  
  
  # ... and animate it! -----------------------------------------------------
  nHrsInPlot = hour(as.period(max(toPlot$roundedTime) - min(toPlot$roundedTime),"hours"))
  
  anim = animate(static + 
                   transition_components(ObsTime,
                                         # # enter_length = as_datetime(hm("0:5")),
                                         exit_length = as_datetime(hm("0:30"))) + 
                   # enter_fade() +
                   exit_fade() +
                   exit_shrink() +
                   shadow_mark(past = T,future = F,alpha = 0.4,size = size/2) +
                   ggtitle("Common Scoter Nocturnal Migration: {format(frame_time, '%d %B %Y, %H:%M')}"),
                 # nframes = 50,
                 nframes = nHrsInPlot * 60,
                 fps = 8,
                 width = 600,
                 height = 600,
                 bg = "transparent")
  
  # anim
  
  anim_save(str_glue("output/scoter_hiFreq_{nightStr}.gif"))
}


# Combined dates ----------------------------------------------------------

toPlotCombined = geoCodedScoter %>% 
  filter(baseDate >= min(nights),
         baseDate <= max(nights),
         (hour(ObsTime) >=18 | hour(ObsTime) < 3),
         !is.na(lat),
         lat != 0,
         includeInVis) %>% 
  mutate(Night = factor(baseDate))

static = ggplot(toPlotCombined) + 
  geom_polygon(data = uk , aes(x=long, y = lat, group = group),fill = "white") +
  geom_point(aes(x = lon,y = lat,group = obsID,color = Night),
             size = 3) +
  coord_map() +
  theme_black() 

static


nHrsInPlot = hour(as.period(max(toPlotCombined$standardisedTime) - min(toPlotCombined$standardisedTime),"hours"))

anim = animate(static + 
                 transition_components(standardisedTime,
                                       # # enter_length = as_datetime(hm("0:5")),
                                       exit_length = as_datetime(hm("0:30"))) + 
                 # enter_fade() +
                 exit_fade() +
                 exit_shrink() +
                 shadow_mark(past = T,future = F,alpha = 0.3,size = size/2) +
                 ggtitle("Common Scoter Nocturnal Migration: {format(frame_time, '%H:%M')}"),
               # nframes = 50,
               nframes = nHrsInPlot * 60,
               fps = 8,
               width = 600,
               height = 600,
               bg = "transparent")

 # anim

anim_save(str_glue("output/scoter_hiFreq_allDays.gif"))

