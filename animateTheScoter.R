library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(tmaptools)
library(sp)
library(ggmap)
library(hrbrthemes)

remotes::install_github("wilkelab/ggtext")
library(ggtext)

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
         hoursAfter8pm = as.numeric(difftime(standardisedTime,
                                               today() + hours(20),
                                               units = "hours")),
         County = replace_na(County,""),
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
geocodedSiteData <- "output/geocodedSites.csv"

# if we already have geo data, read it and figure out what's missing
if (file.exists(geocodedSiteData)) {
  allGeo = read_csv(geocodedSiteData)
  newSites = sites %>% anti_join(allGeo)
} else {
  # otherwise, we'll just get everything
  newSites = sites
}

if (nrow(newSites) > 0){
  # this will take a minute or two...
  newGeo = newSites %>%
    mutate(geocode = map(locationToSearch,
                         # handle geocoding errors
                         possibly(geocoder,otherwise = list()))) 
  
  newGeo = newGeo %>% 
    unnest_wider(geocode) %>% 
    select(locationToSearch,lat,lon)
  
  if (exists("allGeo")) {
    allGeo = rbind(allGeo,newGeo)
  } else {
    allGeo = newGeo
  }
  # cache successful results for future use
  write_csv(allGeo %>% filter(!is.na(lat)),
            geocodedSiteData)
}

geoCodedScoter = cleanScoter %>% 
  left_join(allGeo) 

# spit out a file with dodgy dates or missing geocode
dataToFix = geoCodedScoter %>% 
  mutate(noGeoCode = is.na(lat)) %>% 
  filter(includeInVis) %>% 
  filter(noGeoCode | invalidDateTime) %>% 
  write_csv("output/dataQualityIssues.csv")

write_csv(geoCodedScoter,"output/cleanData.csv")

# Theme for plots ---------------------------------------------------------

theme_black <- function (base_size = 16, base_family = font_rc ){
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
                  dmy("03-04-2020"),
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
    # theme_ft_rc(grid = F) +
    labs(title = "Common Scoter Nocturnal Migration",
         subtitle = "D. Bradnum, J. Dunning, A. Lees, O.Metcalf",
         caption = str_glue("{format(nightToPlot,'%d %B %Y')}"),
         tag = "01:00") +
    theme(plot.title = element_text(size = rel(1.5),face = "bold"),
          plot.subtitle = element_text(size = rel(0.9),face = "italic"),
          plot.caption = element_text(hjust=0.5, size=rel(1.5)),
          plot.tag = element_text(margin = margin(l = -100,
                                                  b = -40),
                                  size = rel(3),face = "bold"),
          plot.tag.position = "topright")
  
  static
  
  
  # ... and animate it! -----------------------------------------------------
  nHrsInPlot = as.numeric(difftime(as.POSIXct(nightToPlot + days(1) + hours(2)), 
                                   as.POSIXct(nightToPlot + hours(20) + minutes(45)),
                          units = "hours"))
  
  framesOutputDir <- "output/frames"
  
  anim = animate(static + 
                   transition_components(ObsTime,
                                         range = c(as.POSIXct(nightToPlot + hours(20) + minutes(45)),
                                                   as.POSIXct(nightToPlot + days(1) + hours(2))),
                                         # # enter_length = as_datetime(hm("0:5")),
                                         exit_length = as_datetime(hm("0:30"))) + 
                   # enter_fade() +
                   exit_fade() +
                   exit_shrink() +
                   shadow_mark(past = T,future = F,alpha = 0.4,size = size/2) +
                   labs(title = "Common Scoter Nocturnal Migration",
                        subtitle = "D. Bradnum, J. Dunning, A. Lees, O.Metcalf",
                        tag = "{format(frame_time, '%H:%M')}",
                        caption = "{format(frame_time,'%d %B %Y')}"),
                 end_pause = 20,
                 # nframes = 50,
                 nframes = nHrsInPlot * 60,
                 fps = 8,
                 width = 600,
                 height = 600,
                 bg = "transparent",
                 ## UNCOMMENT THESE LINES to generate stills rather than a GIF
                 # device = "png",
                 # renderer = file_renderer(framesOutputDir, prefix = str_glue("{i}_"), overwrite = TRUE)
                 )
  
  # anim 
  
  anim_save(str_glue("output/final_scoter_hiFreq_{nightStr}.gif"))
}


# combine all frames into a single gif ------------------------------------

png_files <- list.files(framesOutputDir,full.names = T)
gif_file <- "output/final_scoter_hiFreq_combined.gif"
# gif_file = tempfile(fileext = ".gif")
gifski(png_files, gif_file,width = 600,height = 600,delay = 1.0/8.0)
utils::browseURL(gif_file)

# all dates superimposed ----------------------------------------------------------
nightLevels = format(nights,"%d %B %Y")

toPlotCombined = geoCodedScoter %>% 
  filter(baseDate >= min(nights),
         baseDate <= max(nights),
         (hour(ObsTime) >=18 | hour(ObsTime) < 3),
         !is.na(lat),
         lat != 0,
         includeInVis) %>% 
  mutate(Night = factor(format(baseDate,"%d %B %Y"),levels = nightLevels))

static = ggplot(toPlotCombined) + 
  geom_polygon(data = uk , aes(x=long, y = lat, group = group),fill = "white") +
  geom_point(aes(x = lon,y = lat,group = obsID,color = Night),
             size = 4) +
  coord_map(xlim = c(-14,4)) +
  theme_black() +
  # theme_ft_rc(grid = F) +
  labs(title = "Common Scoter Nocturnal Migration",
       subtitle = "D. Bradnum, J. Dunning, A. Lees, O.Metcalf",
       # caption = str_glue("{format(nightToPlot,'%d %B %Y')}"),
       tag = "01:00") +
  theme(plot.title = element_text(size = rel(1.5),face = "bold"),
        plot.subtitle = element_text(size = rel(0.9),face = "italic"),
        plot.caption = element_text(hjust=0.5, size=rel(1.5)),
        plot.tag = element_text(margin = margin(l = -100,
                                                b = -40),
                                size = rel(3),face = "bold"),
        plot.tag.position = "topright",
        legend.text = element_text(size = rel(1)),
        legend.title = element_blank(),
        legend.position = "bottom") 

static

nHrsInPlot = as.numeric(difftime(as.POSIXct(today() + days(1) + hours(2)),
                                 as.POSIXct(today() + hours(20) + minutes(45)), 
                                 units = "hours"))

anim = animate(static + 
                 transition_components(standardisedTime,
                                       range = c(as.POSIXct(today() + hours(20) + minutes(45)),
                                                 as.POSIXct(today() + days(1) + hours(2))),
                                       # # enter_length = as_datetime(hm("0:5")),
                                       exit_length = as_datetime(hm("0:30"))) + 
                 # enter_fade() +
                 exit_fade() +
                 exit_shrink() +
                 shadow_mark(past = T,future = F,alpha = 0.4,size = size/2) +
                 labs(title = "Common Scoter Nocturnal Migration",
                      subtitle = "D. Bradnum, J. Dunning, A. Lees, O.Metcalf",
                      tag = "{format(frame_time, '%H:%M')}")
                      # caption = "{format(frame_time,'%d %B %Y')}")
               ,
               end_pause = 20,
               # nframes = 50,
               nframes = nHrsInPlot * 60,
               fps = 8,
               width = 600,
               height = 600,
               bg = "transparent")

# anim 

anim_save(str_glue("output/scoter_hiFreq_allDays.gif"))

