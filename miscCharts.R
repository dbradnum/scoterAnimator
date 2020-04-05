geoCodedScoter = read_csv("output/cleanData.csv")

theme_set(theme_ft_rc())

# check number of records per night
recordsPerNight = geoCodedScoter %>% 
  filter(includeInVis) %>% 
  count(baseDate) %>% 
  arrange(-n)

ggplot(recordsPerNight,aes(baseDate,n)) + 
  geom_col() +
  scale_x_date(date_labels = "%d %b")

# plot distribution over hours
hourFactorLevels = as.character(c(18:23,0:17))
geoCodedScoter %>% 
  filter(includeInVis,
         baseDate >= dmy("31-mar-2020"),
         baseDate <= today()) %>% 
  count(baseDate,
        hr = factor(hour(ObsTime),
                    levels = hourFactorLevels)) %>% 
  ggplot(aes(hr,n,fill = factor(baseDate))) + 
  geom_col(position="dodge") + 
  ggtitle("Number of Scoter records per hour per day")

ggsave("output/recordsByHourByDay.png")

geoCodedScoter %>% 
  filter(includeInVis,
         baseDate >= dmy("31-mar-2020"),
         baseDate <= today(),
         (hour(standardisedTime) > 18 | hour(standardisedTime) < 6)) %>% 
  ggplot(aes(standardisedTime,fill = factor(baseDate))) + 
  geom_histogram(binwidth = minutes(10)) +
  ggtitle("Number of Scoter records per 10-minute interval")

ggsave("output/recordsByTimeInterval.png")

geoCodedScoter %>% 
  filter(includeInVis,
         baseDate >= dmy("31-mar-2020"),
         baseDate <= today(),
         (hour(standardisedTime) > 18 | hour(standardisedTime) < 6),
         lon > -5) %>% 
  ggplot(aes(lon,hoursAfter8pm)) +
  geom_point(alpha = 0.6,colour = "orange") +
  stat_smooth(method = "lm") +
  ggtitle("Scoter Observation Time vs Location Longitude")

ggsave("output/recordtimeVsLongitude.png")
