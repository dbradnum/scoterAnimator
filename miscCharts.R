# check number of records per night
recordsPerNight = cleanScoter %>% 
  filter(includeInVis) %>% 
  count(baseDate) %>% 
  arrange(-n)

ggplot(recordsPerNight,aes(baseDate,n)) + 
  geom_col() +
  scale_x_date(date_labels = "%d %b")

# plot distribution over hours
hourFactorLevels = as.character(c(18:23,0:17))
cleanScoter %>% 
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

