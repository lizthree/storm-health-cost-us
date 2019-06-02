library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
# file <- file.path("repdata_data_StormData.zip")
# 
# if (!file.exists(file)){
#   url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#   download.file(url, file, method = "curl")
# }
# 
# # unzip file into data folder
# if (file.exists(file)){
#   unzip(file, "repdata_data_StormData.txt")
# }

data_raw <- fread("repdata_data_StormData.csv", select = c("BGN_DATE", "STATE", "EVTYPE","FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP"))

data <- na.omit(data_raw)


health <- data.frame(as.Date(data$BGN_DATE, "%m/%d/%Y"), as.character(data$STATE), tolower(data$EVTYPE), as.numeric(data$FATALITIES), as.numeric(data$INJURIES))
colnames(health) <- c("date", "state", "event", "fatalities", "injuries")
health <- separate(health,date,into=c("year","month","day"),sep="-", drop=TRUE)
# > range(health$year)
# [1] "1950" "2011"


#rm(data_raw)
# > dim(health)
# [1] 902297      5

# unique(health$event[grepl("hail", health$event)])
# combine like-data, found that certain data should be considered together
# hurricane's don't compare to other high-fatalities events
# different variations of heat at the top
# avoided rain or hail
health$event <- as.character(health$event)
health$event[grepl("hurricane|tropical storm",health$event)] <- "hurricane\tropical storm"
health$event[grepl("heat|warm",health$event)] <- "heat/unseasonaly warm"
health$event[grepl("ice|freezing|freeze|snow|cold",health$event)] <- "freezing cold/snow"
health$event[grepl("winter|storm|blizzard",health$event)] <- "winter storm/blizzard"
health$event[grepl("storm surge",health$event)] <- "storm surge"
health$event[grepl("flood|flooding",health$event)] <- "flooding/flash flooding"
health$event[grepl("current",health$event)] <- "rip current"
health$event[grepl("tstm|thunderstorm wind",health$event)] <- "thunderstorm wind"
health$event[grepl("fire",health$event)] <- "fire/wildfire"
health$event[grepl("hail",health$event)] <- "hail"
#health$event <- factor(health$event)

health_fatalities <- health %>%
  select(event, year, fatalities) %>%
  filter(year > "1992" & fatalities > 0) %>%
  group_by(year,event) %>%
  summarise(fatalities = sum(fatalities)) %>%
  arrange(year, event, fatalities) 

top_3_fatalities_per_year <- health_fatalities %>% 
  top_n(n=3, wt=fatalities) %>%
  arrange(year, event, fatalities)

other_fatalities <- setdiff(health_fatalities, top_3_fatalities_per_year)
other_fatalities$event = "other"
other_fatalities_per_year <- other_fatalities %>% 
  group_by(year, event) %>%
  summarise(fatalities = sum(fatalities))

# combine all data
all_fatalities_year_event <- rbind(top_3_fatalities_per_year, other_fatalities_per_year)
all_fatalities <- all_fatalities_year_event %>%
  group_by(year, event) %>%
  summarise(fatalities) %>%
  mutate(freq = round(100 * fatalities/sum(fatalities), 0)) # paste0(round(100 * fatalities/sum(fatalities), 0), "%")


g <- ggplot(all_fatalities, aes(x = year, y = fatalities, fill = event))
g + geom_bar(stat="identity", width = 0.5) + 
  geom_text(data=subset(all_fatalities,event!="other"), aes(label=ifelse(fatalities == pmax(fatalities), fatalities, "")), size=3) + # data=subset(all_fatalities,event!="other"),
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Top 3", 
       subtitle="Manufacturer across Vehicle Classes")

max_per_year <- subset(all_fatalities, event!="other") %>% group_by(year) %>% filter(fatalities==max(fatalities))

g <- ggplot(top_3_fatalities_per_year, aes(x = year, y = fatalities, fill = event))
g + geom_point(stat="identity", size=7, shape=21, color="black") + 
  geom_text(data=max_per_year, aes(label=pmax(fatalities)), size=2) + # data=subset(all_fatalities,event!="other"),
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Top 3", 
       subtitle="Manufacturer across Vehicle Classes")

g <- ggplot(top_3_fatalities_per_year, aes(x = year, y = fatalities, fill = event))
g + geom_bar(stat="identity", width=0.5) + 
  geom_text(data=max_per_year, aes(label=pmax(fatalities)), size=3) + # data=subset(all_fatalities,event!="other"),
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Top 3", 
       subtitle="Manufacturer across Vehicle Classes")


g <- ggplot(health_fatalities_max, aes(x = year, y = fatalities, fill = event))
g + geom_bar(stat="identity", width = 0.5, position = "dodge") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Top 3", 
       subtitle="Manufacturer across Vehicle Classes")

g <- ggplot(top_5_per_year, aes(x = decade, y=fatalities))
g + geom_bar(aes(fill=factor(event))) + 
  labs(title="title", 
       subtitle="subtitle",
       caption="caption",
       x="x axis",
       fill="fill?")

g <- ggplot(top_5_per_year, aes(x = year, y=fatalities, group=event, fill=event))
g + geom_area() + scale_x_discrete(breaks = c("1990","1995","2000", "2005", "2011")) +
  labs(title="Top 5", 
       subtitle="subtitle",
       caption="caption",
       x="x axis",
       fill="fill?")

#health_injuries_agg <- aggregate(injuries ~ event, health, sum)
#health_sort <- data.frame(apply(health_injuries_agg,2,sort,decreasing=T))



prop_dmg <- data.frame(as.Date(data$BGN_DATE, format = "%m/%d/%Y"), data$STATE, data$EVTYPE, as.numeric(data$PROPDMG), as.character(data$PROPDMGEXP))
colnames(prop_dmg) <- cbind("date", "state", "event", "prop-dmg", "prop-dmg-x")

## 