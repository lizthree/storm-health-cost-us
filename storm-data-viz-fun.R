library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)
library(gganimate)
# inspired by: https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da

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

data_raw <- fread("repdata_data_StormData.csv", select = c("BGN_DATE", "STATE", "EVTYPE","FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP"))

data <- na.omit(data_raw)

data$EVTYPE <- as.character(tolower(data$EVTYPE))
data$EVTYPE[grepl("hurricane|tropical storm",data$EVTYPE)] <- "hurricane\tropical storm"
data$EVTYPE[grepl("heat|warm",data$EVTYPE)] <- "heat/unseasonaly warm"
data$EVTYPE[grepl("ice|freezing|freeze|snow|cold|windchill|low temperature|frost",data$EVTYPE)] <- "freezing cold/snow"
data$EVTYPE[grepl("winter|storm|blizzard",data$EVTYPE)] <- "winter storm/blizzard"
data$EVTYPE[grepl("storm surge",data$EVTYPE)] <- "storm surge"
data$EVTYPE[grepl("flood|flooding",data$EVTYPE)] <- "flooding/flash flooding"
data$EVTYPE[grepl("current|currents",data$EVTYPE)] <- "rip current"
data$EVTYPE[grepl("tstm|thunderstorm wind",data$EVTYPE)] <- "thunderstorm wind"
data$EVTYPE[grepl("fire",data$EVTYPE)] <- "fire/wildfire"
data$EVTYPE[grepl("hail",data$EVTYPE)] <- "hail"
data$EVTYPE[grepl("high winds|strong winds|high wind|strong wind|gusty",data$EVTYPE)] <- "heavy/strong winds"
data$EVTYPE[grepl("high surfs|heavy surfs|high surf|heavy surf|high sea|high seas|rough seas",data$EVTYPE)] <- "heavy/high surf/sea"
data$EVTYPE[grepl("avalanche|avalance",data$EVTYPE)] <- "avalanche"
data$EVTYPE[grepl("hypothermia",data$EVTYPE)] <- "hypothermia/exposure"
data$EVTYPE[grepl("lightning",data$EVTYPE)] <- "lightning"
data$EVTYPE[grepl("tornado",data$EVTYPE)] <- "tornado"
data$EVTYPE[grepl("mudslide|landslide",data$EVTYPE)] <- "mudslide/landslide"
data$EVTYPE[grepl("heavy rain|heavy rains|rain|hvy rain",data$EVTYPE)] <- "heavy rain"
data$EVTYPE[grepl("wind|winds",data$EVTYPE)] <- "winds"
#health$event <- factor(health$event)

health <- data.frame(as.Date(data$BGN_DATE, "%m/%d/%Y"), as.character(data$STATE), as.character(tolower(data$EVTYPE)), as.numeric(data$FATALITIES), as.numeric(data$INJURIES))
colnames(health) <- c("date", "state", "event", "fatalities", "injuries")
health <- separate(health,date,into=c("year","month","day"),sep="-", drop=TRUE)
health$event = as.character(health$event)
# > range(health$year)
# [1] "1950" "2011"


#rm(data_raw)
# > dim(health)
# [1] 902297      5

# unique(data$EVTYPE[grepl("lightning", data$EVTYPE)])
# combine like-data, found that certain data should be considered together
# hurricane's don't compare to other high-fatalities events
# different variations of heat at the top
# avoided rain or hail

# 5 rounds down so 1994 goes to 1990
health_fatalities <- health %>%
  select(event, year, fatalities) %>%
  filter(year > "1999" & fatalities > 0) %>%
  #mutate(decade = as.numeric(year)-(as.numeric(year)%%5)) %>%
  group_by(year,event) %>%
  summarise(fatalities = sum(fatalities)) %>%
  arrange(year, event, fatalities) 


co = c("darkorange3", "steelblue3")#, "bisque3", "slateblue4")
event_co = c("flooding/flash flooding", "heat/unseasonaly warm", "tornado", "winter storm/blizzard")

health_fatalities_formatted <- health_fatalities %>%
  ungroup() %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-fatalities),
         Value_rel = fatalities/fatalities[rank==1],
         Value_lbl = paste0(" ",round(fatalities))) %>%
  group_by(event) %>% 
  filter(rank<=10) %>%
  mutate(highlight = ifelse(event %in% event_co, "H", "L")) %>%
  ungroup()

staticplot = ggplot(health_fatalities_formatted, aes(rank, group = event, 
                                                     fill = as.factor(event), color = as.factor(event))) +
  geom_tile(aes(y = fatalities/2,
                height = fatalities,
                width = 0.9, fill=highlight), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(event, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=fatalities,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +# scale_color_manual(values=co) +
  guides(color = co, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(year, transition_length = 5, state_length = 3) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Fatalities per Year : {closest_state}',  
       subtitle  =  "Top 5 Weather Events",
       caption  = "Data Source: Storm Data")

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))