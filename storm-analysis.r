library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)
library(gganimate)

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

health_fatalities_formatted <- health_fatalities %>%
  ungroup() %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-fatalities),
         Value_rel = fatalities/fatalities[rank==1],
         Value_lbl = paste0(" ",round(fatalities))) %>%
  group_by(event) %>% 
  filter(rank<=10) %>%
  ungroup()

staticplot = ggplot(health_fatalities_formatted, aes(rank, group = event, 
                                       fill = as.factor(event), color = as.factor(event))) +
  geom_tile(aes(y = fatalities/2,
                height = fatalities,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(event, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=fatalities,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
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
# 
# animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
#         renderer = ffmpeg_renderer()) -> for_mp4
# anim_save("fatalities.mp4", animation = for_mp4 )

#table(health_fatalities$year)
#1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 
#3    3   14   16   24   20   21   17   18   17   16   17   16   13   13   14   13 
#2008 2009 2010 2011 
#13   13   11   14 

top_3_fatalities_per_year <- health_fatalities %>% 
  top_n(n=3, wt=fatalities) %>%
  arrange(year, desc(fatalities))

top_5_fatalities_per_year <- health_fatalities %>% 
  top_n(n=5, wt=fatalities) %>%
  arrange(year, desc(fatalities))

top_10_fatalities_events <- health_fatalities %>%
  ungroup(year) %>%
  select(-year) %>%
  group_by(event) %>%
  summarise(fatalities = sum(fatalities)) %>%
  top_n(n=10, wt=fatalities) %>%
  ungroup() %>% arrange(desc(fatalities)) %>%
  mutate(highlight = ifelse(grepl("heat|flood|tornado", event), "T", "F")) 


co = c("grey50", "red")
ggplot(top_10_fatalities_events, aes(x=reorder(event, -desc(fatalities)), y=fatalities)) +
  geom_bar(aes(fill=highlight), stat="identity", width=0.5) +
  coord_flip() + theme_bw() + scale_fill_manual(values=co) +
  labs(x="Weather event", 
       y="Fatalities", 
       title="Lead Causes of Fatalities by Weather", 
       subtitle="Years 1993 to 2011", 
       caption="Source: https://github.com/") +
  theme(plot.title = element_text(face="bold"),
        plot.background=element_rect(fill="white"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.title.y = element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        legend.position = "none",
        aspect.ratio = 1/2.5)



percent_top_3_fatalities_events <- health_fatalities %>%
  mutate(freq = round(100 * fatalities/sum(fatalities), 0)) %>%
  group_by(year,event,freq) %>%
  summarise(fatalities = sum(fatalities)) %>%
  top_n(n=3, wt=fatalities) %>%
  ungroup() %>% 
  arrange(year, desc(fatalities), freq) %>%
  mutate(highlight = ifelse(grepl("heat|flood|tornado", event), "T", "F")) %>%
  filter(highlight == "T")

co = c("blue", "orange", "brown")
labs = c("Flood related", "Heat related", "Tornado related")
ggplot(percent_top_3_fatalities_events, aes(x=year, y=freq,col=event)) + 
  geom_point() +
  facet_grid(event~.) + theme_bw() + scale_color_manual(values=co) +
  labs(x="Year", 
       y="Percent of fatalities", 
       title="Lead Causes of Fatalities by Weather", 
       subtitle="Years 1993 to 2011", 
       caption="Source: https://github.com/") +
  scale_x_discrete(breaks=c(1993,2002,2011), labels=c("1993","2002","2011")) +
  scale_y_continuous(breaks=c(0,20,40,60,80), labels=c("0","20","40","60","80")) +
  geom_text(data=data.frame("Flood related", "Heat related", "Tornado related"), aes(x = 15, y = 60, label=labs),inherit.aes = FALSE) +
  theme(strip.background = element_rect(fill = "white", colour = "black",linetype="blank"),
        strip.text.y = element_blank(),#text(size = 7.5, angle = 90),
        plot.title = element_text(face="bold"),
        plot.background=element_rect(fill="white"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        legend.position = "none")






#other_fatalities <- setdiff(health_fatalities, top_5_fatalities_per_year)
#other_fatalities$event = "other"
#other_fatalities_per_year <- other_fatalities %>% 
#  group_by(decade, event) %>%
#  summarise(fatalities = sum(fatalities))
#
## combine all data
#all_fatalities_year_event <- rbind(top_5_fatalities_per_year, other_fatalities_per_year)
#all_fatalities <- all_fatalities_year_event %>%
#  group_by(decade, event) %>%
#  summarise(fatalities) %>%
#  mutate(freq = paste0(round(100 * fatalities/sum(fatalities), 0), "%"))
#
#
##multiple others per year
#all_fatalities_year_event1 <- rbind(top_3_fatalities_per_year, other_fatalities)
#g <- ggplot(data=top_3_fatalities_per_year, aes(x=year, y=fatalities, fill=event, order=fatalities)) + 
#  geom_bar(stat="identity",width=0.5, position = "dodge") +
#  geom_boxplot(data = other_fatalities, aes(x=year, y=fatalities))+
#  coord_flip()
#
#g <- ggplot(data=top_5_fatalities_per_year, aes(x = decade, y = fatalities, fill=event)) + 
#  geom_bar(stat="identity", position = "dodge") +
#  coord_flip()
#
#
#g <- ggplot(all_fatalities, aes(x = year, y = fatalities, fill = event))
#g + geom_bar(stat="identity", width = 0.5) + 
#  geom_text(data=subset(all_fatalities,event!="other"), aes(label=ifelse(fatalities == pmax(fatalities), fatalities, "")), size=3) + # data=subset(all_fatalities,event!="other"),
#  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
#  labs(title="Top 3", 
#       subtitle="Manufacturer across Vehicle Classes")
#
#max_per_year <- subset(all_fatalities, event!="other") %>% group_by(year) %>% filter(fatalities==max(fatalities))
#
#g <- ggplot(top_3_fatalities_per_year, aes(x = year, y = fatalities, fill = event))
#g + geom_point(stat="identity", size=7, shape=21, color="black") + 
#  geom_text(data=max_per_year, aes(label=pmax(fatalities)), size=2) + # data=subset(all_fatalities,event!="other"),
#  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
#  labs(title="Top 3", 
#       subtitle="Manufacturer across Vehicle Classes")
#
#g <- ggplot(top_3_fatalities_per_year, aes(x = year, y = fatalities, fill = event, order = fatalities))
#g + geom_bar(stat="identity",width=0.5) + 
#  geom_text(data=max_per_year, aes(label=pmax(fatalities)), size=3) + # data=subset(all_fatalities,event!="other"),
#  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
#  labs(title="Top 3", 
#       subtitle="Manufacturer across Vehicle Classes")
#
#
#g <- ggplot(health_fatalities_max, aes(x = year, y = fatalities, fill = event))
#g + geom_bar(stat="identity", width = 0.5, position = "dodge") + 
#  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
#  labs(title="Top 3", 
#       subtitle="Manufacturer across Vehicle Classes")
#
#g <- ggplot(top_5_per_year, aes(x = decade, y=fatalities))
#g + geom_bar(aes(fill=factor(event))) + 
#  labs(title="title", 
#       subtitle="subtitle",
#       caption="caption",
#       x="x axis",
#       fill="fill?")
#
#g <- ggplot(top_5_per_year, aes(x = year, y=fatalities, group=event, fill=event))
#g + geom_area() + scale_x_discrete(breaks = c("1990","1995","2000", "2005", "2011")) +
#  labs(title="Top 5", 
#       subtitle="subtitle",
#       caption="caption",
#       x="x axis",
#       fill="fill?")
#
#p <- ggplot(top_3_fatalities_per_year, aes(x=year, y=fatalities)) + 
#  geom_boxplot()

#health_injuries_agg <- aggregate(injuries ~ event, health, sum)
#health_sort <- data.frame(apply(health_injuries_agg,2,sort,decreasing=T))



prop_dmg <- data.frame(as.Date(data$BGN_DATE, format = "%m/%d/%Y"), as.character(data$STATE), data$EVTYPE, as.numeric(data$PROPDMG), as.character(data$PROPDMGEXP), as.numeric(data$CROPDMG), as.character(data$CROPDMGEXP))
colnames(prop_dmg) <- cbind("date", "state", "event", "prop", "prop_x", "crop", "crop_x")
prop_dmg <- separate(prop_dmg,date,into=c("year","month","day"),sep="-", drop=TRUE)


# Sorting the property exponent data
prop_dmg$prop_exp[prop_dmg$prop_x == "K"] <- 1000
prop_dmg$prop_exp[prop_dmg$prop_x == "M"] <- 1e+06
prop_dmg$prop_exp[prop_dmg$prop_x == ""] <- 1
prop_dmg$prop_exp[prop_dmg$prop_x == "B"] <- 1e+09
prop_dmg$prop_exp[prop_dmg$prop_x == "m"] <- 1e+06
prop_dmg$prop_exp[prop_dmg$prop_x == "0"] <- 1
prop_dmg$prop_exp[prop_dmg$prop_x == "5"] <- 1e+05
prop_dmg$prop_exp[prop_dmg$prop_x == "6"] <- 1e+06
prop_dmg$prop_exp[prop_dmg$prop_x == "4"] <- 10000
prop_dmg$prop_exp[prop_dmg$prop_x == "2"] <- 100
prop_dmg$prop_exp[prop_dmg$prop_x == "3"] <- 1000
prop_dmg$prop_exp[prop_dmg$prop_x == "h"] <- 100
prop_dmg$prop_exp[prop_dmg$prop_x == "7"] <- 1e+07
prop_dmg$prop_exp[prop_dmg$prop_x == "H"] <- 100
prop_dmg$prop_exp[prop_dmg$prop_x == "1"] <- 10
prop_dmg$prop_exp[prop_dmg$prop_x == "8"] <- 1e+08
# give 0 to invalid exponent data, so they not count in
prop_dmg$prop_exp[prop_dmg$prop_x == "+"] <- 0
prop_dmg$prop_exp[prop_dmg$prop_x == "-"] <- 0
prop_dmg$prop_exp[prop_dmg$prop_x == "?"] <- 0
# compute the property damage value
prop_dmg$prop_cost <- prop_dmg$prop * prop_dmg$prop_exp

# Sorting the property exponent data
prop_dmg$crop_exp[prop_dmg$crop_x == "M"] <- 1e+06
prop_dmg$crop_exp[prop_dmg$crop_x == "K"] <- 1000
prop_dmg$crop_exp[prop_dmg$crop_x == "m"] <- 1e+06
prop_dmg$crop_exp[prop_dmg$crop_x == "B"] <- 1e+09
prop_dmg$crop_exp[prop_dmg$crop_x == "0"] <- 1
prop_dmg$crop_exp[prop_dmg$crop_x == "k"] <- 1000
prop_dmg$crop_exp[prop_dmg$crop_x == "2"] <- 100
prop_dmg$crop_exp[prop_dmg$crop_x == ""] <- 1
# give 0 to invalid exponent data, so they not count in
prop_dmg$crop_exp[prop_dmg$crop_x == "?"] <- 0
# compute the crop damage value
prop_dmg$crop_cost <- prop_dmg$crop * prop_dmg$crop_exp


prop_test <- prop_dmg %>% select(year, event, prop_cost) %>%
    filter(prop_cost > 0, year > "1992") %>%
    group_by(year, event) %>%
    summarise(cost = sum(prop_cost)) %>%
    arrange(year, event, desc(cost)) %>%
    mutate(highlight = "P")

crop_test <- prop_dmg %>% select(year, event, crop_cost) %>%
  filter(crop_cost > 0) %>%
  group_by(year, event) %>%
  summarise(cost = sum(crop_cost)) %>%
  arrange(year, event, desc(cost)) %>%
  mutate(highlight = "C")

top_prop_dmg_events <- prop_test %>%
  group_by(year, event, highlight) %>%
  summarise(cost = sum(cost)) %>%
  top_n(n=1, wt=cost) %>%
  ungroup() %>% arrange(year, desc(cost)) 



# Most
# unique(top_crop_dmg_events$event)
# [1] flooding/flash flooding freezing cold/snow      drought                 winter storm/blizzard  
# [5] hail                   
# unique(top_prop_dmg_events$event)
# [1] winter storm/blizzard   flooding/flash flooding fire/wildfire           tornado                

top_crop_events <- unique(top_crop_dmg_events$event)
top_prop_events <- unique(top_prop_dmg_events$event)

top_prop_dmg_events_10 <- prop_test %>%
  ungroup(year) %>%
  select(-year) %>%
  group_by(event) %>%
  summarise(cost = sum(cost)) %>%
  top_n(n=10, wt=cost) %>%
  ungroup() %>% arrange(desc(cost)) %>%
  mutate(Type = "Property")

top_crop_dmg_events_10 <- crop_test %>%
  ungroup(year) %>%
  select(-year) %>%
  group_by(event) %>%
  summarise(cost = sum(cost)) %>%
  top_n(n=10, wt=cost) %>%
  ungroup() %>% arrange(desc(cost)) %>%
  mutate(Type = "Crop")


all_crop_prop_dmg_10 <- rbind(top_prop_dmg_events_10, top_crop_dmg_events_10)





ggplot(all_crop_prop_dmg_10, aes(x=reorder(event, -desc(cost)), y=cost)) +
  geom_bar(aes(fill=Type), stat="identity", width=0.5, position = "dodge") +
  coord_flip() + theme_bw() + 
  labs(y="Cost (dollars)",  
       title="Top 10 causes of property and crop damage", 
       subtitle="USA - Years 1993 to 2011", 
       caption="Source: https://github.com/") +
  theme(strip.background = element_rect(fill = "white", colour = "black",linetype="blank"),
        strip.text.y = element_blank(),
        plot.title = element_text(face="bold"),
        plot.background=element_rect(fill="white"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y = element_blank(),
        panel.border=element_blank(),
        legend.position = c(0.90, 0.15))

crops = c("flooding/flash flooding", "winter storm/blizzard", "drought", "freezing cold/snow")
filtered_top_crop_dmg_events <- crop_test %>%
  filter(event %in% crops) %>%
  group_by(year, event) %>%
  summarise(cost = sum(cost)) %>%
  top_n(n=5, wt=cost) %>%
  ungroup() %>% arrange(year,desc(cost)) 

co = c("darkorange3", "blue", "steelblue3", "bisque3", "slateblue4")
labs = c("Drought related", "Flood related", "Freezing/snow related", "Hail related", "Thunderstorm related")
df_crops = data.frame("Drought related", "Flood related", "Freezing/snow related", "Hail related", "Thunderstorm related")
ggplot(filtered_top_crop_dmg_events, aes(x=year, y=cost,col=event)) + 
  geom_point() + facet_grid(event~.)
  facet_grid(event~.) + theme_bw() + scale_color_manual(values=co) +
  labs(x="Year", 
       y="Percent of overall cost", 
       title="Percent of cost for leading causes of weather damage", 
       subtitle="Years 1993 to 2011", 
       caption="Source: https://github.com/") +
  scale_x_discrete(breaks=c(1993,2002,2011), labels=c("1993","2002","2011")) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100), labels=c("0","20","40","60","80","100")) +
  geom_text(data=df_crops, aes(x = 5, y = 70, label=labs),inherit.aes = FALSE, size=3) +
  theme(strip.background = element_rect(fill = "white", colour = "black",linetype="blank"),
        strip.text.y = element_blank(),#text(size = 7.5, angle = 90),
        plot.title = element_text(face="bold"),
        plot.background=element_rect(fill="white"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        legend.position = "none")
