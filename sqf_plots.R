#######################################
# Stop, Question, and Frisk
# Plots
# by Mari Roberts
# 10/9/2020
#######################################

source('sqf_library')

library(ggplot2) # graphs
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr)

# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggpubr")

###########
# import
###########

sqf.data <- read_csv('sqf_08_16.csv')

###########
# clean
###########

sqf <- sqf.data %>%
  select(id, year, found.weapon, found.gun, arrested, searched, suspect.race, suspect.age,
         suspect.build, suspect.sex, suspect.height, suspect.weight,
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         precinct, inside, location.housing, observation.period, officer.uniform,
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, radio.run, day, month, time.period, timestamp)

# Convert variable types as necessary
sqf <- sqf %>% mutate(suspect.race = as.factor(suspect.race), 
                      suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      location.housing = as.factor(location.housing),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct))

# Filter to complete cases
sqf <- sqf %>% filter(complete.cases(sqf))

# create stopped because variable
sqf <- sqf %>% 
mutate(stopped.bc = case_when((stopped.bc.desc==TRUE) ~ "stopped.bc.desc",
                              (stopped.bc.violent==TRUE) ~ "stopped.bc.violent",
                              (stopped.bc.other==TRUE) ~ "stopped.bc.other",
                              (stopped.bc.object==TRUE) ~ "stopped.bc.object",
                              (stopped.bc.casing==TRUE) ~ "stopped.bc.casing",
                              (stopped.bc.lookout==TRUE) ~ "stopped.bc.lookout",
                              (stopped.bc.drugs==TRUE) ~ "stopped.bc.drugs",
                              (stopped.bc.clothing==TRUE) ~ "stopped.bc.clothing",
                              (stopped.bc.bulge==TRUE) ~ "stopped.bc.bulge",
                              (stopped.bc.furtive==TRUE) ~ "stopped.bc.furtive"))

###########
# plot
###########

# filter data for heat map plot
df <- sqf %>% filter(year!=2016 & year!=2015) %>% select(arrested, year, day, month, timestamp)

# hourly arrests
df <- df %>% mutate(hour=hour(timestamp))

# count arrests per year month day and hour
df <- df %>% mutate(arrested = ifelse(arrested==FALSE,0,1))
df <- df %>% group_by(year, month, day, hour) %>% tally(arrested)

# fix variable formats for plotting
df$hour <- as.integer(df$hour)
df$month <- as.integer(df$month)
df$day <- as.integer(df$day)
df$year <- as.integer(df$year)

# heat plot
p <- ggplot(df,aes(day,hour,fill=n)) +
           geom_tile(color= "dark blue",size=0.1) + 
           scale_fill_viridis(name="Number of Arrests",option ="C")
p <- p + facet_grid(year~month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <- p + scale_x_continuous(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title= paste("Hourly Arrests"), x="Day", y="Hour Commencing")
p <- p + theme(legend.position = "bottom")+
         theme(plot.title=element_text(size = 14))+
         theme(axis.text.y=element_text(size=6)) +
         theme(strip.background = element_rect(colour="white"))+
         theme(plot.title=element_text(hjust=0))+
         theme(axis.ticks=element_blank())+
         theme(axis.text=element_text(size=7))+
         theme(legend.title=element_text(size=8))+
         theme(legend.text=element_text(size=6))+
         removeGrid()#ggExtra
p

###########
# plot
###########

library(ggridges)

# filter data for heat map plot
df <- sqf %>% select(year, suspect.age)

# fix variable format for plotting
df$year <- as.factor(df$year)

ggplot(df, aes(x = suspect.age, y = factor(year), fill = year)) +
  geom_density_ridges(alpha = 0.8, color = "white") +
  labs(x = "Suspect Age", y = "Year") +
  guides(fill = F) +
  theme_ridges()

###########
# plot
###########

df <- sqf %>%
  group_by(suspect.sex) %>%
  summarise(count = n())

###########
# plot
###########

# count stop reasons
df <- sqf %>%
    group_by(stopped.bc) %>%
    summarise(count = n())

# remove missing data
df <- na.omit(df)

# factor variable
df$stopped.bc <- factor(df$stopped.bc)

# plot
ggplot(df, aes(x = reorder(stopped.bc, count), y = count, fill = "blue")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), hjust = -0.25)+
  coord_flip() + 
  scale_fill_viridis_d() + theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank()) +
  labs(title="Reasons for Stops",
        x ="Count", y = "Stop Reason") 

################

gde_15 <- readOGR("/Users/mr4909/csgjc/sqf/nyc_map/nyc.shp")
