#######################################
# Stop, Question, and Frisk
# Plots
# by Mari Roberts
# 10/9/2020
#######################################

source('sqf_library')

library(ggplot2) # plots
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(ggpubr) # plots


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
mutate(stopped.bc = case_when((stopped.bc.desc==TRUE) ~ "Fits Description",
                              (stopped.bc.violent==TRUE) ~ "Violence",
                              (stopped.bc.other==TRUE) ~ "Other",
                              (stopped.bc.object==TRUE) ~ "Suspicious Object",
                              (stopped.bc.casing==TRUE) ~ "Casing",
                              (stopped.bc.lookout==TRUE) ~ "Lookout",
                              (stopped.bc.drugs==TRUE) ~ "Drug Transaction",
                              (stopped.bc.clothing==TRUE) ~ "Clothing",
                              (stopped.bc.bulge==TRUE) ~ "Suspicious Bulge",
                              (stopped.bc.furtive==TRUE) ~ "Furtive Movements"))

# create additional variable
sqf <- sqf %>% 
  mutate(additional = case_when((additional.report==TRUE) ~ "Report from Officer/Other",
                                (additional.investigation==TRUE) ~ "Ongoing Investigation",
                                (additional.proximity==TRUE) ~ "Proximity to Scene of Offense",
                                (additional.evasive==TRUE) ~ "Evasive Responses",
                                (additional.associating==TRUE) ~ "With Known Criminals",
                                (additional.highcrime==TRUE) ~ "Area of High Crime",
                                (additional.direction==TRUE) ~ "Change Direction from Officer",
                                (additional.time==TRUE) ~ "Time Fits Crime Incidence",
                                (additional.sights==TRUE) ~ "Sights/Sounds of Crime",
                                (additional.other==TRUE) ~ "Other"))

# POC vs nonPOC
sqf <- sqf %>% 
  mutate(poc = case_when((suspect.race=="black") ~ "BIPOC",
                         (suspect.race=="white") ~ "White",
                         (suspect.race=="hispanic") ~ "BIPOC",
                         (suspect.race=="asian") ~ "BIPOC",
                         (suspect.race=="native american") ~ "BIPOC",
                         (suspect.race=="other") ~ "BIPOC"))


###########
# hourly arrests
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

###########
# stop reason
###########

# count stop reasons
df <- sqf %>%
    group_by(stopped.bc) %>%
    summarise(count = n())

# remove NAs
df <- na.omit(df)

# for sorting bug in ggbarplot
df <- df[order(df$count),]
df$group <- c(1:10)
df$group <- factor(df$group)

p <- ggbarplot(df, x = "stopped.bc", y = "count",
          rotate=TRUE,
          fill = "group",      
          color = "white",          
          palette = "viridis",           
          sort.by.groups = TRUE,     
          sort.val = "asc",          
          x.text.angle = 60,          
          legend = "right", legend.title = "Stop Reason"
)
p

###########
# additional reason
###########

# count stop reasons
df <- sqf %>%
  group_by(additional) %>%
  summarise(count = n())

# remove NAs
df <- na.omit(df)

# for sorting bug in ggbarplot
df <- df[order(df$count),]
df$group <- c(1:10)
df$group <- factor(df$group)

p <- ggbarplot(df, x = "additional", y = "count",
               rotate=TRUE,
               fill = "group",      
               color = "white",          
               #palette = "Set1",           
               sort.by.groups = TRUE,     
               sort.val = "asc",          
               x.text.angle = 60,          
               legend = "right", legend.title = "Stop Reason"
)
p

################
# suspect sex
################

# count suspect sex
df <- sqf %>%
  group_by(suspect.sex) %>%
  summarise(count = n())
