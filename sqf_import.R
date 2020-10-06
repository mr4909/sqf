#######################################
# Stop, Question, and Frisk
# Import and Clean
# by Mari Roberts
# 10/9/2020
#######################################

# load libraires and functions
source("sqf_library.R")

###########
# import
###########

# import and combine all the 9 years of data
sqf.data.full <- foreach(year=2008:2016, .combine='rbind.fill') %dopar% {
  filename <- paste0( year, '.csv')
  this.data <- read.csv(filename, na = c("", "NA", "*", "**"))
  this.data <- this.data %>%
    `names<-`(tolower(names(.)))
  this.data
}

# duplicate data for cleaning
sqf.data <- sqf.data.full

# import offense codes
offense.codes <- read.delim('offense-codes.tsv', header=FALSE, col.names=c('code','offense'))
offense.codes$offense <- tolower(offense.codes$offense)
# custom function to convert offense codes to human-readable names
convert.offense.code <- function(codes) {
  offenses <- offense.codes$offense[as.integer(codes)]
  offenses <- factor(offenses, levels=offense.codes$offense)
  offenses
}

# import arrest codes
arrest.offenses <- read.delim('arrest.offenses.tsv', header=FALSE, col.names=c('real.offense','nominal.offense'))
arrest.offenses$real.offense <- trim(arrest.offenses$real.offense)
arrest.offenses$nominal.offense <- trim(arrest.offenses$nominal.offense)
# combine and standardize top 100 reasons for arrest
convert.arrest.reasons <- function(rawlist) {
  ndx <- match(rawlist, arrest.offenses$nominal.offense)
  responses <- arrest.offenses$real.offense[ndx]
  ndx <- is.na(responses) & (rawlist!="")
  responses[ndx] <- "not.top.100"
  responses <- factor(responses)
  responses
}

###########
# clean
###########

# remove variables with more than 10% of its values missing in any year
sqf.data <- sqf.data %>% select(-recstat, -officrid, -sumoffen, -compyear, -comppct)

# create a timestamp
sqf.data <- sqf.data %>% mutate(datestop =  sprintf("%08d", as.integer(datestop)),
                                timestop = sprintf("%04d", as.integer(timestop)),
                                timestamp = mdy_hm(paste(datestop, timestop))) %>% 
                         # drop irrelevant variables
                         select(-datestop, -timestop) 

# fix variables for precinct, location, and serial number
sqf.data <- sqf.data %>% mutate(precinct = as.factor(pct), 
                                xcoord = as.integer(xcoord),
                                ycoord = as.integer(ycoord), 
                                serial = ser_num) %>%
                         # drop irrelevant variables
                         select(-pct, -ser_num)

###########
# recode 
###########

# recode Y/N variables into F/T using custom function (recode.yn)
sqf.data <- sqf.data %>% mutate(frisked = recode.yn(frisked), 
                                searched = recode.yn(searched), 
                                extra.reports = recode.yn(adtlrept),
                                reason.explained = recode.yn(explnstp), 
                                others.stopped = recode.yn(othpers),
                                arrested = recode.yn(arstmade),
                                summons.issued = recode.yn(sumissue),
                                radio.run = recode.yn(radio)) 

#  recode or rename other binary variables using custom functions
sqf.data <- sqf.data %>% mutate(inside = recode.io(inout), # inside or outside
                                observation.period = perobs, # rename
                                suspected.crime = convert.offense.code(detailcm), # convert offense code to readable names
                                officer.verbal = recode.0V(offverb), # whether the officer provided a verbal statement when not in uniform
                                officer.shield = recode.0S(offshld), # # whether the officer provided a shield when not in uniform
                                arrested.reason = convert.arrest.reasons(arstoffn)) %>% # convert arrest code into readable names
                        # drop irrelevant variables
                        select(-adtlrept, -explnstp, -othpers, -arstmade, -sumissue, -radio, 
                               -inout, -perobs, -detailcm, -offverb, -offshld, -arstoffn, -forceuse)

# location: recode 'P' (for Pedestrian, which occurs mainly after 2008) and blank as 'neither'.
# H       P       T 
# 435942  476059 2060832  241163
sqf.data <- sqf.data %>% mutate(location.housing = 
                                  recode.factor(sqf.data.full$trhsloc, 
                                                c('P', 'H', 'T'), 
                                                c('neither', 'housing', 'transit')))
sqf.data <- sqf.data %>% mutate(location.housing = 
                                  replace(location.housing, 
                                  is.na(location.housing), 'neither')) %>% select(-trhsloc)

# period of stop (in minutes)
sqf.data <- sqf.data %>% mutate(stop.length = perstop) %>% select(-perstop)

# recode type of id and officer in uniform
sqf.data <- sqf.data %>% 
  mutate(identification = recode.factor(typeofid, c('O','P','R','V'),
                          c('other', 'photo', 'refused', 'verbal')),
         officer.uniform = recode.factor(offunif, c('M', 'N', 'Y'),
                           c('N', 'N', 'Y')),
         officer.uniform = recode.yn(officer.uniform)) %>% 
  select(-typeofid, -offunif)

# recode physical force variables
sqf.data <- sqf.data %>% mutate(force.hands = recode.yn(pf_hands),
                                force.wall = recode.yn(pf_wall),
                                force.ground = recode.yn(pf_grnd),
                                force.drawn = recode.yn(pf_drwep),
                                force.pointed = recode.yn(pf_ptwep),
                                force.baton = recode.yn(pf_baton),
                                force.handcuffs = recode.yn(pf_hcuff),
                                force.pepper = recode.yn(pf_pepsp),
                                force.other = recode.yn(pf_other)) %>% 
                          # drop irrelevant variables
                          select(-pf_hands, -pf_wall, -pf_grnd, 
                                 -pf_drwep, -pf_ptwep, -pf_baton, 
                                 -pf_hcuff, -pf_pepsp, -pf_other)

# recode primary circumstances of stop
sqf.data <- sqf.data %>% mutate(stopped.bc.object = recode.yn(cs_objcs),
                                stopped.bc.desc = recode.yn(cs_descr),
                                stopped.bc.casing = recode.yn(cs_casng),
                                stopped.bc.lookout = recode.yn(cs_lkout),
                                stopped.bc.clothing = recode.yn(cs_cloth),
                                stopped.bc.drugs = recode.yn(cs_drgtr),
                                stopped.bc.furtive = recode.yn(cs_furtv),
                                stopped.bc.violent = recode.yn(cs_vcrim),
                                stopped.bc.bulge = recode.yn(cs_bulge),
                                stopped.bc.other = recode.yn(cs_other)) %>% 
                          # drop irrelevant variables
                          select(-cs_objcs, -cs_descr, -cs_casng, -cs_lkout, 
                                 -cs_cloth, - cs_drgtr, -cs_furtv, -cs_vcrim, 
                                 -cs_bulge, -cs_other)

# recode reasons for frisk
sqf.data <- sqf.data %>% mutate(frisked.bc.suspected.crime = recode.yn(rf_vcrim),
                                frisked.bc.weapons = recode.yn(rf_othsw),
                                frisked.bc.attire = recode.yn(rf_attir),
                                frisked.bc.actual.crime = recode.yn(rf_vcact),
                                frisked.bc.noncompliance = recode.yn(rf_rfcmp),
                                frisked.bc.threats = recode.yn(rf_verbl),
                                frisked.bc.prior = recode.yn(rf_knowl),
                                frisked.bc.furtive = recode.yn(rf_furt),
                                frisked.bc.bulge = recode.yn(rf_bulg)) %>% 
                          # drop irrelevant variables
                          select(-rf_vcrim, -rf_othsw, -rf_attir, -rf_vcact, 
                                 -rf_rfcmp, -rf_verbl, -rf_knowl,-rf_furt, 
                                 -rf_bulg)

# recode secondary circumstances of stop
sqf.data <- sqf.data %>% mutate(additional.report = recode.yn(ac_rept),
                                additional.investigation = recode.yn(ac_inves),
                                additional.proximity = recode.yn(ac_proxm),
                                additional.evasive = recode.yn(ac_evasv),
                                additional.associating = recode.yn(ac_assoc),
                                additional.direction = recode.yn(ac_cgdir),
                                additional.highcrime = recode.yn(ac_incid),
                                additional.time = recode.yn(ac_time),
                                additional.sights = recode.yn(ac_stsnd),
                                additional.other = recode.yn(ac_other)) %>% 
                          # drop irrelevant variables
                          select(-ac_rept, -ac_inves, -ac_proxm, -ac_evasv, 
                                 -ac_assoc, -ac_cgdir, -ac_incid, -ac_time, 
                                 -ac_stsnd, -ac_other)

# recode basis of search
sqf.data <- sqf.data %>% mutate(searched.hardobject = recode.yn(sb_hdobj),
                                searched.outline = recode.yn(sb_outln),
                                searched.admission = recode.yn(sb_admis),
                                searched.other = recode.yn(sb_other)) %>% 
                         # drop irrelevant variables
                         select(-sb_hdobj, -sb_outln, -sb_admis, -sb_other)

# recode results of frisk/search
sqf.data <- sqf.data %>% mutate(found.contraband = recode.yn(contrabn),
                                found.pistol = recode.yn(pistol),
                                found.rifle = recode.yn(riflshot),
                                found.assault = recode.yn(asltweap),
                                found.knife = recode.yn(knifcuti),
                                found.machinegun = recode.yn(machgun),
                                found.other = recode.yn(othrweap)) %>% 
                                # drop irrelevant variables
                                select(-contrabn, -pistol, -riflshot, 
                                       -asltweap, -knifcuti, -machgun, 
                                       -othrweap)

# recode demographics of stop subject
sqf.data <- sqf.data %>% mutate(suspect.sex = recode.factor(sex, c('M', 'F'),
                                                            c('male', 'female')),
                                suspect.race = recode.factor(race, c('A','B','I','P','Q','W','Z'),
                                                             c('asian','black','native american',
                                                               'black hispanic','white hispanic',
                                                               'white','other')),
                                suspect.hispanic = (suspect.race %in% c('black hispanic',
                                                                        'white hispanic'))) %>% 
                        # drop irrelevant variables
                        select(-sex, -race)

# remove outliers for age and DOB and fix date format
sqf.data <- sqf.data %>% mutate(suspect.age = age, 
                                suspect.age = replace(suspect.age, suspect.age > 100, NA),
                                dob = sprintf("%08d", as.integer(dob)),
                                suspect.dob = mdy(dob),
                                suspect.dob = replace(suspect.dob, suspect.dob=='1900-12-31', NA)) %>% 
                        # drop irrelevant variables
                        select(-age, -dob)

# remove outliers for weight (in lbs) and fix height
sqf.data <- sqf.data %>% mutate(suspect.height = (ht_feet + as.numeric(ht_inch)/12),
                                suspect.weight = weight,
                                suspect.weight = replace(suspect.weight, suspect.weight >= 700, NA)) %>% 
                        # drop irrelevant variables
                        select(-ht_feet, -ht_inch, -weight)

# recode hair color, eye color, and build
sqf.data <- sqf.data %>% mutate(suspect.hair = recode.factor(haircolr, 
                                                             c('BA','BK','BL','BR','DY','FR',
                                                               'GY', 'RD', 'SN', 'SP', 'WH', 'XX', 'ZZ'),
                                                             c('bald', 'black', 'blond', 'brown', 'dyed', 
                                                               'frosted', 'gray', 'red', 'sandy', 'salt and pepper', 
                                                               'white', 'unknown', 'other')),
                                suspect.eye = recode.factor(eyecolor,
                                                            c('BK','BL','BR','GY','GR','HA', 'MA', 'Z', 
                                                              'ZZ', 'P', 'PK','DF', 'XX',  'MC', 'VI'),
                                                            c('black','blue','brown','gray','green','hazel', 
                                                              'maroon',  'other', 'other','pink','pink', 
                                                              'two different','unknown', 'unknown','violet')),
                                suspect.build = recode.factor(build,
                                                              c('H', 'M', 'T', 'U', 'Z'),
                                                              c('heavy', 'medium', 'thin', 'muscular', 'unknown'))) %>% 
                        # drop irrelevant variables
                        select(-haircolr, -eyecolor, -build)

# add extra useful fields and filter data

# fields for weapon found or gun found
sqf.data <- sqf.data %>% mutate(found.gun = (found.pistol|found.rifle|found.assault|found.machinegun),
                                found.weapon = (found.pistol|found.rifle|found.assault|found.machinegun|
                                                found.knife|found.other))
# add a unique id
sqf.data$id <- 1:nrow(sqf.data)

# eliminate all ages except for those between 10 and 90.
sqf.data <- sqf.data %>% filter(suspect.age >= 10 & suspect.age <= 90)

# convert coordinates to lat/lon
coords <- proj4::project(list(sqf.data$xcoord, sqf.data$ycoord), nyc.proj, inverse=TRUE)
sqf.data$lat <- coords$y
sqf.data$lon <- coords$x

# recode suspect.race for "white hispanic" and "black hispanic" to "hispanic"
levels(sqf.data$suspect.race) <- c("asian", "black", "native.american", "hispanic", "hispanic", "white", "other")

# add weekday, month, and time (6 four-hour-bins denoted by 1 through 6)
# sqf.data <- sqf.data %>% mutate(day = wday(timestamp, label = T, abbr = F),
#                                 month = month(timestamp, label = T, abbr = F),
#                                 time.period = case_when(
#                                   hour(timestamp) < 4 ~ '1',
#                                   hour(timestamp) >= 4 & hour(timestamp) < 8 ~ '2',
#                                   hour(timestamp) >= 8 & hour(timestamp) < 12 ~ '3',
#                                   hour(timestamp) >= 12 & hour(timestamp) < 16 ~ '4',
#                                   hour(timestamp) >= 16 & hour(timestamp) < 20 ~ '5',
#                                   hour(timestamp) >= 20 ~ '6'
#                                 ))

# drop remaining irrelevant columns
sqf.data <- sqf.data %>% select(-crimsusp, -repcmd, -revcmd, -othfeatr, -addrtyp, 
                                -rescode, -premtype, -premname, -addrnum, -stname,
                                -stinter, -crossst, -aptnum, -state, -zip, -addrpct,
                                -post, -serial)

###########
# NAs
###########

# proportion of missing values in all columns by year
# proportion.nas <- sqf.data %>% group_by(year) %>% summarize_all(funs(nas = sum(is.na(.))/n()))

# variables with more than 10% missing values
sqf.nas.variables <- c("additional.report", "additional.investigation", "additional.proximity", 
                       "additional.evasive","additional.associating", "additional.direction", 
                       "additional.highcrime", "additional.time","additional.sights", "additional.other",
                       "stopped.bc.desc", "stopped.bc.violent", "stopped.bc.other", "stopped.bc.object",
                       "stopped.bc.casing", "stopped.bc.lookout", "stopped.bc.drugs", "stopped.bc.clothing",
                       "stopped.bc.furtive", "stopped.bc.bulge",
                       "radio.run","frisked.bc.suspected.crime","frisked.bc.weapons","frisked.bc.attire",
                       "frisked.bc.actual.crime","frisked.bc.noncompliance","frisked.bc.threats",
                       "frisked.bc.prior","frisked.bc.furtive","frisked.bc.bulge",
                       "force.hands", "force.wall","force.ground","force.drawn","force.pointed",
                       "force.baton", "force.handcuffs", "force.pepper", "force.other",
                       "searched.hardobject","searched.outline","searched.admission","searched.other",
                       "found.weapon", "found.gun", "arrested", "searched", "frisked","extra.reports")

# custom function to replace NAs with FALSE
sqf.data[update.logical(sqf.data, sqf.nas.variables, is.na(sqf.data))] <- FALSE

# view proportion of missing values in all columns by year again
# proportion.nas <- sqf.data %>% group_by(year) %>% summarize_all(funs(nas = sum(is.na(.))/n()))

# write csv
write.csv(sqf.data, "sqf_08_16.csv")
