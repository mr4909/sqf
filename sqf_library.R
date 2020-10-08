#######################################
# Stop, Question, and Frisk
# Libraries and custom functions
# by Mari Roberts
# 10/9/2020
#######################################

# load necessary packages
requiredPackages = c('plyr',
                     'dplyr',
                     'tidyverse',
                     'lubridate',
                     'ROCR',
                     'foreach',
                     'proj4',
                     'data.table',
                     'ggplot2',
                     'readr')
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

###################
# custom functions
###################

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# read.por tries two different methods for reading in an SPSS file.
# NOTE: for compatibility, column names are converted to lowercase
read.por <- function(filename) {
  df <- data.frame()
  try(df <- data.frame(read.spss(filename), stringsAsFactors=FALSE), silent = TRUE)
  if (nrow(df) == 0) {
    try(df <- data.frame(as.data.set(spss.portable.file(filename)), stringsAsFactors=FALSE), silent = TRUE)
  }
  names(df) <- tolower(names(df))
  df
}

# functions to recode factor levels
recode.factor <- function(f, old.levels, new.levels) {
  f.new <- as.factor(f)
  level.map <- new.levels
  names(level.map) <- old.levels
  levels(f.new) <- level.map[levels(f.new)]
  f.new <- factor(f.new, levels = unique(new.levels))
  f.new
}

recode.yn <- function(f) {
  f.new <- factor(f, levels=c('N','Y'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

recode.yesno <- function(f) {
  f.new <- factor(f, levels=c('NO','YES'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for the stop being inside or outside
recode.io <- function(f) {
  f.new <- factor(f, levels=c('O','I'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for whether the officer provided a verbal statement when not in uniform
recode.0V <- function(f) {
  f.new <- factor(f, levels=c('0','V'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for whether the officer provided a shield when not in uniform
recode.0S <- function(f) {
  f.new <- factor(f, levels=c('0','S'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# logit function
logit <- function(rawlist) {
  output <- exp(rawlist)/(1+exp(rawlist))
  output
}

# inverse logit function
inv.logit <- function(x) {
  p <- exp(x)/(1+exp(x))
  p
}

# list of drug-related suspected crimes
druglist <- c("criminal possession of marihuana", "criminal possesion of controlled substance", 
              "criminal sale of controlled substance", "criminal sale of marihuana")

# proj4 strings
epsg.2263 = "+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
nyc.proj = "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"

# function to replace NAs with FALSE
update.logical <- function(dt, cols, criteria) {
  require(data.table)
  x <- as.data.frame(which(criteria==TRUE, arr.ind = TRUE))
  y <- as.matrix(subset(x, x$col %in% which((names(dt) %in% cols), arr.ind = TRUE)))
  y
}

# standardize
standardize <- function(x) {
  x.std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  x.std
}