#######################################
# Stop, Question, and Frisk
# Analysis
# by Mari Roberts
# 10/9/2020
#######################################

source('sqf_library.R')

###########
# import
###########

sqf.data <- read_csv('sqf_08_16.csv')

###########
# clean
###########

# filter by criminal posession of a weapon and select variables
sqf <- sqf.data %>% filter(suspected.crime=='cpw') %>% 
  select(id, year, found.weapon, found.gun, arrested, searched, suspect.race, suspect.age,
         suspect.build, suspect.sex, suspect.height, suspect.weight,
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         precinct, inside, location.housing, observation.period, officer.uniform,
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, radio.run, day, month, time.period)

# convert variable types as necessary
sqf <- sqf %>% mutate(suspect.race = as.factor(suspect.race), 
                      suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      location.housing = as.factor(location.housing),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct))

# filter to complete cases, then split into train and test sets
sqf <- sqf %>% filter(complete.cases(sqf))

# split training and test by year
train <- sqf %>% filter(year==2008|2010|2012|2014|2016)
test <- sqf %>% filter(year==2009|2011|2013|2015)

# separately standardize real-valued attributes for train/test sets
train <- train %>% mutate(suspect.height = standardize(suspect.height),
                          suspect.weight = standardize(suspect.weight),
                          suspect.age = standardize(suspect.age),
                          observation.period = standardize(observation.period))

test <- test %>% mutate(suspect.height = standardize(suspect.height),
                        suspect.weight = standardize(suspect.weight),
                        age = suspect.age,
                        suspect.age = standardize(suspect.age),
                        observation.period = standardize(observation.period))

# fit logistic model on the training set
# takes awhile to run
model <- glm(found.weapon ~ precinct + location.housing +  
               additional.report + additional.investigation + additional.sights +
               additional.proximity + additional.evasive + additional.associating +
               additional.direction + additional.highcrime + additional.time + 
               stopped.bc.object + stopped.bc.bulge + stopped.bc.desc + stopped.bc.violent +
               stopped.bc.casing + stopped.bc.lookout + stopped.bc.drugs + stopped.bc.clothing +
               stopped.bc.furtive + suspect.age + suspect.build + suspect.sex +
               suspect.height + suspect.weight + inside + observation.period +
               officer.uniform + radio.run + day + month + time.period, data=train, family = 'binomial')

# generate predictions for test set
test$predicted.probability <- predict(model, newdata = test, type='response') 

# output predictions in decreasing order
test %>% arrange(desc(predicted.probability)) %>% select(year, found.weapon,
                                                         predicted.probability)

# generate confusion matrices for a given threshold
threshold <- 0.5
test <- test %>% mutate(prediction = case_when(
  predicted.probability < threshold ~ F,
  predicted.probability >= threshold ~ T
))
table(test$prediction, test$found.weapon)

# output precision and recall for a threshold of 0.5
cat('At the threshold of ', threshold, ' the precision is ', 
    100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, prediction==T)),
    '%, and the recall is ', 100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, found.weapon==T)),
    '%\n')

# change the threshold from 0 to 1 in increments of 0.25
# to see what happens to precision and recall
# R code that checks this
for (threshold in c(seq(0,1,0.025))) {
  test <- test %>% mutate(prediction = case_when(
    predicted.probability < threshold ~ F,
    predicted.probability >= threshold ~ T
  ))
  
  cat('At the threshold of ', threshold, ' the precision is ', 
      100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, prediction==T)),
      '%, and the recall is ', 100*nrow(filter(test, prediction==T, found.weapon==T))/nrow(filter(test, found.weapon==T)),
      '%\n')
}

# compute AUC 
test.pred <- prediction(test$predicted.probability, test$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 

# generate predictions for training set
#train$predicted.probability <- predict(model, newdata = train, type='response') 

# compute AUC on training set
#train.pred <- prediction(train$predicted.probability, train$found.weapon)
#train.perf <- performance(train.pred, "auc")
#cat('the auc score is ', 100*train.perf@y.values[[1]], "\n") 

# performance plot
plot.data <- test %>% arrange(desc(predicted.probability)) %>% 
  mutate(numstops = row_number(), percent.outcome = cumsum(found.weapon)/sum(found.weapon),
         stops = numstops/n()) %>% select(stops, percent.outcome)

theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of weapons recovered", limits=c(0, 1), labels=scales::percent)
p

# calibration plot
plot.data <- test %>% mutate(calibration = round(100*predicted.probability)) %>% 
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(found.weapon))

p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p

# make precinct calibration plot
plot.data <- test %>% group_by(precinct) %>% 
  summarize(model.estimate = mean(predicted.probability),
            numstops = n(),
            empirical.estimate = mean(found.weapon))

p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p