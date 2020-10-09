# Stop, Question, and Frisk


# Repository Structure 

`sqf_analysis.R`: Builds found.weapon classifier with logistic regression, generates calibration plots.  
`sqf_import.R`: Imports, combines, cleans, and standardizes all years, and writes out one csv file stop-and-frisk csv data for years 2008-2016.  
`sqf_library.R`: Runs packages and custom functions needed in `sqf__import.R`.  
`sqf_plots.R`: Creates data visualizations.    

# Data

[SQF Data](https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page)

`arrest.offenses.tsv`: List of arrestable offenses and their digit codes.  

`offense-codes.tsv`: List of offenses and their digit codes.   



