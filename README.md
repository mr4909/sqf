# Stop, Question, and Frisk

Examine all complete cases of suspicion of a “criminal possession of weapon” (CPW), and predict the probability that a weapon will be found.  

# Repository Structure 

`sqf_analysis.R`: Builds found.weapon classifier with logistic regression, generates calibration plots.  
`sqf_import.R`: Imports, combines, cleans, and standardizes all years, and writes out one csv file stop-and-frisk csv data for years 2008-2016.  
`sqf_library.R`: Runs packages and custom functions needed in sqf_import.R, sqf_analysis.R, and sqf_plots.R.  
`sqf_plots.R`: Creates data visualizations.    

`codebook.pdf`: Information on the structure, contents, and layout of files.  

# Data

[SQF Data](https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page)

`arrest.offenses.tsv`: List of arrest reasons and their digit codes.  

`offense-codes.tsv`: List of offenses and their digit codes.   



