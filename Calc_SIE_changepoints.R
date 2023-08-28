## Calc_SIE_changepoints.R
## Code to process sea ice extent data and identify changepoints
## Version: original version 26 January 2023. Final version 4 July 2023

options(scipen = 9999) # turn off scientific numbers

#################
### Load data ###
#################

setwd("~/OneDrive/R/")
seaice <- read.csv("Data/Seaice/SIE_monthly_timeseries_230704.csv", header = T)

seaice <- seaice[,c(1,2)]
colnames(seaice) <- c("Date","Anomaly")

seaice$Anomaly <- ifelse(seaice$Anomaly > 1000000, NA, seaice$Anomaly) # find missing values
seaice <- seaice[!is.na(seaice$Anomaly),] # remove missing values

seaice$rownum <- seq(1:length(seaice$Date))

#############################################
### Step change analysis 1 :: Changepoint ###
#############################################

library(changepoint)

si.cpt <- cpt.mean(seaice$Anomaly, penalty = "BIC",method = "PELT")  # fit cpt.mean
plot(si.cpt, cpt.col="blue")
print(si.cpt)
print(si.cpt@cpts) 

# output >> breakpoints at 346 = 2007.625 and 454 = 2016.625 

#############################################
### Step change analysis 2 :: Strucchange ###
#############################################

library(strucchange)

si.sc <- strucchange::breakpoints(Anomaly ~ 1, h = 0.15, data = seaice) # fit "breakpoints"
print(si.sc$breakpoints)
summary(si.sc)
plot(si.sc)

# output >> breakpoints at 178 = 1993.625, 349 = 2007.875 and 454 = 2016.625 
# output >> if forced to select two breakpoints, breakpoints at 346 = 2007.625 and 454 = 2016.625 

###############
### The end ###
###############
