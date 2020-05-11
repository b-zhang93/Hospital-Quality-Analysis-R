##explore outcome-of-care-measures.csv and plots histogram of the 30-day mortality rates for 
##heart attacks

#read csv
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#assign numeric class to column 11 
outcome[, 11] <- as.numeric(outcome[, 11])

#plots histogram for 30 day mortality rates for heart attacks
hist(outcome[, 11])