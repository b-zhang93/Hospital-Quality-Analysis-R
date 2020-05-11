#function to find the best hospital in a state (lowest mortality rate)
##state is a 2 character abbrecviated state name
##outcome can be "heart attack", "heart failure", or "pneumonia"

##hospitals in dataset are all unique
##if you run length(unique(outcome[,2])) where outcome is the dataset you will get 4706 obs
##this means that all hospitals in the dataset are unique each row and we won't need to group them


best <- function(state, outcome) {
    #read outcome data and selects only columns we need for faster analysis
    bestdata <- read.csv("data/outcome-of-care-measures.csv")[, c(2, 7, 11, 17, 23)]
    
    #checks that state and outcome are valid
    if(match(state, unique(bestdata[,2]), nomatch = 0) == 0) {
        stop("invalid state")
    }
    else if(match(outcome, c("heart attack", "heart failure", "pneumonia"), nomatch = 0) == 0){
        stop("invalid outcome")
    }
    else {
        
        #assigns each outcome a value based on column index
        x <- if(outcome == "heart attack"){
            3
        } else if(outcome == "heart failure"){
            4
        } else {
            5
        }
        
        #creates a data frame with hospitals and their lowest average 30-day mortality rate
        data <- bestdata[bestdata$State == state, c(1,2,x)]
        names(data) <- c("hospital", "ST", "outcome")
        
        #removing NAs and converting outcome column from character to numeric class
        data[,"outcome"] <- as.numeric(data[,"outcome"])
        data <- na.omit(data)
        
        #find the hospital with the lowest rate
        minimum <- min(data$outcome, na.rm = TRUE)
        low <- data[data$outcome == minimum, ]
        low <- low[order(low$hospital), ][1,1]
    }
    #return hostpital name in the state with lowest 30-day death rate
    low
}
