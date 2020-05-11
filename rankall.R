##rankall function outputs a hospital for every state based on the ranking and outcome inputs

rankall <- function(outcome, num = "best") {
    #read outcome data and selects only columns we need for faster analysis
    rankdata <- read.csv("data/outcome-of-care-measures.csv")[, c(2, 7, 11, 17, 23)]
    
    #validity check for outcome argument
    if(match(outcome, c("heart attack", "heart failure", "pneumonia"), nomatch = 0) == 0){
        stop("invalid outcome")
    } 
    
    #assigns each outcome a value based on column index
    x <- if(outcome == "heart attack"){
        3
    } else if(outcome == "heart failure"){
        4
    } else {
        5
    }
    
    #creates a data frame with hospitals and the corresponding 30 day mortality rates outcome
    data <- rankdata[ ,c(1,2,x)]
    names(data) <- c("hospital", "ST", "outcome")
    
    #removing NAs and converting outcome column from character to numeric class
    data[,"outcome"] <- as.numeric(data[,"outcome"])
    data <- na.omit(data)
    
    #orders data by state, outcome, and hospital name
    ranktable <- data[order(data$ST, data$outcome, data$hospital), ]

    ##split and sapply to group by state and using anonymous function within sapply to 
    ##also convert best/worst character inputs as indices
    splitdata <- split(ranktable, ranktable$ST)
    bystate <- sapply(splitdata, function(n) if(num == "best") n[1, 1] else if(num == "worst") n[nrow(n),1]
                      else n[num, 1])

    #return data frame with hospital based on selected rank and outcome for each state
    data.frame(hospital=bystate, state=names(bystate), row.names = names(bystate))
}