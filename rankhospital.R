##rank hospital returns a character vector with the hospital name corresponding to the rank inputted
##rank is inputted as the "num" argument in the function. This is similar to the previous question
##except we need to return a specific row. Thus we will just use the previous template with a few changes

rankhospital <- function(state, outcome, num = "best") {
    #read outcome data and selects only columns we need for faster analysis
    rankdata <- read.csv("data/outcome-of-care-measures.csv")[, c(2, 7, 11, 17, 23)]
    
    #checks that state and outcome are valid
    if(match(state, unique(rankdata[,2]), nomatch = 0) == 0) {
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
        
        #creates a data frame with hospitals and the corresponding 30 day mortality rates outcome
        data <- rankdata[rankdata$State == state, c(1,2,x)]
        names(data) <- c("hospital", "ST", "outcome")
        
        #removing NAs and converting outcome column from character to numeric class
        data[,"outcome"] <- as.numeric(data[,"outcome"])
        data <- na.omit(data)
        
        #ranks hospitals by lowest rate first and then alphabetically by name
        ranktable <- data[order(data$outcome, data$hospital), ]
        
        #outputs hospital name by rank and converts best / worst string inputs into indices
        if(num == "best"){
            num <- 1
        } else if(num == "worst"){
            num <- length(ranktable$outcome)
        }
        rankoutput <- ranktable[num, 1]
        
    }
    #return hostpital name based on the rank within selected state and based on selected outcome
    rankoutput
}