best <- function(state,outcome){
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        namesStates <- levels(factor(dat$State))
        a <- 0
        for (i in seq_along(namesStates)){
              if(state == namesStates[i]){
                      a <- 1
              }
        }
        
        if(a == 0){
                print("invalid state")
                stop()
        }
        
        if( outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia" ){
                print("invalid outcome")
                stop()
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ifelse(outcome == "heart attack",
               outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               ifelse(outcome == "heart failure",
                      outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      ifelse(outcome == "pneumonia",
                             outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"      
                             )
                        )
                )
        
        ## rate
        subData <- dat[dat$State==state,c("State","Hospital.Name",outName)]
        subData[,3] <- as.numeric(subData[,3])
        subData <- subData[!is.na(subData[outName]),]
        mini <- min(subData[,3])
        result <- subData[subData[,3]==mini,c("Hospital.Name")]
        result <- sort(result)
       
        return(result[1])
        
}