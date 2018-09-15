rankhospital <- function(state, outcome, num = "best") {
        
        if (num=="best"){
                return(best(state,outcome))
        }
        
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
        
        ## Return hospital name in that state with the given rank
        ifelse(outcome == "heart attack",
               outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               ifelse(outcome == "heart failure",
                      outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      ifelse(outcome == "pneumonia",
                             outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"      
                      )
               )
        )
        
        subData <- dat[dat$State==state,c("State","Hospital.Name",outName)]
        subData[,3] <- as.numeric(subData[,3])
        subData <- subData[!is.na(subData[outName]),]
        
        if(num == "worst"){
                bad <- max(subData[,3])
                result <- subData[subData[,3]==bad,c("Hospital.Name")]
                result <- sort(result)
                return(result[1])
        }
        
        if(!is.numeric(num)){
            print("Third param 'num' not is numeric")
                stop()
        }
        
        if(length(subData[,3])<as.numeric(num)){
                return(NA)
                stop()
        }
        
        ## 30-day death rate
        result <- subData[order(subData[,3],subData[,2]),2]
        return(result[num])
}