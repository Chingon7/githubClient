rankall <- function(outcome, num = "best") {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        
        if( outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia" ){
                print("invalid outcome")
                stop()
        }
        
        ifelse(outcome == "heart attack",
               outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               ifelse(outcome == "heart failure",
                      outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      ifelse(outcome == "pneumonia",
                             outName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"      
                      )
               )
        )
        
        
        subData <- dat[,c("State","Hospital.Name",outName)]
        subData[,3] <- suppressWarnings(as.numeric(subData[,3]))
        subData <- subData[!is.na(subData[outName]),]
        colnames(subData) <- c("state","hospital","deaths")
      

        ## For each state, find the hospital of the given rank
        
        StateList <- split(subData, subData$state)
        result <- lapply(StateList, function (l,num){
                l <- l[order(l[,3],l[,2]),c(2,1)]
                
                if(num == "best"){
                        return(l[1,])
                }
                
                if(num == "worst"){
                        return(l[nrow(l),])
                }
                
                if(!is.numeric(num)){
                        print("Third param 'num' not is numeric")
                        stop()
                }
                
                if(nrow(l)<as.numeric(num)){
                        return(NA)
                        stop()
                }
                
                return(l[num,])
                
                
        },num)
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        return (as.data.frame(do.call(rbind, result)))

}