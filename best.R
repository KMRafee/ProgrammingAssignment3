best <- function(state, outcome) {
        
        ## Read outcome of measures data
        healthdata  <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        statedata   <- healthdata[, 7]
        outcomedata <- c("heart attack", "heart failure", "pneumonia")
        NotNA       <- c("Not Available")
        
        ## To check whether state and outcome function input are valid
        
        if ((state %in% statedata) == FALSE) {
                stop(print("Invalid input for State"))
                
        } else if ((outcome %in% outcomedata) == FALSE) {
                stop(print("invalid input for outcome"))
                
        }

        ## To select health outcome data for the state
        healthdataforstate <- subset(healthdata,State==state)
        
        ## To select health data for the given outcome 
        if (outcome == "heart attack") {
                reqcolumn   <- c(2,11)
                hlthoutcome <- subset(healthdataforstate[, reqcolumn])
           
        } else if (outcome == "heart failure") {
                reqcolumn   <- c(2,17)
                hlthoutcome <- subset(healthdataforstate[, reqcolumn])
           
        } else {
                reqcolumn   <- c(2,23)
                hlthoutcome <- subset(healthdataforstate[, reqcolumn])
          
        }
        ## to remove Not available from the health output
        hlthoutcomeNA       <- subset(hlthoutcome, hlthoutcome[, 2] != NotNA)
        hlthoutcomeNA[, 2]  <- as.numeric(hlthoutcomeNA[, 2]) 
        ## to do sort the data in ascending order
        aschltoutcomeNA     <- hlthoutcomeNA[order(hlthoutcomeNA[,2],hlthoutcomeNA[,1] ),]
        print(aschltoutcomeNA[1,1])
                
       
}