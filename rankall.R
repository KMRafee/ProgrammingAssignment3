rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30 day death rate
        ## Read outcome of measures data
        healthdata  <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        statedata   <- healthdata[, 7]
        outcomedata <- c("heart attack", "heart failure", "pneumonia")
        NotNA       <- c("Not Available")
        cnames      <- c("hospital", "state","Rate", "Rank", "Last")
        
 
        ## To check outcome function input is valid
        if ((outcome %in% outcomedata) == FALSE) {
                stop(print("invalid input for outcome"))
                
        }
        
        ## To select health outcome data for the state
        ## healthdataforstate <- subset(healthdata,State==state)
        
        ## To select health data for the given outcome 
        if (outcome == "heart attack") {
                reqcolumn   <- c(2,7,11)
                hlthoutcome <- subset(healthdata[, reqcolumn])
                
        } else if (outcome == "heart failure") {
                reqcolumn   <- c(2,7,17)
                hlthoutcome <- subset(healthdata[, reqcolumn])
                
        } else {
                reqcolumn   <- c(2,7,23)
                hlthoutcome <- subset(healthdata[, reqcolumn])
                
        }
        ## to remove Not available from the health output
        hlthoutcomeNA       <- subset(hlthoutcome, hlthoutcome[, 3] != NotNA)
        hlthoutcomeNA[, 3]  <- as.numeric(hlthoutcomeNA[, 3]) 
        
        ## to do sort the data in ascending order
        aschltoutcomeNA     <- hlthoutcomeNA[order(hlthoutcomeNA[, 2],hlthoutcomeNA[,3],hlthoutcomeNA[,1] ),]        
        totrows             <- nrow(aschltoutcomeNA)
        
        ## Adding Rank to the dataframe. The last element is to hold whehter 
        ## hospital is last in the ranking for a state. If Last, the particular
        ## hospital will be displayed when input category is "worst"
        
        aschltoutcomeNA     <- data.frame(aschltoutcomeNA, 1:totrows, 1)
 
        for (i in 1:totrows) {
                if (i == 1) {
                        
                        rankcount            <- 1
                        aschltoutcomeNA[1,4] <- rankcount
                        prevstate            <- aschltoutcomeNA[1,2] 
                } else {
                        if (prevstate == aschltoutcomeNA[i,2]) {
                                rankcount            <- rankcount+1
                                aschltoutcomeNA[i,4] <- rankcount
                                prevstate            <- aschltoutcomeNA[i,2]
                        } else {
                        
                                aschltoutcomeNA[i-1,5]<- 2
                                rankcount            <- 1
                                aschltoutcomeNA[i,4] <- rankcount
                                prevstate            <- aschltoutcomeNA[i,2]
                        }
                } 
                if (i == totrows) {
                        aschltoutcomeNA[i,5] <- 2
                }
        
                
        }
        colnames(aschltoutcomeNA) <- cnames
        newcol                    <- c(1,2,4,5)
        rankalldata               <- subset(aschltoutcomeNA[, newcol])
        ##print(rankalldata)
        
        ## logic to identify the rank of hospital that needs to displayed
        if (num == "best") {
                rowno <- 1
                
        } else if (num == "worst") {
                rowno <- 0
                
        } else if (as.numeric(num) > totrows) {
                stop(print("Invalid input for rank"))
        } else {
                rowno <- as.numeric(num)
        }
        
        ## To input is worst, to print the last hospital in the sequence for a state
        ## else to print all our hospitals
        if (rowno == 0 ) {
                rankallfinal              <- subset(rankalldata,rankalldata[, 4] == 2 )

        } else {
                rankallfinal              <- subset(rankalldata,rankalldata[, 3]==rowno)        
        }
                
        result <- rankallfinal
        ##print(rankallfinal[, 1:2])        
        
        
}        