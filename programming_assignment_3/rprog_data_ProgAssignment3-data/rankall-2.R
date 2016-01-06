rankall <- function(outcome, num="best") {
    ## Read outcome data
    measures <- read.csv('outcome-of-care-measures.csv', colClasses = 'character', na.strings = c("NA", "Not Available"))

    ## Check that outcome is valid
    if(outcome == 'heart attack') {
        i <- 11
    }
    else if(outcome == 'heart failure') {
        i <- 17
    }
    else if(outcome == 'pneumonia') {
        i <- 23
    }
    else {
        stop('invalid outcome')
    }

    #convert to numeric
    measures[, i] <- as.numeric(x=measures[, i])

    data.split <- split(measures[, c(2,7,i)], measures$State)

    ## data.split <- split(measures, measures$State)

    stateRank <- function(stateData, num) {
        #stateData <- stateData[complete.cases(stateData), ]
        ##hospitals <- stateData[order(stateData[,i], stateData[,2]),]
        hospitals <- order(stateData[1], stateData[2], na.last = NA)

        if (num == 'best') {
            r <- 1
        }
        else if (num == 'worst') {
            r <- nrow(stateData)
        }
        else
            r <- as.numeric(num)

        hospitals[r]
    }

    hospital <- lapply(data.split, stateRank, num)
    data.frame(hospital = unlist(hospital), state = names(hospital), row.names = (hospital$state))

}
