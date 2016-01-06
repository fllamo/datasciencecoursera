rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    measures <- read.csv('outcome-of-care-measures.csv', colClasses = 'character', na.strings = c("NA", "Not Available"))

    ## Check that state and outcome are valid
    if (!any(state == state.abb)) {
        stop('invalid state')
    }

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

    data.state <- measures[measures$State == state, ]

    ## convert to numeric
    data.state[, i] <- as.numeric(x=data.state[, i])

    data.state <- data.state[complete.cases(data.state), ]

    data.state <- data.state[order(data.state[,i], data.state[,2]),]


    if (num == 'best') {
        r <- 1
    }
    else if (num == 'worst') {
        r <- nrow(data.state)
    }
    else
        r <- as.numeric(num)

    data.state[r,2]
}
