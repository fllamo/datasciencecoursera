complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    # locate the files
    files <- list.files(directory, full.names = TRUE)
    df <- data.frame()

    ## read the files into a list of data.frames
    if (!is.null(id) && length(id) > 0) {
        for (i in seq_along(id)) {
            x <- id[i]
            data <- read.csv(files[x], header = TRUE)
            df <- rbind(df, c(x,sum(complete.cases(data))))
        }

        colnames(df) <- c('id', 'nobs')
        print(df)
    }

}
