pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!

    # locate the files
    files <- list.files(directory, full.names = TRUE)
    data <- data.frame()

    ## read the files into a list of data.frames
    if (!is.null(id) && length(id) > 0) {
        for (i in seq_along(id)) {
            x <- id[i]
            data <- rbind(data, read.csv(files[x], header = TRUE))
        }
        mean(data[,pollutant], na.rm=T)
    }

}
