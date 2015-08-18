corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    corr <- numeric(0)

    # locate the files
    files <- list.files(directory, full.names = TRUE)

    ## read the files into a list of data.frames
    for (i in 1:332) {
        data <- na.omit(read.csv(files[i], header = TRUE))

        if (nrow(data) >= threshold) {
            correlation <- cor(data['sulfate'], data['nitrate'])

            if (!is.na(correlation)) {
                corr <-append(corr, correlation)
            }
        }
    }

    print(corr)
}
