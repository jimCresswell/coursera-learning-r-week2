corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Get data from files.
  paths <- file.path(directory, list.files(directory, pattern="*.csv"), fsep = .Platform$file.sep)

  output <- vector(mode="numeric")

  for (path in paths) {
    measurements = read.csv(path)
    count <- length(which(complete.cases(measurements)))
    if (count >= threshold) {
      output <- c(output, cor(measurements$sulfate, measurements$nitrate, use = "na.or.complete"))
    }
  }

  output
}