corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Get data from files.
  pollutant.file.names <- list.files(directory, pattern="*.csv")
  
  pollutant.file.paths <- file.path(directory, pollutant.file.names, fsep = .Platform$file.sep)
  # pollutant.file.paths <- file.path(directory, "001.csv", fsep = .Platform$file.sep)
  
  pollutant.data.list <- lapply(pollutant.file.paths, read.csv)
  
  # Filter data for a threshold of complete cases.
  # Note, zero complete cases can be allowed.
  pollutant.data.list.complete = Filter(function(measurements) {
    count <- length(which(complete.cases(measurements)))
    count >= threshold
  }, pollutant.data.list)
  
  # Calculate the correlation.
  Reduce(function(prev, curr) {
    c(prev, cor(curr$sulfate, curr$nitrate, use = "na.or.complete"))
  }, pollutant.data.list.complete, vector(mode="numeric"))
}