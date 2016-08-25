pollutantmean <- function(directory = "", pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  ##
  ## Get the data out of the files.
  ##
  
  ## Pad id with zeros and append file extension to create file names.
  pollutant.file.names <- paste(sprintf("%03d", id), ".csv", sep = "")
  
  # Create system independent relative file path.
  pollutant.file.paths <- file.path(directory, pollutant.file.names, fsep = .Platform$file.sep)

  # Read the files into a list of data frames.
  pollutant.data.list <- lapply(pollutant.file.paths, read.csv)
  
  # Combine data into a single data frame.
  pollutant.data.frame <- Reduce(rbind, pollutant.data.list)

  # Calculate and return the mean given the specified arguments.
  mean(pollutant.data.frame[[pollutant]], na.rm = TRUE)
}