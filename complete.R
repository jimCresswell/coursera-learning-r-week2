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
  
  ## Get the data into a data frame
  pollutant.count <- data.frame()
  
  # Single loop so ID is preserved.
  for(i in id) {
    this.file.name = paste(sprintf("%03d", i), ".csv", sep = "")
    this.file.path = file.path(directory, this.file.name, fsep = .Platform$file.sep)
    this.data = read.csv(this.file.path)
    this.count = length(which(!is.na(this.data$sulfate) & !is.na(this.data$nitrate)))
    pollutant.count <- rbind(pollutant.count, data.frame(id=i, nobs=this.count))
  }
  
  pollutant.count
}