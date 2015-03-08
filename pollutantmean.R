pollutantmean <- function(directory, pollutant, id = 1:332) {
  data = numeric()
  for (i in id) {
    filename <- paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                      ".csv", sep = "")
    read_file = read.csv(filename)
    
    data = c(data, read_file[[pollutant]])
  }
  return(mean(data, na.rm = TRUE))
}
