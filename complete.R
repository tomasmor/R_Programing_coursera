complete <- function(directory, id=1:332) {
  nobs = numeric()
  for (i in id) {
    filename <- paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                      ".csv", sep = "")
    file = read.csv(filename)
    nobs = c(nobs, sum(complete.cases(file)))
  }
  return(data.frame(id, nobs))
}

