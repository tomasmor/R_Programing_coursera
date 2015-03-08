corr <- function(directory, threshold=0){
  df <- complete(directory)
  ids = df[df["nobs"] > threshold, ]$id
  new_cor = numeric()
  for (i in ids){
    filename <- paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                      ".csv", sep = "")
    file = read.csv(filename)
    dff = file[complete.cases(file), ]
    new_cor = c(new_cor, cor(dff$sulfate, dff$nitrate))
  }
  return(new_cor)
  }
  