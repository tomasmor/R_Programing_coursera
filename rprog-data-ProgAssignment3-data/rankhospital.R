rankhospital <- function(state, outcome, num="best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses= "character")
  column <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  state_data <- data[data$State == state, c("Hospital.Name", column)]
  if (nrow(state_data) == 0)
  {
    stop("invalid state")
  }
  state_data[,2] <- as.numeric(state_data[,2])
  ordered_state_data <- order(state_data[column], state_data$Hospital.Name, na.last=NA)
  if (num=="best"){
    as.character(state_data$Hospital.Name[ordered_state_data[1]])
  }
  else if (num=="worst"){
    tail_order_data <- tail(ordered_state_data, n=1)
    as.character(state_data$Hospital.Name[tail_order_data])
  }
  else if (is.numeric(num)){
    as.character(state_data$Hospital.Name[ordered_state_data[num]])
  } else {
    stop("invalid num")
  }
}