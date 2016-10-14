seperateByStore <- function(data, ID, column=NA) {
  
  if (is.na(column)) {
    return(data[data$STORE == ID,])
  } else {
    return(data[data$STORE == ID, column])
  }
  
}