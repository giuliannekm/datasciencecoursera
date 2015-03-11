library(stringr)

corr <- function(directory, threshold = 0){
  monitors <- complete(directory)
  correlation <- vector()
  selection <- subset(monitors, monitors[, "nobs"] >= threshold)
  for (i in selection[,"id"]){
    if(file.exists(directory)){
      index <- str_pad(as.character(i), 3, pad="0")
      data <- file.path(directory, paste(index, ".csv", sep=""))
      x <- read.csv(data)
      new_x <- x[complete.cases(x), ]
      correlation <- c(correlation, cor(new_x$sulfate, new_x$nitrate))
    }
    else{
      next
    }
  }
  return(correlation)
}