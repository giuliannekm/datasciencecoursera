library(stringr)

pollutantmean <- function(directory, pollutant, id = 1:332){
  spec <- vector()
  for (i in id){
    if(file.exists(directory)){
      index <- str_pad(as.character(i), 3, pad="0")
      data <- file.path(directory, paste(index, ".csv", sep=""))
      x <- read.csv(data)
      spec <- c(spec, na.omit(x[,pollutant]))
    }
    else{
      next
    }
  }
  return(mean(spec))
}
