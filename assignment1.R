source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")
source("pollutantmean.R")
source("complete.R")
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

complete <- function(directory, id = 1:332){
  result<-vector()
  for (i in id){
    if(file.exists(directory)){
      index <- str_pad(as.character(i), 3, pad="0")
      data <- file.path(directory, paste(index, ".csv", sep=""))
      x <- read.csv(data)
      qtde <- nrow(na.omit(x))
      result<- c(result, c(i,qtde))
    }
    else{
      next
    }
  }
  final <- matrix(result, ncol=2, byrow=TRUE)
  colnames(final) <- c("id", "nobs")
  return(final)
}

corr <- function(directory, threshold = 0){
  sulfate <- vector()
  nitrate <- vector()
  monitors <- complete(directory)
  selection <- subset(monitors, monitors[, "nobs"] >= threshold)
  for (i in selection[,"id"]){
    if(file.exists(directory)){
      index <- str_pad(as.character(i), 3, pad="0")
      data <- file.path(directory, paste(index, ".csv", sep=""))
      x <- read.csv(data)
      new_x <- na.omit(x)
      sulfate <- c(sulfate, new_x[,"sulfate"])
      nitrate <- c(nitrate, new_x[,"nitrate"])
    }
    else{
      next
    }
  }
  print(sulfate)
  print(nitrate)
  return(cor(sulfate, nitrate, use="complete.obs"))
}

?cor

pollutantmean("specdata", "sulfate",1:10)
complete("specdata",4:21)


cases <- complete("D:/Users/montemurro-g/datasciencecoursera/airpollution")
cases 
selection <- subset(cases, cases[, "nobs"] > 500)
cr <- corr("specdata", 150)
head(cr)


cor

