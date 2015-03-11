library(stringr)

complete <- function(directory, id = 1:332){
  final<-data.frame()
  for (i in id){
    if(file.exists(directory)){
      index <- str_pad(as.character(i), 3, pad="0")
      data <- file.path(directory, paste(index, ".csv", sep=""))
      x <- read.csv(data)
      qtde <- nrow(na.omit(x))
      final <- rbind(final, c(i, qtde))
    }
    else{
      next
    }
  }
  #final <- matrix(result, ncol=2, byrow=TRUE)
  colnames(final) <- c("id", "nobs")
  return(final)
}