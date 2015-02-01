corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
  files_list <- list.files(directory, full.names=TRUE)   # creates a list of files
  #print (files_list)
  id <- 1:length(files_list)
  
  comp <- complete(directory, id)
  ret <- numeric()
  
  tmp <- lapply(files_list[id], read.csv) # apply Read.csv to all items in the list of files 
  for (i in 1:length(id)) {
    # ret[i] <- 0
    if(comp[[2]][[i]] >= threshold) {
      ret <- c(ret, cor(tmp[[i]]$nitrate,tmp[[i]]$sulfate, use="na.or.complete"))
    }
    
  }
  
  ret
}





