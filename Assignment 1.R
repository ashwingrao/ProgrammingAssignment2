setwd("~/Downloads/Git/coursera/Assignment 1/")
dataset_url <- "http://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip", exdir = "specdata")

pollutantmeanFirstCut <- function(directory, pollutant, id = 1:332) {
  # 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  dat <- data.frame()                             #creates an empty data frame
 
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  for (i in id) {                                
    #loops through the files, rbinding them together 
   # print(i)
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[, pollutant]  #subsets the rows that match the 'day' argument
  mean(dat_subset, na.rm=TRUE)      #identifies the mean weight 
  #while stripping out the NAs
}


pollutantmean <- function(directory, pollutant, id = 1:332) {
  # 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  files_list <- list.files(directory, full.names=TRUE)   # creates a list of files
  #print (files_list)
  
  tmp <- lapply(files_list[id], read.csv) # apply Read.csv to all items in the list of files 
 # print (tmp)
  output <- do.call(rbind, tmp) # Combine the data into one massive data frame using rbind
  mean(output[, pollutant], na.rm=TRUE) # Take the mean of the specific pollutant and return the value (last item)
}


completebad <- function(directory, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files_list <- list.files(directory, full.names=TRUE)   # creates a list of files
  tmp <- data.frame()
  
  tmp <- lapply(files_list[id], read.csv) # apply Read.csv to all items in the list of files 
  
  for(ii in id){
    cells[ii] <- sum(sapply(tmp[[ii]],  complete.cases))
  }
  cnames <- c("id", "nobs")
  ret_val <- matrix(cells, nrow=2, ncol=2, byrow=TRUE)
  complete_cases = tapply(tmp, complete.cases)
  nobs = lapply(tmp[complete_cases], sum)
  id = complete_cases[nobs]
  
}

complete <- function(directory, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  dat <- data.frame()                             #creates an empty data frame
   
  for (i in id) {
    dat <- rbind(dat, c(i, sum(complete.cases(read.csv(files_list[i])))))
   
  
    #loops through the files, rbinding them together 
    #print(i)
    #dat <- rbind(dat, read.csv(files_list[i]))
    #complete_dat <- complete.cases(dat[i])
    #sum(complete_dat)
  }
  names(dat) <- c("id","nobs")
  dat
  
}

source("pollutantmean.R")
pollutantmean(directory = "specdata", pollutant = "nitrate")
#pollutantmeanBetter(directory = "specdata", pollutant = "nitrate")
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
#pollutantmeanFirstCut("specdata", "nitrate", 23)
pollutantmean("specdata", "nitrate", 23)

source("complete.R")
#complete("specdata")
complete("specdata", 1)
completeNew("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
completeNew("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
completeNew("specdata", 30:25)
complete("specdata", 3)
completeNew("specdata", 3)
#completeNew("specdata")
complete("specdata", c(2, 4, 8, 10, 12))






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
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
head(cr)
summary(cr)


cr <- corr("specdata")
head(cr)
summary(cr)

