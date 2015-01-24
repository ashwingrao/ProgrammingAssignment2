setwd("/Users/indu/Documents/DropBox-AGR/Dropbox/Git/coursera/Assignment\ 2/ProgrammingAssignment2")
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


makeVector <- function(x = numeric()) {
  m <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  getevn<- function() environment()
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean,
       getevn = getevn)
}
x <- 1:10000

vec<-makeVector(x)

