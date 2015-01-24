## Matrix inversion is usually a costly computation
## so this file contains two functions that enable 
## the user of the function to cache the results and
## Invoke the cached results at a later time

## This function creates a special "matrix" object that can cache its inverse.
## This is usually accomplished by exposing 4 publicly accessible accessor functions
## Controlling access via the getter & setter functions ensures that cached
## values are retrieved only when the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) matInverse <<- inv
  getInverse <- function() matInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## Returns the getter & setter functions so they can be used later.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.

cacheSolve <- function(x, ...) {
  matInverse <- x$getInverse()
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  data <- x$get()
  matInverse <- solve(data, ...)
  x$setInverse(matInverse)
  matInverse ## Return a matrix that is the inverse of 'x'
  
}

