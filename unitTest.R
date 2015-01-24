## Load the newly created Cached Matrix code
source("cachematrix.R")

## This file contains some known unit Tests to ensure 
## that the functions are working as they should

## Unit Test 1
testMatrix <- matrix(c(3,1,2,1),nrow=2,ncol=2)
testCache <- makeCacheMatrix(testMatrix)
testMatrix
cacheSolve(testCache)

## Unit Test 2
testMatrix <- stats::rnorm(16)
dim(testMatrix) <- c(4,4)
testCache <- makeCacheMatrix(testMatrix)
testMatrix
cacheSolve(testCache)

## Unit Test 3
testMatrix <- c(7,3,-2,5)
dim(testMatrix) <- c(2,2)
testMatrix
testCache <- makeCacheMatrix(testMatrix)
cacheSolve(testCache)

## Unit Test 4
testMatrix <- matrix(c(2,3,5,7,11,13,17,19,23), 3, 3)
testCache <- makeCacheMatrix(testMatrix)
testMatrix
cacheSolve(testCache)


## Unit Test 5
testMatrix <- matrix(1:4,2,2)
testCache <- makeCacheMatrix(testMatrix)
testMatrix
cacheSolve(testCache)
