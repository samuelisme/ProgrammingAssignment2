
##Coursera R Programming
##Assignment 2
##Samuel Chan (samuelisme)
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a vector - storing 4 functions to
## set/ get the value of the matrix, and set/get the inverse of 
## the matrix
makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL
  set <- function(y) {
    x <<- y
    ma <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) ma <<- solve
  getsolve <- function() ma
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## Write a short comment describing this function

## This function takes the vector created by makeCacheMatrix().
## It checks the cache - if the cache has already contained the 
## inverse, it returns the inverse matrix from cache (i.e. skip
## computation), otherwise it calculates the inverse of the matrix
## , set this in cache and return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ma <- x$getsolve()
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }
  data <- x$get()
  ma <- solve(data, ...)
  x$setsolve(ma)
  ma
}
