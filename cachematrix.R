## Install the Package if needed
#install.packages("matlib")
library(matlib)

## This function creates a special "matrix" object that can cache its inverse
## The function takes the matrix and returns a list with functions to 
## get and set the matrix and get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Create the values for set and get for the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  ## Create the values for setinv and getinv for the matrix inverse
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  
  ## Add the values to the list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  
  ## Checks to see if the inverse has already been calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## If inverse has not been calculated, then use the solve function to get 
  ## the result from the matrix
  data <- x$get()
  inverse <- solve(data, ...)
  
  ## This takes the result of solve function and saves to the cache
  x$setinv(inverse)
  return(inverse)  
}



