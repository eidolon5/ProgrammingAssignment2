# R Programming Assignment 2
# Description: 
# makeCacheMatrix: creates an object that can cache the inverse of a matrix
# cacheSolve: computes the inverse of the object from makeCacheMatrix. If the 
# inverse was previously computed, cacheSolve display the message "getting cached
# data" and will skip the computation. If the inverse was not previously computed,
# cacheSolve will calculate the inverse. That should be it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  #m <- NULL
  #set <- function(y) {
  #  x <<- y
  #  m <<- NULL
  #}
  #get <- function() x
  #setmean <- function(mean) m <<- mean
  #getmean <- function() m
  #list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  #print(m)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  #m <- x$getmean()
  #if(!is.null(m)) {
  #  message("getting cached data")
  #  return(m)
  #}
  #data <- x$get()
  #m <- mean(data, ...)
  #x$setmean(m)
  #m
}
