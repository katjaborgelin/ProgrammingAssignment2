## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix takes a matrix as a parameter, stores it to variable x
## and defines and returns three functions get, inverse and getinverse.
## Function get returns variable x. 
## Function inverse makes an inverse matrix and stores it to m which is located in parent environment.
## Function getinverse return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  inverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(get=get,
       inverse=inverse,
       getinverse=getinverse)
}

## Function cacheSolve takes a matrix as a parameter and returns an inverse matrix.
## If cached matrix exists, it is returned.
## If it doesn't exist, it is calculated using solve function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$inverse(m)
  message("storing cached data")
  m
}
