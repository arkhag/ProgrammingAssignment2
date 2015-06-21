
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.

## In this assignment, we created two functions, "makeCacheMatrix" and "cacheSolve"

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## the above defines the variable m for inverse
  set <- function(y) {
      x <<- y
      m <<- NULL
    }
## the above sets teh value of the matrix function in the parental environment
    get <- function() x
## the above gets the value of the matrix
    setinverse <- function(inv) m <<- inverse
##the above sets the value of the inverse
    getinverse <- function() m
#the above gets the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
#the above creates a list.
  


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
## if inverse is cached, then the "getting cached data" message appears. 
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
## computes the inverse of a square matrix
  x$setinverse(m)
  return(m)
## returns the value of inverse
}
