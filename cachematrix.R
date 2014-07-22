## cachematrix : cache time consuming matrix inversions
##  Ken Q : rprog-005 : programming assignment 2 : 7/21/2014

## A pair of R functions to cache the inverse of a matrix
##
## makeCacheMatrix
## cacheSolve
##
##
## Example:
##
## X <- makeCacheMatrix(diag(3))  # create X from a 3x3 identity matrix
##
## cacheSolve(X)
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
##
## The next call of cacheSolve(X) will return the cached value
##

## makeCacheMatrix takes an optional matrix as input and returns a cacheable version
##
##  This function returns a list of methods (set, get, setinverse, getinverse)
##  which can be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  # Reset inverse
  inverse <- NULL
  
  # set : set the matrix and reset inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get : get the matrix
  get <- function() {x}
  
  # setinverse : set the inverse
  setinverse <- function(inv) {inverse <<- inv}
  
  # getinverse : get the inverse
  getinverse <- function() {inverse}
  
  # return list of methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve : return a matrix that is the inverse of x
##
##  This function will either calculate the inverse or returned the cached 
##  value of inverse 

cacheSolve <- function(x, ...) {
  # First, check if cached value is valid
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    # Valid : just return cached value
    message("getting cached inverse")
    return(inverse)
  }
  
  # Calculate inverse matrix
  xmatrix <- x$get()
  inverse <- solve(xmatrix, ...)
  
  # Cache for next time
  x$setinverse(inverse)
  
  inverse  
}
