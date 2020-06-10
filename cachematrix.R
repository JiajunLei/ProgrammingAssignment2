## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "vector", which is a list containing 
## a function to
##set the value of the matrix
##get the value of the matrix
##set the inverse of the mean
##get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y=matrix()) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
   
}


## Write a short comment describing this function

##The cacheSolve function calculates the inverse of the special "vector" created with the above function.
##it first checks to see if the matrix has already been calculated. If so, it gets the inverse from the 
##cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the inverse
## of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
