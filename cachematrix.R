## These functions cache the inverse of a matrix

## The makeCacheMatrix function creates a matrix object that
## caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  mtrx <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(mtrx = mtrx, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function returns the inverse of the matrix produced
## by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
