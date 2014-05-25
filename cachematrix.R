## Coursera R Programming Course
## Programming Assignment 2: Caching the Inverse of a Matrix
##
## The following functions provide a means to calculate the inverse
## of a matrix and cache its value for later use. Since calculating 
## the inverse of a matrix can be computationally expensive, caching
## the solution can be valuable for program efficiency.
##
## Example usage:
## mat <- makeCacheMatrix()
## mat$set(matrix(runif(100),ncol=10))
## result <- cacheSolve(mat)

## Cacheable matrix object constructor
## It is assumed that the passed matrix is invertable

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solution) inverse <<- solution
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Check cache for previous inversion of passed matrix.  
## If cached solution exists, return it. Otherwise 
## calculate matrix inverse and cache it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("retrieving cached solution ...")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}

