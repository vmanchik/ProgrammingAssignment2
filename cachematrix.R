## This function is a way to optimize computing of an inverse of matrix, which can be a costly operation.

## This first function crates a special "matrix" object and that can chase its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This second function takes in the matrix created by the first function above and takes its inverse.  
## If the inverse has already been calculated, the function retrieves its values from cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


## The code below tests the two functions above

test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()

test_matrix$getInverse()
cacheSolve(test_matrix)