## Put comments here that give an overall description of what your
## functions do


## The following function creates a "martix" object that caches its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x) {
    matrix <<- x;
    inverse <<- NULL;
  }
  func <- function() return (matrix);
  inv.set <- function(inv) inverse <<- inv;
  inv.get <- function() return (inverse);
  return (list(set = set, func = func, inv.set = inv.set, inv.get = inv.get))
}


## The following function computes the inverse matrix of the given "matrix"
## returned by the function "makeCacheMatrix" above. The "cacheSolve" function should 
## retrieve the inverse from the cache as long as the inverse matrix is being calculated.

cacheSolve <- function(x, ...) {
  inverse <- matrix$inv.get()
  if(!is.null (inverse)) {
    message ("Getting cached data...")
    return (inverse)
  }
  dat <- matrix$inv.get()
  inverse <- solve(dat, ...)
  matrix$inv.set (inverse)
  ## Return a matrix that is the inverse of 'x'
  return (inverse)
}

