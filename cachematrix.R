
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create an object to stores inverse matrix.

## The function below just creates an object which can cache inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
 
   nv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
   


## This function computes the inverse of "matrix" created by makeCacheMatrix
## If the inverse has already been created then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

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