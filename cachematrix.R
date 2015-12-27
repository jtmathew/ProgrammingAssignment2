##These functions create a matrix object that can cache the inverse of the matrix supplied.
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Set the Value of the matrix.
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  ## Get the value of the matrix.
  get <- function () x
  ##Set the value of the inverse.
  setInv <- function(inverse){
    i <<- inverse
  }
  ##Get the value of the inverse.
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  ##Check for cached inverse matrix
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##SOlve Inverse Matrix and store in cache
  mx <- x$get()
  i <- solve(mx, ...)
  x$setInv(i)
  i
}
