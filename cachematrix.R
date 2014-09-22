## This script contains functions that cache the inverse of a matrix rather than compute it each time

## This function computes a special matrix using the passed function to setmatrixinv 
makeCacheMatrix <- function(x = matrix()) {
  matx <- NULL
  set <- function(y){
    x <<- y
    matx <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) matx <<- solve
  getmatrixinv <- function() matx
  list(set=set, get=get,
       setmatrixinv=setmatrixinv,
       getmatrixinv=getmatrixinv)
}


## This function computes the inverse matrix of makeCacheMatrix
## using solve and return it, or return the cached version if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getmatrixinv()
  if(!is.null(matinv)){
    message("getting cached data")
    return(matinv)
  }
  matrix <- x$get()
  matinv <- solve(matrix, ...)
  x$setmatrixinv(matinv)
  matinv
}
