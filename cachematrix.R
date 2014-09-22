## This script contains functions that cache the inverse of a matrix rather than compute it each time

## This function computes a special matrix using the passed function to setmatrixinv 
makeCacheMatrix <- function(x = matrix()) {
  matx <- NULL
  set <- function(y){
    x <<- y
    ## Initializing the matrix to null before recalculating the inverse
    matx <<- NULL
  }
  
  ## Returns the matrix
  get <- function() x
  
  ## Set the cached inverse matrix
  setmatrixinv <- function(solve) matx <<- solve
  
  ## Get the cached inverse matrix
  getmatrixinv <- function() matx
  
  ## Put all the matrix and inverse matrix in a list and return a special matrix
  list(set=set, get=get,
       setmatrixinv=setmatrixinv,
       getmatrixinv=getmatrixinv)
}


## This function computes the inverse matrix of makeCacheMatrix
## using solve and return it, or return the cached version if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getmatrixinv()
  ## If inverse computation is already done, return the cached version
  if(!is.null(matinv)){
    message("getting cached data")
    return(matinv)
  }
  
  ## Calculating inverse incase a cached version is not available
  matrix <- x$get()
  
  ## Calculating inverse using solve
  matinv <- solve(matrix, ...)
  
  ## Set the calculated inverse inside data structure using setmatrixinv of makeCacheMatrix
  x$setmatrixinv(matinv)
  
  ## Return the inverse matrix
  matinv
}
