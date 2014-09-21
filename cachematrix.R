## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
