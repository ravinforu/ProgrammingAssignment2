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
  setmatrix <- function(solve) matx <<- solve
  getmatrix <- function() matx
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinverse()
  if(!is.null(matinv)){
    message("getting cached data")
    return(matinv)
  }
  matrix <- x$get()
  matinv <- solve(matrix, ...)
  x$setmatrix(matinv)
  matinv
}
