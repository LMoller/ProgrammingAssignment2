## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse. It sets the value of the matrix,
## gets the value of the matrix, sets the value of the inverse and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }                                                 
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function checks to see if the inverse of the matrix has been set. If so, it gets the inverse from the cache.
## Otherwise, it inverts the matrix and sets the value in the cache.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
