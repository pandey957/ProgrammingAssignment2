#Author Satyendra Raj Pandey
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <-function(x, ...)
{
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
