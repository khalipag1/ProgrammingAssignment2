## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x =matrix()) {
  i_m <- NULL
  set <- function(y) {
    x <<- y
    i_m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i_m <<- solve
  getInverse <- function() i_m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  
  i_m <- x$getInverse()
  if(!is.null(i_m)) {
    message("getting cached data")
    return(i_m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(i_m)
  i_m
}
