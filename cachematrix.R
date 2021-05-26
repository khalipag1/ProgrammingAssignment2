##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##set the value of the matrix
##get the value of the matrix
##set the value of the Inverse
##get the value of the Inverse

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

## function CacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  i_m <- x$getInverse()
  if(!is.null(i_m)) {
    message("getting cached data")
    return(i_m)
  }
  data <- x$get()
  i_m <- solve(data, ...)
  x$setInverse(i_m)
  i_m
}

