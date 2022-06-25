## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and defines the get and set functions for that matrix for getting and setting the inverse
## of the matrix and caching the result

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieves the cached inverse of the matrix if it exists and returns it
## If the inverse does not exist in the cache, the value is computed, stored in the cache for future use,
## and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
