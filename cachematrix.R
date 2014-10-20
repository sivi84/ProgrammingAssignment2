##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x=matrix()){
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  ## function to memorize the inverse matrix
  setcache <- function(cache) inverted <<- cache
  ## function to retrieve the inverse matrix
  getcache <- function() inverted
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
    ## retrieving the cached inverse matrix
    inverted <- x$getcache()
    ## if is cached, take the cached one
    if(!is.null(inverted)) {
      message("getting cached inverse matrix")
      return(inverted)
    }
    ## otherwise solve it
    data <- x$get()
    inverted <- solve(data, ...)
    ## and set the inverse matrix
    x$setcache(inverted)
    inverted
  }
