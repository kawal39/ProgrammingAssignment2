## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## stores the cached value & initialized to null
   m <- NULL
   ## creates matrix in working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of matrix
  get <- function() x
  ## invert matrix and store in cache
  setsolve <- function(inverse) m <<- inverse
  ## get inverted matrix from cache
  getsolve <- function() m
  ## return created functions to working environment
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## attempt to get matrix from cache
  m <- x$getsolve()
  ## return inverted matrix from cache if it exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## create matrix
  data <- x$get()
  ## return inverse of matrix
  m <- solve(data, ...)
  ## set inverted matrix in cache
  x$setsolve(m)
  ## return inverted matrix
  m
}
