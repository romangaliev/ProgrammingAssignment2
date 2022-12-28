## A pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # is a temporary local variable here
  set <- function(y) {
    x <<- y    # simply sets received matrix into cache
    m <<- NULL # sets m to NULL
  }
  get <- function() x # simply returns x
  setInverse <- function(solve) m <<- solve # solve is a local variable here
  getInverse <- function() m # simply returns m which is just setInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {  # i.e. if getInverse() was successfull
    message("getting cached inverse x")
    return(m)
  }
  # if the getInverse() above was NOT successful then do 4 steps:
  # 1. get the matrix
  data <- x$get() # data is a temporary local variable
  # 2. inverse the matrix
  m <- solve(data, ...) # returns  Inverse of x
  # 3. set inverse into cache
  x$setInverse(m)
  # 4. return inversed matrix
  m
}
