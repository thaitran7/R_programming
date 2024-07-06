# These functions are used to create a special "matrix" object that can cache its inverse.
# The makeCacheMatrix function creates this special "matrix" object, and the cacheSolve 
# function computes the inverse of the matrix. If the inverse has already been calculated 
# and the matrix has not changed, then cacheSolve retrieves the inverse from the cache.

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix 
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
