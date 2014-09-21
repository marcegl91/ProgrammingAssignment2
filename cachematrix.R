## Create a matrix, and compute the inverse of it, then cache the
## result of that computation.


## This function creates a list of functions to handle a matrix,
## with methods to set/get a given matrix and to set/get the matrix
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## At the beginning set the matrix inverse to NULL
  i <- NULL
  
  ## Function to set a matrix to makeCacheMatrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Function to get the matrix stored in makeCacheMatrix
  get <- function() x
  
  ## Function to set the inverse of the matrix stored in makeCacheMatrix
  setinverse <- function(inverse) i <<- inverse
  
  ## Function to get the inverse of the matrix stored in makeCacheMatrix
  getinverse <- function() i
  
  ## List of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of an object created by makeCacheMatrix,
## stores it into a 'cache' and returns it.
cacheSolve <- function(x, ...) {
  
  ## Try to obtain the previously computed (and cached) matrix inverse
  ## (if it exists). In that case, return it and finish the function
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Computes the matrix inverse, stores it and returns it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
}
