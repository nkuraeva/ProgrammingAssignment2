## In this example we introduce the <<- operator which can be used to assign a 
## value to an object in an environment that is different from the current
## environment. Below are two functions that are used to create a special object
## that stores a numeric Matrix and caches its mean.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #calculate the inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the mean of the special "matrix" created 
## with the above function. However, it first checks to see if the mean has 
## already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
