## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     getinverse <- function() inv
     setinverse <- function(m) inv <<- m
     list(get = get,
          set = set,
          getinverse = getinverse,
          setinverse = setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (is.null(inv)) {
        # Need to put some eror handling for cases where the inverse cannot
        # be calculated.
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
    }
    else {
        message("Retrieving cached inverse")
    }
    inv
}
