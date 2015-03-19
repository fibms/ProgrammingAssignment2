#
# Contains functions to calculate the inverse of an invertible square matrix.
#
# - makeCacheMatrix: This function creates a special "matrix" object that can 
#   cache its inverse.
# - cacheSolve: This function computes the inverse of the special "matrix" 
#   returned by makeCacheMatrix above. If the inverse has already been 
#   calculated (and the matrix has not changed), then it will retrieve the 
#   inverse from the cache.
#
# Example usage:
#   source("cachematrix.R")
#   m <- makeCacheMatrix(matrix(c(4,2,7,6), 2, 2))
#   inv <- cacheSolve(m)
#

#
# This function creates a special "matrix" to hold the value and its inverse. It
# is just a list of functions to 
# 1. get the value of the matrix
# 2. set the value of the matrix
# 3. get the inverse of the matrix
# 4. set the inverse for the matrix
#
# When a new value is set, the inverse of the previous value is cleared out.
#
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     getInverse <- function() inv
     setInverse <- function(m) inv <<- m
     list(get = get,
          set = set,
          get.inverse = getInverse,
          set.inverse = setInverse)
}

#
# This function takes an instance of the matrix created by makeCacheMatrix
# and calculates the inverse of the matrix. If the inverse for the matrix has
# already been calculated before and the matrix has not changed, the cached 
# value is returned.
#
# It is assumed that the matrix supplied is a square matrix and is always
# invertible. Hence, no error handling has been put in place.
#
cacheSolve <- function(x, ...) {
    inv <- x$get.inverse()
    if (is.null(inv)) {
        # Inverse has not been calculated for this matrix.
        # Calculate the inverse and cache the value.
        inv <- solve(x$get(), ...)
        x$set.inverse(inv)
    }
    inv
}
