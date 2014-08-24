## This is a small library of functions that allows the solution of a matrix
## to be cached and reused during computation.
##
## Create a "cache matrix" using the makeCacheMatrix() function and then
## get or set its matrix value using the $get() and $set() functions.
##
## A specialized cacheSolve() function has been provided to store the solution
## of the matrix in the "cache matrix" itself or to retrieve a previously cached
## solution.  
##
## The cached solution will be cleared anytime the cache matrix value is altered
## with the $set() function.


# Create a special "matrix" object that is capable of caching its own inverse
#
# Arguments:
#       x       a matrix to wrap as a cache matrix, defaults to an empty matrix
#
# Returns:
#       A list with the named indices
#           $get    to retrieve the matrix
#           $set    to set the matrix and clear the cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Compute the inverse of a cache matrix or retrieve the cached solution if it 
# has been computed previously
#
# Arguments:
#       x       a cached matrix created with makeCacheMatrix()
#       ...     additional arguments to pass to the solve() function
#
# Returns:
#       the inverse of the matrix x or a cached solutions from a previous 
#       calculation

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    if(is.null(i)) {
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
    }
        
    i  
}
