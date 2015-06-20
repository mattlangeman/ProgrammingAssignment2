## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverse_x <<- inverse
    get_inverse <- function() inverse_x
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x = x$get_inverse()
    if (! is.null(inverse_x)) {
        message("getting cached data")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data, ...)
    x$set_inverse(inverse_x)
    inverse_x
}