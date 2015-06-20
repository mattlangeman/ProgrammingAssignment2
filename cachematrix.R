## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Store inverse of x as part of object
    inverse_x <- NULL
    
    ## setter function for setting intital matrix
    set <- function(y) {
        x <<- y
        ## New matrix means inverse must be recalculated
        inverse_x <<- NULL
    }
    ## getter function for getting intial matrix
    get <- function() x
    
    ## setter function for getting matrix inverse
    setinverse <- function(inverse) inverse_x <<- inverse
    
    ## getter function for getting matrix inverse
    getinverse <- function() inverse_x
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    ## Try to get inverse from cache
    if (! is.null(inverse_x)) {
        message("Got inverse from cache")
        return(inverse_x)
    }
    ## Inverse not in cache, calculate with solve
    data <- x$get()
    inverse_x <- solve(data, ...)
    ## Set inverse in cache
    x$setinverse(inverse_x)
    inverse_x
}