## Put comments here that give an overall description of what your
## functions do
## These functions allow a matrix inverse to be cached for faster access

## Write a short comment describing this function
## This function creates a "cache matrix" which is a list of 4 elements
## These elements are functions that allow the matrix to be used as normal
## With the added benefit of storing the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     matrixInverse <- NULL
    set <- function(y) {
            x <<- y
            matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function allows the use of the "cacheMatrix"
## It solves, stores, and returns the inverse if it doesn't already exist
## It grabs the inverse from memory if it does exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
