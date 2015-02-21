## Programming Assignment 2
## Description of functions: two functions that cache the inverse of a matrix (benefits: reducing computation cost)
## Assumption: matrix supplied are always invertible.

## Function 1:     makeCacheMatrix
## Function that creates a list that contains 4 member functions: set, get, setInv and getInv. 

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL 
        # Set function: set a matrix to object created by makeCacheMatrix function
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        # Get function: returns the input matrix
        get <- function() x 
        # Set function: set the inversed matrix
        setInv <- function(inv) xinv <<- inv
        # Get function: returns the inversed matrix
        getInv <- function() xinv 
        # Return a list that contains functions previously defined
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Function 2:     cacheSolve
## Function that computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed, the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        mat <- x$getInv() 
        if(!is.null(mat)) {
                message("Getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data)        # "data" is a square invertible matrix so solve(data) returns its inverse
        x$setInv(mat)
        mat
}