## With matrix inversion being a costly computation, it may be beneficial
## to cache the inverse of a matrix rather than compute it repeatedly

## Create the special matrix object that can cache its inverse
## Per the example, this function should do the following:
## - Set the matrix
## - Get the matrix
## - Set the inverse
## - Get the inverse
makeCacheMatrix <- function(x = matrix()) {
    ##Create variable to hold the inverse matrix
    i <- NULL
    
    ##Set function for matrix, reset inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ##Get the matrix
    get <- function() x
    
    ##Set inverse
    setinverse <- function(inv) i <<- inv
    
    ##Get inverse
    getinverse <- function() i
    
    ##Create and return list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix or return if already calculated

cacheSolve <- function(x, ...) {
    ##Try and get the inverse
    i <- x$getinverse()
    
    #If inverse has been calculated, return
    if (!is.null(i)){
        message("getting cached data")
        return i
    }
    
    #Otherwise, solve for inverse
    m <- x$get()
    i <- solve(m, ...)
    
    #Then cache the value
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
