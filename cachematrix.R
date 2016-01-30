## The function makeCacheMatrix returns a list of some functions
## the functions that contains are: 
#set = set the value of the Matrix , get = returns the stored matrix
#setInverse = set a inverse matrix, getInverse = get the cached value
#The cacheSolve function calculates the inverse matrix that was created with
# the makeCacheMatrix function.

## The makeCacheMatrix creates a matrix and it can stored their 
# Inverse Matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        # store the matrix 
        set <- function(m) {
                x <<- m
                inverse <<- NULL
        }
        # returns the stored matrix
        get <- function() x 
        
        setInverse <- function(invM) inverse <<- invM
        # get the cached value
        getInverse <- function() inverse
        #return a list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

cacheSolve <- function(x, ...) {
        # get the cached value
        inv <- x$getInverse()
        # if a cached value exists, it is returned.
        if(!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        # otherwise get the matrix and calculate the inverse
        # and store it in cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        # return the inverse
        inv
}