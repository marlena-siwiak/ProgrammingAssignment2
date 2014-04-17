## Pair of functions to compute and cache the inverse of a matrix. 

## The function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(invmatrix) m <<- invmatrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function computes the inverse of the matrix object returned by the makeCacheMatrix function. If the inverse has already been computed and stored in the matrix object, the function retrieves it from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}