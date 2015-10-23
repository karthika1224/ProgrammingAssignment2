## caching the inverse of a matrix
## Calculating each time will not be as effective as caching.

## Below are 2 functions that creates a special object which stores a matrix 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        nv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special matrix created by the above
## makecachematrix.If the inverse has already been calculated,it just retrieves 
## value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
