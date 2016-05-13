## The following functions are able to cache potentially time-consuming computations.

## makeCacheMatrix creates a special "matrix" that
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    #This function creates a special "matrix" object that can cache its inverse.
    #initial cache value is set to NULL
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #return the matrix
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    #gets the value of the cache
    getInverse <- function() i
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}
