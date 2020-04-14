## The following functions create a custom matrix object that is able to
## calculate and cache its own inverse.

## returns an object that contains the given matrix and the ability to
## to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
        )
}


## returns the inverse of the given matrix object. If value is not cached,
##inverse is calculated and stored on the passed in object

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("returning cached inverse")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}
