## The makeCacheMatrix function creates a special matrix object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function computes the inverse of the special "matrix" created by
## the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ma <- x$get()
    inv <- solve(ma, ...)
    x$setInv(inv)
    inv
}
