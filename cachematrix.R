## This function creates a cache matrix. This matrix is
# used by cacheSolve method to cache the inverse matrix of it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(get = get, setInv = setInv, getInv = getInv)
}


## Use the cache matrix from makeCacheMatrix function to calculate 
# the inverse matrix of it. The invese matrix is saved to cached matrix. 
# So you don't need to recalculate it next time.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)){
            message("inverse was cached")
            return(inv)
        }
        
        inv <- solve(x$get())
        x$setInv(inv)
        inv
}
