## makeCacheMatrix and cacheinv codes are "blind" copies of makeVector and cachemean
# where the object "m" has been replaced by the object "inv" and the method mean()
# has been replaced by solve(), not exactly sure how it really works to be honest.
# Hence why there aren't any comment within the code itself (a lot of work 
# on lexical scoping to be done).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## See above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    }
