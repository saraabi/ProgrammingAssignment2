## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(x) {
        m <<- x;
        inverse <<- NULL;
    }
    get <- function() m;
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() inverse;
    return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## Write a short comment describing this function

cacheSolve <- function(m = matrix(), ...) {
        ## Return a matrix that is the inverse of 'm'
    inverse <- m$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- m$get()
    inverse <- solve(data, ...)
    m$setinv(inverse)
    return(inverse)
}
