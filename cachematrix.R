## The two functions below collectively create, cache, and invert 
## matrices that are capable of inversion, in order to save computing power
## by simply accessing a cached version of the inverted matrix (when one is
## available), instead of re-inverting it every time.

## The makeCacheMatrix function creates a list containing the following
## functions:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

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


## The cacheSolve function retrieves the inverse matrix if it was
## previously cached, otherwise it calculates the inverse and returns
## the result.

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
