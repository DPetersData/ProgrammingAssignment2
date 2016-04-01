## Overall these functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setValue <- function(Value) m <<- Value
    getValue <- function() m
    list(set = set, get = get,
         setValue = setValue,
         getValue = getValue)
}


## This function computes the inverse of the special "matrix" returned by the 'makeCacheMatrix' function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getValue()
    
    if(!is.null(m)) {
        message("retrieving data from cache")
        return(m)
        } 
    
    message("matrix is currently NULL")
    
    m <- x$get()
    mInverted <- solve(m)
    return(mInverted)
}
