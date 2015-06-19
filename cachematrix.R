## These functions cache the inverse of a matrix object so that it
## does not have to be recalculated every time.

## makeCacheMatrix defines a special matrix object which caches
## the inverse along with the matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## initiate cache
    m <- NULL
    ## assign the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## retrieve the matrix
    get <- function() x
    ## calculate the inverse
    setsolve <- function(solve) m <<- solve
    ## retrieve the inverse
    getsolve <- function() m
    ## return a list containing the functions.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve checks if the inverse has already been calculated and
## returns that value, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## look for cached data
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## inverse not been cached so calculate it
    data <- x$get()
    m <- solve(data, ...)
    ## cache data for future use
    x$setsolve(m)
    ## return inverse
    m
}
