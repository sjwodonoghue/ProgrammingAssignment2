## The below functions implement creating a special matrix object that
## allows the inverse of the matrix to be computed and stored in the
## parent/global envirnment.

## makeCacheMatrix creates a matrix object with associated functions that
## allow you to cache the inverse of the matrix in the parent/global 
## environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
        
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## if the inverse of the given matrix is cached cacheSolve will print that value
## else it will compute the value of the inverse and set the caching variable to
## be equal to this.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()

    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
