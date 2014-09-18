## This functions make the use of the solve function for matrices. 
## It makes the computation of the inverse of a matrix somewhat faster 
## and simpler by caching the computation.

## The makeCacheMatrix function creates a special matrix that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- mean
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The cacheSolve function computes the inverse of the special matrix 
## returned by the makeCacheMatrix function. If the inverse has already
## been calculated, then the cachesolve should retrieve the inverse from the cahce.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
