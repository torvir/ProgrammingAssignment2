## Solve the inverse of a matrix and cache result
## to reuse the cache if the same matrix is solved again.

## "makeCacheMatrix" takes a matrix as argument
## The matrix is stored in variable "x"
## and the Inverse matrix is cached in "m" (It is NULL by default)
## If "x" is set to another value "m" is set to NULL
## The functions "set", "get", "setInverse", and "getInverse" are returned as a list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## "cacheSolve" takes a matrix (created with makeCacheMatrix) as argument and returns the inverse matrix.
## It first checks if the inverse matrix already is cached, and returns it if it is present.
## Otherwise it calls solve() to get the inverse matrix, returns it
## and writes it to the cache variable of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
