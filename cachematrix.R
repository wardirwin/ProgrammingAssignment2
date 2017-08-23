##
## Utility functions to compute once and cache the inverse
## of an invertible square matrix in order to avoid costly
## computations doing the same calculation each time the
## inverse is requested.
##
## It is assumed that the matrix supplied is always invertible
## (as per the instructions given for this assignment).
##
## Example usage:
##
##     > a <- matrix(c(2,4,3,7), nrow=2, ncol=2)
##     > a
##          [,1] [,2]
##     [1,]    2    3
##     [2,]    4    7
##     > cm = makeCacheMatrix()
##     > cm$set(a)
##     > cacheSolve(cm)
##          [,1] [,2]
##     [1,]  3.5 -1.5
##     [2,] -2.0  1.0
##     > cacheSolve(cm)
##     getting cached data
##          [,1] [,2]
##     [1,]  3.5 -1.5
##     [2,] -2.0  1.0
##

##
## makeCacheMatrix()
##
##     Creates a special "matrix" object that can cache its inverse.
##
##     If getinv() returns NULL, it is because cacheSolve() has
##     not been called after 'y' has been set(). This has been left
##     this way in order to be consistent with the example special
##     'vector' code used to illustrate this programming assignment.
##

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) xinv <<- inverse
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

##
## cacheSolve
##
##      Computes the inverse of the special "matrix" returned by
##      makeCacheMatrix above. If the inverse has already been
##      calculated (and the matrix has not changed), then
##      cacheSolve() retrieves the inverse from the cache.
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if (!is.null(xinv)) {
        message('getting cached data')
	return(xinv)
    }
    mat <- x$get()
    xinv <- solve(mat)
    x$setinv(xinv)
    xinv
}
