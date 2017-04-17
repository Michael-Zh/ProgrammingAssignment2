## This pair of function will store the inversion of a matrix, making
## the inversion available when you need to use it later on (instead
## of recalculating.) 

## The first function have four parts:
##1.Set the value of the matrix
##2.Store the value of the matrix
##3.Set the inverse of the matrix
##4.Store the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function checked if the inversion already existed.
## If so, it would restore it. If not, then it will calculate the
## inversion and show the result.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
