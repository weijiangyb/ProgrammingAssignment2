## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Added Comment 1: makeCacheMatrix function creates a matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## Added Comment 2: cacheSolve function computes the inverse of the special "matrix" created
## by makeCacheMatrix function above. If the inverse has already been calculated and the
## matrix has not changed, it will retrieve the inverse from the cache instead of computing one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
