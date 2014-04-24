## Put comments here that give an overall description of what your
## functions do

## These functions will create a matrix and allow you to get the inverse and
##      cache the inverse.  By caching the inverse, the inverse does not need
##      to be calculated again when cacheSolve is called.


## Write a short comment describing this function

## This function allows you to create a matrix (using set), get the matrix,
##      set the inverse of the matrix, and get the existing matrix inverse.
## The cache of the inverse will be set in the value 'm'

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function

## This function returns a matrix that is the inverse of 'x'
## If the inverse of 'x' has already been cached, the cached value will be
##      returned instead of recalculating it.
## If the inverse of 'x' has not been cached, then the inverse of 'x' is
##      calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
