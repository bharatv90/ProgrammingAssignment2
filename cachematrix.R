## Put comments here that give an overall description of what your
## functions do

## This function creates a special vector which contains functions to
## set the value of the vector, get the value of the vector, set the 
## inverse of the vector and get the inverse of the vector.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special vector created in the 
## above function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}