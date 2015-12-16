## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.
## x is a special
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(is.null(inv)) {
        #cached inverse was null, so recalculate and cache calculated inverse
        inv <- solve(x$get())
        x$setinverse(inv)
    }
    return(inv)
}
