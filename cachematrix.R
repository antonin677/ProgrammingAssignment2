## This goup of functions calculates the inverse of a matrix and cache
## the result in memory where it can be retrieved the next time the 
## function is called for the same matrix instead of calculating it again.

## This function creates a list object comprised of functions that can
## store and retrieve a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function uses the object created by makeCacheMatrix to check if 
## the inverse of a matrix has been calculated, if it has it
## returns the inverse, if it hasn't it calculates the inverse, caches it
## to memory and returns the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, diag(ncol(data)), ...)
    x$setinverse(inv)
    inv
}

