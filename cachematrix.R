## Matrix inversion is a time consuming calculation.  We will cache the inverse of the matrix
## rather than compute it for every loop. 

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the value of the inverse of the matrix
## Checks to see if the inverse has already been calculated. If the inverse
## has been calculated the program retrieves and returns that value.
## if the value has not been determined then the program computes the inverse
## and sets the value to the cache value (inv) and then returns the inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if X is a square invertible matrix, then solve(X) returns its inverse.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
