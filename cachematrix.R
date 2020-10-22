## These functions together cache the inverse of a special matrix.
## Both codes were inspired by the "makeVector" and "cachemean" examples.

## Starting with the function "makeCacheMatrix", the first step was to set
## and get the value of the matrix. Then, the value of its inverse is
## programmed through "setInverse" and "getInverse".

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) Inverse <<- inverse
        getInverse <- function() Inverse
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Then, we have the code of the function "cacheSolve", which calculates the 
## inverse of the matrix generated above. First, it is reviewed whether the
## inverse has already been calculated or not. If yes, the inverse value is
## obtained from the cache directly. If not, it calculates the inverse of the
## data and stores the value in the cache thanks to "setInverse".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}