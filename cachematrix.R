## The following pair of functions cache the inverse of a matrix. Doing that you can in some cases use the cache instead 
## of calculating the inverse repeatedly, and skips all the computation each time.

## MakeCacheMatrix creates a matrix, which is a list containing a function to set the value of the matrix,
## get the value of the Matrix, set the value of the solve (inverse of a matrix), and get the value of the solve.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## CacheSolve calculates the solve (inverse of a matrix) of the matrix created in the previos function. 
## The function first checks to see if the function solve has already been calculated. If so, it gets the inverse from
## the cache and skips all the computation. Otherwise, it calculates the inverse of the data and sets the value of this
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}

## The function makeCacheMatrix is incomplete without the function CacheSolve.
## To run the function you have to enter: cacheSolve(makeCacheMatrix("insert some matrix here"))