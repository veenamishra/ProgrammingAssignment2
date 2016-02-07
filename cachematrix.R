## Following functions creates a matrix and stores its inverse in an environment
## other than its current environment and caches its inverse. 

## This function creates a special "matrix" object that can cache its inverse.
## The return object is a list contating four functions to access the matrix
## data and its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
        inverse <- NULL

        ## set the value of the Matrix and reset the inverse to NULL
        set <- function(y) {
                matrix <<- y
                inverse <<- NULL
        }

        ## get the value of the matrix
        get <- function() matrix

        ## set the value of the inverse of the Matrix
        setinverse <- function(x) inverse <<- x 

        ## get the value of the inverse of the Matrix
        getinverse <- function() inverse

        ## create the list of functions and return the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return (inverse)
        }
        
        ## compute the inverse of the given matrix
        data <- x$get()
        inverse <- solve(data, ...)

        ## cache the inverse
        x$setinverse(inverse)

        ## return the inverse
        inverse
}
