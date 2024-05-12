## Put comments here that give an overall description of what your
## functions do

##This pair of functions cache the inverse of a matrix

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x<<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function

## This function calculates the inverse of the matrix created by the above function.
## It first checks to see if the inverse has already been calculated. If the 
## inverse has been calculated, it gets it from the cache; if it has not been 
## calculated, it will be calculated and set in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setinverse(inver)
        inver
}