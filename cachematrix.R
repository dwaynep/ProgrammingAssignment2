## This file contain two functions that will do the following

## Creates a special "matrix" that can cache its inverse
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinversenum<- function(inverse) inv_x <<-inverse
    getinversenum <- function() inv_x
    list(set = set, get = get,
         setinversenum = setinversenum,
         getinversenum = getinversenum)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv_x <- x$getinversenum()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinversenum(inv_x)
        return(inv_x)
    }

}
