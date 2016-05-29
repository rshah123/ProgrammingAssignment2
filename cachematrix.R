## To Cache the Inverse of a Matrix:
## Benefit to cache the inverse of a matrix rather than compute it repeatedly
## Below are pair (two) of functions that are used to create a special object that 
## stores a matrix and caches its inverse

## The below function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(matrixinv) inv <<- matrixinv
        getInv <- function() inv
        list(set = set,get = get,setInv = setInv,getInv = getInv)
}
## The below function computes the inverse of the special "matrix" created by 
## makeCacheMatrix function above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
