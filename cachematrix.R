## The main idea of this function is copied from rdpeng in his script of manipulation of mean

# Calculating the inverse of a matrix is quite troublesome especially the order is very large. 
# My functions below can effectively solve this problem. We input the matrix that we would like 
# to compute its inverse in makeCacheMatrix. Then this function will generate a list containing 4 functions regarding the matrix
# This is to prepare for the second function.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# We input the returned list generated in function makeCacheMatrix in the function below cacheSolve.
# If the returned list containing the inverse of the original matrix, then the cacheSolve will directly extract the inverse matrix
# Otherwise, the inverse will be calculated here.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        temp <- x$get()
        inv <- solve(temp, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
