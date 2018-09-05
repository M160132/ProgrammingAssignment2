## The main idea of this function is copied from rdpeng in his script of manipulation of mean

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
