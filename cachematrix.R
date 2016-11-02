## This collection of functions serves to cache inverse of a matrix,
## as a time-saving substitute of recomputation.

## Initialize cached matrix pseudo-class.
## Return a list containing cached inverse (defaul NULL) and operating functions.
## Note: The functions assume all input matrices are invertible!
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        invisible(list(set = set, get = get,
             setinv = setinv,
             getinv = getinv))
}


## Check status of inverse cache, return inverse if cached,
## calculate and return if not.
cacheSolve <- function(x,...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        m <- x$get()
        inv <- solve(m,...)
        x$setinv(inv)
        inv
}
