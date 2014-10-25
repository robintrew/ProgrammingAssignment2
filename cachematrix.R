## This pair of functions enable the caching of a matrix inverse
## inside a special "matrix" object.

## This function creates a special "matrix" object (a list) from a matrix.
## The "matrix" object can cache the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## This function returns the inverse of the special "matrix"
## returned by makeCacheMatrix. It retrieves a previously cached inverse
## or caches a newly calculated inverse.
cacheSolve <- function(x, ...) {
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
