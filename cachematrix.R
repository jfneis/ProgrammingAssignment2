## creates a object that caches the matrix inverse. if the same matrix is
## calculated again, the cached result will be returned

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    ## <<- operator stores the object in another enviroment
    setinverse <- function(solve) inverse <<- solve 
    getinverse <- function() inverse

    ## returns a list so the object created by this function can be called as
    ## x$get or x$setinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## try getting the inverse matrix from cache.
## if not possible, calculates it (and stores in cache)

cacheSolve <- function(x, ...) {
    ## no problem using 'inverse' variable name as the cached one is in another enviroment (<<-)
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## arriving here means the cache was not hit
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
