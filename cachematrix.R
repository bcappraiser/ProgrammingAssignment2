## Below are two functions; the first which creates a special matrix object that
## can cache its inverse, the second which computes the inverse of the special
## matrix returned by the first.

## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

##Some code to test run the above functions.
a <- matrix(c(1,3,8,20,15,36,29,88,10), nrow = 3, ncol = 3)
x <- makeCacheMatrix(a)
x$get()
x$getinverse()
cacheSolve(x)
