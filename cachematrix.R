## A set of functions that cache matrix operations

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix(), ...) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Retrieves from cache if existing, else computes the inverse of the matrix 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(i)
    i
}
