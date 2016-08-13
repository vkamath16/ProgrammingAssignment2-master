## Function creates a special matrix object that can Cache the Inverse of a Matrix:

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve functon does inverse operation of the special matrix created by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("We got cached data here")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## Testing
##x = matrix(1:4,2,2)
##m = makeCacheMatrix(x)
##m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##cacheSolve(m)
##getting cached data
##    [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5