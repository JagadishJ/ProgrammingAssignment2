## Matrix inversion is usually a costly computation and their may be some benefit to ## caching the inverse of a matrix rather than compute it repeatedly. Below are ## two functions that are used to create a special matrix object that stores the ## inverse of the matrix in cache

## makeCacheMatrix: This function creates a special "matrix" object that can cache ## its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" returned ## by makeCacheMatrix above. If the inverse has already been calculated (and the ## matrix has not changed), then the cachesolve should retrieve the inverse from ## the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    #checking if the matrix inverse data exists in cache
    if(!is.null(m)) {
        message("getting matrix inverse cached data")
        return(m)
    }
    # if the matrix inverse is not present in cache, then calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

##example:
## b = matrix(c(1,2,3,4,5,4,3,2,1), nrow = 3, ncol = 3)
## y1 <- makeCacheMatrix(b)
## cacheSolve(y1)
## [,1] [,2]   [,3]
## [1,]  0.375   -1  0.875
## [2,] -0.500    1 -0.500
## [3,]  0.875   -1  0.375
## cacheSolve(y1)
## getting matrix inverse cached data
## [,1] [,2]   [,3]
## [1,]  0.375   -1  0.875
## [2,] -0.500    1 -0.500
## [3,]  0.875   -1  0.375

##573702fe80e915ef4319607dc702ce493c4203cc
