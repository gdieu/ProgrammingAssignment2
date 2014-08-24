## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) invMatrix <<-inverse
        getinverse <- function() invMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated  
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinverse()
        if (!is.null(invMatrix)) {
                message("getting cached inverse matrix")
        } else {
                invMatrix <- solve(x$get())
                x$setinverse(invMatrix)
        }
        invMatrix
}