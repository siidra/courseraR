## Create a matrix object and compute the inverse of the matrix,
## using lexical scoping to avoid recalculating the inverse if
##it has already been calculated and has not changed.
## Assumption: the matrix is always invertible 

## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<-y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of the special matrix returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then it should retrieve the inverse from the cache (not recalculate it)
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
