## These two functions calculate the inverse of a matrix, then cache that value
## for quick retrieval.  If a new matrix is calculated, it will recalculate and
## cache the new inverse.

## This function performs the following:
## 1. Set the value of the matrix
## 2. Retrieve the value of the matrix
## 3. Set the value of the inverse
## 4. Retrieve the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function checks to see if the inverse is already cached; if it is,
## retrieve it.  If not, recalculate the inverse and cache the new values.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}