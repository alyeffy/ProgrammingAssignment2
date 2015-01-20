## Put comments here that give an overall description of what your
## functions do

## Returns a list containing a function to:
## 1. Set the value of the matrix.
## 2. Get the value of the matrix.
## 3. Set the value of the inverse of the matrix.
## 4. Get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- as.matrix(y)
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## REQUIRES: Matrix is square and invertible.
## Returns the inverse of a matrix. First checks if the inverse of the matrix
## has been computed. If it has, it accesses and returns the result and exits.
## Else it computes the inverse and sets the value in the cache using the
## setinverse function.

cacheSolve <- function(x, ...) {    
        i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
