## Put comments here that give an overall description of what your
## functions do

## creates an instance of a cache matrix object, with its inverse field, i,
## initialized to NULL, and a getter and setter function for i

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## first checks if the inverse value of the cache matrix object input has been
## calculated (i.e. not NULL). If it has, then it returns the inverse value of
## the matrix by accessing the i field using the getinverse() function. Else,
## it calculates and returns the inverse by itself.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
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
