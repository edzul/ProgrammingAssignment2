## These functions will calculate and cache the inverse of a certain matrix 
## only calculating the inverse of it when it has changed

##Makes a special matrix object which contains functions for gettting,
##setting the matrix data inverse and data
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setdata <- function(y) {
      x <- y
      i <<- NULL
    }
    getdata <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(getdata = getdata,
         setdata = setdata,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This function will return the inverse of a matrix x, and calculating it only 
##when the matrix it has changed
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$getdata()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
