## Put comments here that give an overall description of what your
## functions do

## Function: to create a function with get and set that cache the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    # Set a default value of an inverse matrix
    m <- NULL
    # Set a matrix + reset value of an inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Get a matrix
    get <- function() x
    # Set an inverse matrix
    setInv <- function(solve) m <<- solve
    # Get an inverse matrix
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Function: to solve the inverse of the matrix
## (This function will return its value from cache if it already existed)
cacheSolve <- function(x, ...) {
    # Get an inverse matrix from cache
    m <- x$getInv()
    # Check an inverse matrix from cache and display a message
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Calculate a new inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    # Set an inverse matrix to cache
    x$setInv(m)
    # Return a matrix that is the inverse of 'x'
    return(m)
}
