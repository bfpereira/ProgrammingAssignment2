# These functions cache the inverse of a matrix. Matrix inversion can be 
# a costly computation so caching the inverse of a matrix is more efficent 
# than to repeatedly calculate it.

#makeCacheMatrix is a function that accepts a square matrix as it's input
# and returns a list consisting  of 4 internal functions:
#set() sets the value of the input matrix
#get() gets the value of the input matrix
#setinv() sets the inverse of the matrix
#getinv() gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        print (x)
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    output <- list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve takes the output list of the makeCacheMatrix. If the inverse of the
#is already cached then it returns it. If not, then it calculates the inverse
#matrix using solve()

cacheSolve <- function(output, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
# Return a matrix that is the inverse of 'x'

