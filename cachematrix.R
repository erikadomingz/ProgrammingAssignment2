## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a special "matrix" object that
## can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Initialize variables
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## gets the matrix x
    get <- function() x
    
    ## solve - a function that returns the inverse of a square
    ## matrix if square matrix is invertible
    setInverse <- function(solve) m <<- solve
    
    ## gets the inverse of the matrix
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMarix above

cacheSolve <- function(x, ...) {

    ## if the inverse has already been calculated (and the matrix has not
    ## changed), cacheSolve returns the inverse of the cache
    
    ## gets the inverse of the matrix x
    m <- x$getInverse()
    
    ## Checks if inverse has already been calculated 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## gets matrix
    data <- x$get()
    ## calculates inverse
    m <- solve(data, ...)
    ## sets inverse
    x$setInverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
