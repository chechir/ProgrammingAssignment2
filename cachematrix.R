## The makeCacheMatrix function allows to store a Matrix and its inverted value
## The cacheSolve function, calculates the inverted of the matrix (using the solve function) if neccesary. If the value
##   was already in the cache, then just returns the cached value. In this way, calculation time could be saved

## Stores a matrix and its inverted as cache

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    m <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(solve) invMatrix <<- solve
    getInvMatrix <- function() invMatrix 
    list(set = set, get = get,
         setInvMatrix = setInvMatrix ,
         getInvMatrix = getInvMatrix )
}


## read from cache. Otherwise, calculates the inverted matrix for x using "solve"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInvMatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInvMatrix(inv)
    inv
}
