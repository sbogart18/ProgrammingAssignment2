## Cache the inverse of a matrix 'x' (assuming matrix is invertible)
## Create structure so that previously-computed inverse can be retrieved without recalculation

## Assumes 'x' is an invertible matrix
## Creates list with functions that
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the matrix's inverse
    ## get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <- NULL
    }
    get <- function() x
    setInv <- function(inverse) Inv <<- inverse
    getInv <- function() Inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## If inverse matrix has already been computed, retrieves it from cache
## Otherwise, calculates inverse matrix

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv
            ## Return a matrix that is the inverse of 'x'
}
