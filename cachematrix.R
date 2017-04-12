## Caching the Inverse of a Matrix 
## set a matrix
## get a matrix
## set an inverse
## get an inverse

## create a special "matrix" object that can cache its "inverse"

makeCacheMatrix <- function(x = matrix()) {
        invrs = NULL
    set = function(y) {
        x <<- y
        invrs <<- NULL
    }
    get = function() x
    setinv = function(inverse) invrs <<- inverse 
    getinv = function() invrs
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## Compute the "inverse" of the special "matrix"

cacheSolve <- function(x, ...) {
    invrs = x$getinv()
    if (!is.null(invrs)){
        message("getting cached data")
        return(invrs)
    }
    mat.data = x$get()
    invrs = solve(mat.data, ...)
    x$setinv(invrs)
    return(invrs)
}
