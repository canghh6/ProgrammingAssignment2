## The functions below are used to store a square matrix 'x', 
## as well as calculate and cache the inverse 'invM' of 'x'. 
## In addition to the function 'makeCacheMatrix' and 'cacheSolve'
## four functions, namely, set get setInv and getInv, will be 
## returned by makeCacheMatrix in a list, for accessing data cached 
## in makeCacheMatrix's defining environment.

## makeCacheMatrix is called to set a matrix in its defining env,
## and returns four functions, set get setInv and getInv, for
## retrieving and updating cached matrix 'x' and its inverse 
## 'invM'.
## We do not check if an input matrix is square and invertible, 
## and leave this check to solve().

makeCacheMatrix <- function(x = matrix()) {
    
    if (nrow(x) != ncol(x)) {
        message("Input matrix is not a square matrix")
        return()
    }
    
    invM <- NULL
    
    set <- function(y) {
        
        if (nrow(y) != ncol(y)) {
            message("Input matrix is not a square matrix")
            return()
        }
        
        if (nrow(x) != nrow(y) || sum(as.numeric(x!=y)) != 0) {
            x <<- y
            invM <<- NULL
        } else {
            message("New matrix is identical to the one cached")
        }
        
    }
    
    get <- function() x
    
    setInv <- function(invx) invM <<- invx
    
    getInv <- function() invM
    
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)

}


## This function, cacheSolve, is called to retrieve cached 
## inverse matrix, if available. In case a new square matrix
## is stored in the defining env of makeCacheMatrix by means
## of set(x), cacheSolve calculate the inverse of the new
## matrix, cache it to the env of makeCacheMatrix, and return it.

cacheSolve <- function(x, ...) {
    ## Return the inverse matrix of the matrix cached in
    ## the env of makeCacheMatrix
    
    invM <- x$getInv()
    
    if(!is.null(invM)) {
        message("getting cached inverse matrix")
        return(invM)
    }
    
    m <- x$get()
    invM <- solve(m, ...)
    x$setInv(invM)
    invM
    
}
