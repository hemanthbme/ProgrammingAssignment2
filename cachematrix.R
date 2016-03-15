## Matrix inversion is a time consuming operation and there may have some 
## benefit in caching the inverse of matrix rather than computing it 
## repeatedly. 

## This function creates a list in which the cached matrix inversion
## is saved into and called whenever needed.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## m return as NULL
    set <- function(y) { 
        x <<- y  ## x==y
        m <<- NULL ## m==NULL
    }
    get <- function() x 
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function helps in calling the cached data, and in case it doesnt find,
## it computes the matrix inversion and produce the data

cacheSolve <- function(x, ...) {
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
