## makeCacheMatrix - creates a list of functions for a matrix x
##    set(m)      - set the matrix values to be inverted
##    get(m)      - returns the matrix values that were set
##    setInv(inv) - sets the inverted matrix values
##    getInv      - gets the cached inverted matrix values
## 
## cacheSolve - inverts a matrix x using caching
##
## to use:
##   n=10
##   mnxn <- matrix(runif(n*n), nrow=n, ncol=n)  #or any other square matrix
##   x <- makeCacheMatrix()
##   x$set(mnxn)
##   x$get()
##   cacheSolve(x)  #calcuates the value and caches it
##   imnxn <- cacheSolve(x)  #returns the cached value
##   round( mnxn %*% imnxn)  # shold be an identity matrix

## create a set of functions to invert and cache a matrix
makeCacheMatrix <- function(x = matrix()) {
     xInv <- NULL               # inverted matrix
     
     set <- function(m) {       
          x <<- m
          xInv <<- NULL         # inverted matrix needs to be recalculated
     }
     get <- function() x
     setInv <- function(inv) xInv <<- inv
     getInv <- function() xInv
     
     list ( set= set, get=get, setInv=setInv, getInv=getInv)
}


## inverts a matrix and caches it for fast lookup

cacheSolve <- function(x, ...) {
     inv <- x$getInv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     message("solving inverse")
     mdata <- x$get()
     inv <- solve(mdata, ...)
     x$setInv(inv)
     inv
}
