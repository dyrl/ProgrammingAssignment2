## Below function calculates and stores inverse of (an invertible) matrix. If the inverse has 
## been calculated previously, function returns the cached value. 
## If not, function calculates and caches the inverse.

## Function defines the following actions, to be called in later function:
## Set matrix argument
## Retrieve matrix argument
## Cache inverse calculated
## Retrieve inverse previously cached


makeCacheMatrix <- function(x = matrix()) {
  
              set <- function(y) {
                      x <<- y
                      inv <<- NULL
              }
              
              get <- function() x
              
              setinv <- function(solve) inv <<- solve
              
              getinv <- function() if(exists("inv")) {inv} else {inv <- NULL}
              
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)

}


## For an input matrix, the function does the following:
## 1. Checks if an inverse has previously been calculated and cached
## 2. If 1 is TRUE, checks if the input matrix is consistent with cached inverse
## If 1 & 2 TRUE, returns cached inverse
## If 1 or 2 FALSE, calculates, returns, and caches inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    
       inv <- x$getinv()
 
       if(!is.null(inv)) {
          if(exists("matr")) {
            if(identical(matr, x$get())) {
              message("getting cached data")
              return(inv)
            } 
          }
       }
  
 
       matr <<- x$get()
       inv <- solve(matr, ...)
       x$setinv(inv)
       inv
  
}
