## This program caches the inverse of a matrix to help 
## rather than compute it repeatedly which could be costly computation
## We intend to have 2 functions - one for caching the matrix and 
## other reuse the cache (than to recalculate)

## This function creates a special "matrix" object that can cache its inverse. 
## This has bunch of 4 functions - 
## 1 - get matrix, 
## 2 - set matrix, 
## 3 - set matrix inverse & 
## 4 - get matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##inv matrix var
  im <- NULL
  
  setmat <- function(y) {
        x <<- y
        im <<- NULL
  }
  
  getmat <- function() x
  ##setmatinverse <- function(solve) im <<- solve
  setmatinverse <- function(z) {
    im <<- solve(z)
  }
  getmatinverse <- function() im
  list(setmat = setmat, getmat = getmat,setmatinverse = setmatinverse,
       getmatinverse = getmatinverse)

}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed) then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  im <- x$getmatinverse()
  if(!is.null(im)) {
    print("getting cached data")
    return(im)
  }
  data <- x$getmat()
  im <- solve(data, ...)
  x$setmatinverse(im)
  im
  
}
