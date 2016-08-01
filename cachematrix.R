## this program caches the inverse of a matrix

## create a "matrix" that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  cacheMat <- NULL
  set <- function(y) {
      x <<- y
      cacheMat <<- NULL
  }
  get <- function() x
  setMat <- function(inverse) cacheMat <<- inverse
  getInv <- function() cacheMat
  list(set = set, get = get, setMat = setMat, getInv = getInv)
}


## compute the inverse of the "matrix" that is returned by 
## makeCacheMatrix:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cacheMat <- x$getInv()
    if(!is.null(cacheMat)){
        message("getting cached data")
        return(cacheMat)
    }
    matData <- x$get()
    cacheMat <- solve(matData, ...)
    x$setMat(cacheMat)
    cacheMat
}
