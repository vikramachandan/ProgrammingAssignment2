## Vikramachanddran Selvakumar - Cumming, Georgia  27-Dec-2015
## The following two functions template was originally written by Roger.D.Peng 
## from CoursEra team as part of R Programming Language Course.
## Took this course Dec 2015 and as part of assignment students participating were expected to 
## complete the functions as part of week 2 programming assignment.

## makeCacheMatrix function takes matrix object/vetor as input and returns special vector/object, list of functions.
## precondition is input matrix must be a square matrix on which inverse could be calculate.
## set function initializes the matrix object and inverse mat objects.
## get function returns the matrix
## compinv function caches the inverse matrix into invmat
## getinv function returns invmat
## Example invocation
## t <- matrix(1:4,2,2)
## d <- makeCacheMatrix(t)

makeCacheMatrix <- function(x = matrix()) {
invmat = NULL
  set <- function(y){
      x <<- y
      invmat <<- NULL
  }
  get <- function() x
  compinv <- function(invm) invmat <<- invm
  getinv <- function(x) invmat
  list(set=set,get=get,compinv=compinv,getinv=getinv)

}

## cacheSolve function takes object/specical vector returned by makeCachematrix as input
## if the inverse of the matrix is present in the cache it does not compute the inverse of the matrix - returns from the cache
## else it computes the inverse of the matrix and returns it.
##example invocation
## t <- matrix(1:4,2,2)
## d <- makeCacheMatrix(t)
##cacheSolve(d)
## when you invoke first time you SHOULD NOT see "getting cached data" message on the console
## when you invoke second time you SHOULD see "getting cached data" message on the console indicating that the inverse is being 
## picked up from cache rather than getting computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          invmat <- x$getinv()
  print(invmat)
  message("before")
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  message("after")
  data <- x$get()
  invmat <- solve(data,...)
  x$compinv(invmat)
  invmat
        ## Return a matrix that is the inverse of 'x'
}
