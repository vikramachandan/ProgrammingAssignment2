## Vikramachanddran Selvakumar - Cumming, Georgia  27-Dec-2015
## The following two functions template was originally written by Roger.D.Peng 
## from CoursEra team as part of R Programming Language Course.
## Took this course Dec 2015 and as part of assignment students participating were expected to 
## complete the functions as part of week 2 programming assignment.
## makeCacheMatrix function takes special object/vetor(matrix) as input and returns special vector/object list of functions.
## set function initializes the matrix object and inverse mat objects.
## get function returns the matrix
## compinv function caches the inverse matrix into invmat
## getinv function returns invmat


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

## cacheSolve function takes object/specical vector returned by makeCachematrix 
## if the inverse of the matrix is present in the cache it does not compute the inverse of the matrix pointed to by passed vector
##/object.
## else it computes the inverse of the matrix and returns it.

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
