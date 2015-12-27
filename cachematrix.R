## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invmat = NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  compinv <- function(invm) invmat <<- invm
  getinv <- function(x) invmat
  list(set=set,get=get,compinv=compinv,getinv=getinv)

}


## Write a short comment describing this function

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
