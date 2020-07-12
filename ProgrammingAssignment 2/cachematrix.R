## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initialize the inverse
  
  set <- function(y){  ## Set the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x ## get the matrix
  
  setinv <- function(inverse) inv <<- inverse  ## Set the matrix's inverse
  
  getinv <- function() inv ##Get the matrix's inverse function
  
  list(set=set,get=get,setinv=setinv,getinv=getinv) ##Return a list contain above functions
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if (is.null(x$getinv())==FALSE){ ##Testing if the matrix's inverse is calculated
    message("getting cached data")
    return(x$getinv())
  }
  
  else {
    mat <- x$get()  ##get the matrix
    invmat <- solve(mat,diag(2)) ##Calcul the inverse
    x$setinv(invmat)  
  }
  
  x$getinv() ##Return the matrix's inverse
}
