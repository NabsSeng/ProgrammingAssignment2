## contains 3 functions:
## a) makeCacheMatrix - stores the inverse of matrix if an inverse exists
## b) cachesolve - given a matrix, returns the inverse. If not cached caches result

library("matrixcalc")

## Store inverse of a matrix. Exposes set, get, setinv, getinv and getSingular as fuctions 
## that can be called by the consumer. 

makeCacheMatrix <- function(X = matrix()) {
  invMat <- NULL
  isSingular <- NULL
  set <- function(y) {
    X <<- y
    invMat <<- NULL
    isSingular <<- is.singular.matrix(X)
  }
  get <- function() X
  setinv <- function(inv)  { 
    invMat <<- inv
    
  }
  getinv <- function() invMat
  
  
  getSingularFlag <- function() { isSingular }
  
  list(set = set, get = get,
       
       setinv = setinv,
       getSingularFlag = getSingularFlag,
       getinv = getinv)
}
#######################################
## Return cached inverse matrix if availabl; else compute inverse and store result.
## Note not all matrices have an inverse - check is made to ensure matrix is non-singular
#######################################
cachesolve <- function(X, ...) {
  I_M <- X$getinv()
  if(!is.null(I_M)) {
    message("getting cached data")
    return(I_M)
  }
  
  M <- X$get()
  if (is.null(X$getSingularFlag()) || !X$getSingularFlag()) { 
    I_M <- solve(M,...)
    X$setinv(I_M)
    I_M
  }
  else {
    message("Inverse does not exist as matrix is singular")
    NULL
  }
  
}
##########################################
testCacheMatrixSolve <- function (X = matrix() ) {
  CM <- makeCacheMatrix(X)
  CM$set(X)
  res <- cachesolve(CM)
  print.matrix(res)
  message(res)
  res1 <- cachesolve(CM)
  print.matrix(res1)
  #res1
  #res
  # let us take the transpose of X 
  #CM$set(t(X))
  #cachesolve(CM)
  #cachesolve(CM)
  
}