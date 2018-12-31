## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function creates a "cached matrix", which is a list 
## containing following function:
## 1. Set the value of the matrix 
##    (It also sets inverse matrix to null)
## 2. Get the value of the matrix
## 3. Sets the value of the inverse matrix
## 4. Gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  set <- function(y) {
    x <<-y
    inv_x <<-NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) inv_x <<-solve
  
  getinverse <- function() inv_x
  
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function returns inverse matrix or NULL 
## if the solution cannot be found.
## As the first step, the function is trying to get 
## a cached value, if the value is not null, it returns 
## the value. If the value is not cache, the function 
## calculates and caches it.
## Note: I added additional check for the case when inverse 
## matrix doesnt exist
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting data from cache")
    return(i)
  }
  
  m <- x$get() 
  if (det(m) == 0) {
    message ("det(m) == 0, solution is not found")
    return(NULL)
  }
  
  i <- solve(m)
  x$setinverse(i)
  i
}