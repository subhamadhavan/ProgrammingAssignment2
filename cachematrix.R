## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
## The first function, makeCacheMatrix creates a special "vector", 
## which is a list containing a function to set, get, set the inverse and get the inverse of a matrix



makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set<-function(y)  {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinverse<-function(inverse) inv_m <<- inverse
  getinverse<-function() inv_m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function checks if the inverse of the matrix is stored; if it is, it returns it; 
## if not, it calculates and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinverse()
    if(!is.null(inv_m)) {
        message("getting cached reverse matrix")
        return(inv_m)
    }
    my_data <- x$get()
    inv_m <- solve(my_data)
    x$setinverse(inv_m)
    inv_m
}
