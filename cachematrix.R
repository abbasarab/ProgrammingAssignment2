#Matrix inversion is usually a computationally expensive operation in data 
#analysis. If we have a matrix that its content does not change during the 
#course of the computation, it may be justified to compute the inverse of 
#matrix and cache it instead of computing its inverse over and over again. 
#The purpose of this program is to fulfill exactly this goal. 

#The "makeCacheMatrix" gets a matrix as its input and create a list containing
#the original matrix and NULL value as its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#The "cacheSolve" function get the list provided by "makeCacheMatrix" as an input
#and check the inverse in the list. If the inverse is NULL, i.e. it is the 
#first time that we are trying to compute the inverse, it computes the inverse 
#and replace the NULL by computed inverse matrix and return the inverse. If 
#the inverse was not NULL at the first place, i.e. the inverse has been computed 
#previously, it skips the computation and return the previously computed 
#inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (!is.null(x$getinv())){
    message("Getting cached data.")
    return(x$getinv())
  }
  matrix <- x$get()
  x$setinv(solve(matrix))
  x$getinv()
}
