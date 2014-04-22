## Functions to reduce computation time by using a cache when calculating the
# inverse of a matrix

## The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setinv <- function(inverse) inv <<- inverse
  #get the value of the inverse
  getinv <- function() inv
  #return the lisst of functions
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


#The function cacheSolve calculates the inverse of the special "matrix" created 
#with the makeCahcheMatrix function. It first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse from the cache. If not it 
#calculates the inverse of the matrix and sets the value of the inverse in the 
#cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #Check if inv is already existing
  if (!is.null(inv)) {
    #If exists retunrs the cached data
    message("Inverse already calculated, getting result from data")
    return(inv)
  }
  #If not, calculate the inverse
  #get the matrix
  matr <- x$get()
  #Calculate the inverse
  inv <- solve(matr, ...)
  #Update the cache
  x$setinv(inv)
  #return the inverse
  inv
}
