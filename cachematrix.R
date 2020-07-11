## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                                       #define a NULL inverse
  set <- function(y) {                                            #defining a function here which will set values to our matrix x and it's inverse m
    x <<- y
    m <<- NULL
  }
  get <- function() x                                             #used to get the value of x
  setinverse <- function(inv) m <<- inv                           #this function is used to set some value for our inverse matrix m
  getinverse <- function() m                                      #this function is used to get the values of inverse m
  list(set = set, get = get,                                      ## It creates a list of all the functions you have defined.you need this in order to refer to the functions with the $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function
# Now, If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve function will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  m <- x$getinverse()                         #get the inverse from cache
  if(!is.null(m)) {                            #if inverse is already there then just return it and no need to continue.
    message("Getting cached data")
    return(m)
  }
  data <- x$get()                                      #if inverse is not in the cache then get the values of x
  m <- solve(data, ...)                                 #find the inverse of our matrix
  x$setinverse(m)                                       #set it's inverse in the cache
  m                                                   #return the values of x
  
}
