## This pair of functions calculates the inverse of a matrix and returns it to the console and
## at the same time it stores the inverted matrix in memory and retrieves it when the function is called again.
## It can be useful to use this technique when otherwise compuationally intense operations (like inverting a matrix)
## have to repeated multiple times in a given script.
## While doing this, it is important to be aware of the rules of lexical scoping as they determine which functions
## and variables are accessible in which environment at any given time.

## Like makeVEctor, makeCacheMatrix defines a set of four functions and two variables that can later on be used by
## cacheSolve and returns them as a list to the parent environment. Like this, cacheSolve has not only access to the
## functions but also to the variables that were defined when the functions were created. In contrast to makeVector, 
## makeCacheMatrix requires the input to be a matrix rather than a vector.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Like cachemean, cacheSolve uses the functions and variables defined by their repespective "make" function to 
## perform a mathematical operation on the input of the "make" function. As input, both functions require an object
## which is a list of functions that have access to previously stored input variables. Before executing 
## operation, both functions check whether the operation has been performed before and return the chached answer
## from memory when applicable. In contrast to cachemean, cacheSolve does not cumpute a mean, but inverts a matrix
## and returns the result to the console.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
