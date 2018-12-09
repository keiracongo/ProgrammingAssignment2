makeCacheMatrix <- function(x = matrix()) { ## define the argument
  inv <- NULL                               ## inv as NULL will hold value of matrix inverse
  set <- function (y) {                     ## define set function
    x <<- y                               ## value of matrix in the parent environment 
    inv <<- NULL                          ## set inv to NULL if there is a new matrix
  } 
  get <- function () x                      ## gets value of matrix
  
  setinverse <- function (inverse) inv <<- inverse ## value of inv in parent environment 
  getinverse <- function () inv                    ## gets value of inv
  list (set = set, get = get, setinverse =setinverse, getinverse = getinverse) ## order 
  
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <-x$getinverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()                 ## get original data
  inv <- solve(data, ...)         ## use solve function to inverse the matrix
  x$setinverse(inv)               ## set inv matrix
  inv
}
