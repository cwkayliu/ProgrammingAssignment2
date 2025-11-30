## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that stores the special matrix and the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## if the inverse already existed, retrieve the cached inverse; otherwise compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Example matrix
my_matrix <- matrix(c(2, 3, 4, 7), nrow = 2, byrow = TRUE)

# Special matrix
my_matrix1 <- makeCacheMatrix(my_matrix)
my_matrix1

# Call computes and cached matrix
inv1 <- cacheSolve(my_matrix1)
inv1

# Second call retrieves the cached inverse
inv2 <- cacheSolve(my_matrix1)
inv2
