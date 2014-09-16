# Two functions that save/cache the inverse of a matrix


# Create an objecct that cache the inverse of a matrix
makeCacheMatrix <- function(m = matrix()) {
  
  # Initialize the inverse
  i <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  # Function that gets/returns the matrix
  get <- function() {m}
  
  # Function that sets the inverse of the matrix
  setInverse <- function(inverse) {i <<- inverse}
  
  # Function that gets/returns the inverse of the matrix
  getInverse <- function() {i}
  
  # Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# A function to compute the inverse of a matrix returned by the above function.
# If the inverse has already been calculated,
# then the function should return the saved inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # Checks if inverse has been calculated previously
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse with matrix multiplication
  m <- (solve(data)) %*% (data)
  
  # Set the inverse to the object
  x$setInverse(m)
  
  # Return the matrix
  m
}
