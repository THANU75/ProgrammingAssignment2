
#********************************************************************************************************
## The function makeCacheMatrix() creates a special "vector", which is really a list containing a function to
#
# set the value of the matrix
# get the value of the matrix
# set the value of the Inverse Matrix
# get the value of the Inverse Matrix
#
#********************************************************************************************************
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL # Set inv_matrix to NULL
  #For "Set" function - Pass Inp_matrix to x / Reinitialize inv_matrix to NULL
  set <- function(Inp_matrix) {
    x <<- Inp_matrix
    inv_matrix <<- NULL
  }
  
  get <- function() x # Return the matrix x
  #Function to get the Inverse Matrix of x (assigned to inv_matrix)
  setinverse <- function(inverse) inv_matrix <<- inverse 
  
  # Return the Inverse Matrix of x (inv_matrix)
  getinverse <- function() inv_matrix
  list(set = set, # gives the name 'set' to the set() function defined above
       get = get, # gives the name 'get' to the get() function defined above
       setinverse = setinverse, # gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse) # gives the name 'getinverse' to the getinverse() function defined above

}

#********************************************************************************************************
## cacheSolve() function is required to populate and/or retrieve the inverse matrix from input matrix
#
# The following function calculates the Inverse Matrix of the special "matrix" created with the above function makeCacheMatrix(). 
# However, it first checks to see if the Inv Matrix  has already been calculated. If so, it gets the Inv Matrix from the cache 
# and skips the computation. Otherwise, it computes the Inverse Matrix of the Matrix in the cache 
# and sets the value of the Inverse Matrix  in the cache via the "setinverse" function.
#
#********************************************************************************************************
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  #If there is already  Inverse Matrix in the cache - 
  #And print message("getting cached data")
  #Return the value of Inverse Matrix
  if (!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  #If the cache is not having valid Inverse Matrix - 
  #Get the Matrix from cache and assign to my_matrix
  my_matrix <- x$get()
  #Using Solve() function compute the Inverse Matrix of my_matrix 
  inv_matrix <- solve(my_matrix, ...) # Assign to "inv_matrix"
  
  x$setinverse(inv_matrix)#Save the inverse matrix "inv_matrix" to cache
  inv_matrix  # Return the value of inverse matrix "inv_matrix"
}
