makeCacheMatrix <- function(x = matrix()) {
      m <- NULL #sets m to NULL to act as a placeholder for a future value
      
      set <- function(y) {
            x <<- y #set the vector to the value x
            m <<- NULL  #resets the solved matrix to NULL
      }
      
      #returns the vector x
      get <- function() x
      
      #setmatrix assigns the inverse to m
      setmatrix <- function(solve) m <<- solve
      
      #returns the inverse, m
      getmatrix <- function() m
      
      #creates a return value
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      #assigns value from getmatrix the value of m 
      m <- x$getmatrix()
      #if m is not empty, return the message and m itself
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      #assign to data the matrix x
      data <- x$get()
      #solves the matrix 
      m <- solve(data, ...)
      #calls setmatrix to set the matrix in makeCachematrix
      x$setmatrix(m)
      #return m
      m
}