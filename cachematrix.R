## Caching the Inverse of a Matrix

## The "makeCacheMatrix" function will create a special "matrix" object 
## that is capable of storing the inverse of a matrix.
makeCacheMatrix <- function(x=matrix()) {
  matrix_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  
  get <- function() x
  
  set_matrix_inverse <- function(input_inverse) matrix_inverse <<- input_inverse
  
  get_matrix_inverse <- function() matrix_inverse
  
  list(set = set, get = get,
       set_matrix_inverse = set_matrix_inverse,
       get_matrix_inverse = get_matrix_inverse)
  
}

## The "cacheSolve" function will calculate the inverse for special 
## "matrix" object returned from the "makeCacheMatrix" function if 
## a cached solution is not available (because it hasn't been calculated,
## stored or the matrix has been changed)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$get_matrix_inverse()
  
  # check if the matrix_inverse has previously been calculated and cached.
  if (!is.null(matrix_inverse)) {
    message("This is a cashed matrix inverse.")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data)
  x$set_matrix_inverse(matrix_inverse)
  matrix_inverse
}
