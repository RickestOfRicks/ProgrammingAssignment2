## This function was created for the Coursera course:
## R Programming's second assignment

## This function creates a special "Matrix" object that can have it's 
## inverse cached.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
      set <- function(temp) {    			##Allows user to set new values for the matrix
            x <<- temp
		inverse <<- NULL
		print("Set Matrix")
      }
	setInverse <- function(tempInverse){
		inverse <<- tempInverse				##Sets the inverse of the matrix in a different environment
	}
	get <- function() x						##Returns the matrix
	getInverse <- function() inverse
	
	##Displays the different functions within this function.
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the given matrix. If this inverse
## has already been calculated, the function returns the cached inverse if 
## and only if the parent matrix has remained unchanged.

cacheSolve <- function(x, ...) {

	inverse <- x$getInverse()				## Gets the inverse of the matrix
	if(!is.null(inverse)){					## Checks if inverse has been cached
		message("Getting cached inverse.")	## If the inverse has been calculated
		return(inverse)						## returns the cached matrix
	}
	matrix <- x$get()						## If the inverse has not been calculated
	inverse <- solve(matrix)				## previously, calculates the inverse of the matrix
	x$setInverse(inverse)

	inverse									## Return a matrix that is the inverse of 'x'
}
