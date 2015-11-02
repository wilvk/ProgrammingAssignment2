## Put comments here that give an overall description of what your
## functions do

## This function makes the cache matrix and sets a boolean whether the 
## matrix has changed or not to indicate to cacheSolve() whether 
## the matrix needs to be solved or the cached version can be used.

makeCacheMatrix <- function(x = matrix() ) {

	m <- NULL
	
	matrixChanged <- TRUE
	
	if(exists("tempMatrix") && tempMatrix == x) {
		matrixChanged <- FALSE
  	}
  	else {
    		tempMatrix <- x
  	}
		
	set <- function (y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	
	hasMatrixChanged <- function () matrixChanged

	setMatrix <- function(solvedMatrix) m <<- solvedMatrix

	getMatrix <- function() m

	list(set = set, get = get,
		setMatrix = setMatrix,
		getMatrix = getMatrix,
		hasMatrixChanged = hasMatrixChanged)
}


## This function takes the list from makeCacheMatrix and returns the value
## of the solved matrix. If the matrix has been previously solved, the existing
## solved matrix value is used, otherwise the matrix is solved.

cacheSolve <- function(x, ...) {

  m <- x$getMatrix()
  
	if(!is.null(m) || x$hasMatrixChanged() ) {
		message("getting cached matrix")
		return(m)
	}

	data <- x$get()

	m <- solve(data, ...)

	x$setMatrix(m)

	m
}
