## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(InverseM) m <<- InverseM
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	if (det(x) != 0) { ### Caurse we can assume that the matrix is invetible, but  
				 ### it's rather easy to check (if we aren't talking about the 
				 ### pseudoinversion of matrix, which could be applied to any matrix).

		m <- x$getInverse()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}

		data <- x$get()
		m <- solve(data, ...)
		x$setInverse(m)
		m
	}
	else { 
		message("Matrix is not invertible!") 
		return(m)
	}
}
