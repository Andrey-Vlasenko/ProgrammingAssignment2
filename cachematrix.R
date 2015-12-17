# Below are two functions that are used to create a special
# object that stores a numeric matrix and cache's its invertion.

## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x 
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse  
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	if (det(x$get()) != 0) { 
	### Course we can assume that the matrix is invetible,
	### but it's rather easy to check (if we aren't talking 
	### about the pseudoinversion of matrix).

		m <- x$getsolve()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}

		data <- x$get()
		m <- solve(data, ...)
		x$setsolve(m)
		m
	}
	else { 
		message("Matrix is not invertible!") 
		return(m)
		### Probably we could do something else
		### but we won't :)
	}
}
