## This function calculates the inverse of a matrix and cache
## its value. Subsequent calls will return the cached value


## This function creates a matrix with a variable x in the
## environment to cache the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## This function looks for a cached inverse of the matrix.
## If it exists, it is returned. Otherwise, it calculates the
## inverse matrix, stores it in the cache, and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
