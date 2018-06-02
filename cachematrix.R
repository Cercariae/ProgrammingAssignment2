## makeCacheMatrix creates a special "matrix", with a function to:
	##  (1)set the value of the matrix
	##  (2)get the value of the matrix
	##  (3)set the value of the inverse = "inv"
	##  (4)get the value of the inv


## Making a matrix for the inverse

makeCacheMatrix <- function(x = matrix()) {
	   inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
	    inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv        
}

