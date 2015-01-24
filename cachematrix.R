## Together the functions create an object that can store a matrix and its inverse and calculate a matrices inverse. The result of inverting the matrix
## can be cached to save from having to recalculate it again in future 

## makeCacheMatrix accepts a matrix as an argument and returns a list with 4 functions to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

		m <- NULL
		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
        get <- function() {
				x
		}
		
        setinvmatrix <- function(invmatrix) {
				m <<- invmatrix
		}
		
        getinvmatrix <- function() {
				m
		}
		
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)

}

## cacheSolve accepts an object created by makeCacheMatrix as an argument.
## It checks to see if the matrix has been inverted previously and if it has then it returns the previously calculated inverted matrix as the result.
## Otherwise it calculates the inverted matrix, stores this value in the cache and returns the calculated inverted matrix as the result. 

cacheSolve <- function(x, ...) {
        m <- x$getinvmatrix()
		if(!is.null(m)) {
			message("getting cached inverted matrix")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinvmatrix(m)
		m
}
