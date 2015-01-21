
## makeCacheMatrix() is a function that creates a list of functions

## 1. set() - this sets the matrix to be inversed
## 2. get() - if a matrix is set this returns the set matrix
## 3. setinverse() - sets the inverse of a matrix
## 4. getinverse() -  if an inverse matrix is set this returns the inverse matrix
	
makeCacheMatrix <- function(x = matrix(),...) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x		
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i      
	  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is a function that returns the inverse of the matrix set() in makeCacheMatrix.
## If the inverse of the matrix has already been calculated (as tested through (!is.null(i))
## it returns the cached matrix, if the matrix has not been calculated on the matrix has been 
## changed through set() then the function re calculates the inverse matrix.  

cacheSolve <- function(x, ...) {		
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
		matrix <- x$get()
        i <- solve(matrix, ...)       
        x$setinverse(i)
        i
}
