## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## The first function, makeCacheVector creates a special "matrix", which is
## really a list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse and set the
## value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
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


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
