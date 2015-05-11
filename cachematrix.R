## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix is a function that creates a special matrix that can be used to cache
its inverse, since matrix inversion can be time consuming.  Instead of recalculating
the inverse of a matrix, it will be stored in a cache for retrieval.

makeCacheMatrix returns a list with a function to 
1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the matrix inverse
4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrixInverse <- function(calcMatrixInverse) m <<- matrixInverse
    getmatrixInverse <- function() m
    list(set = set, get = get,
         setmatrixInverse = setmatrixInverse,
         getmatrixInverse = getmatrixInverse)
}


## Write a short comment describing this function

The following function calculates the matrix inverse of the special "vector"
created with the above function. However, it first checks to see if the
matrix inverse has already been calculated. If so, it `get`s the matrix inverse from the
cache and skips the computation. Otherwise, it calculates the matrix inverse of
the data and sets the value of the matrix inverse in the cache via the `setMatrixInverse`
function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$matrixInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- calcMatrixInverse(data, ...)
    x$setmatrixInverse(m)
    m
}
