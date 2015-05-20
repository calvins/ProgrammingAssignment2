# Programming assignment 2 is an opportunity to implement custom R functions that solve the common
# programming problem of wasting time recalculating something your code has already determined.
# Calculating means and matrix inversions are two examples.
# At the same time, it gives the student a chance to review R's Lexical Scoping Rules and how they
# are helpful when designing and coding the custom R functions.
# The function makeCacheMatrix will use the matrix you specify and create a special matrix that can cache its inverse.
# The function cacheSolve can calculate the inverse of the special matrix or retrieve it from a cache so that you save
# time by retrieving the value and not recalculating it.

# Our functions exist in the Global Environment, the Environment makeCacheMatrix was defined in, and 
# the Environment cacheSolve was defined in.
# 
# makeCacheMatrix has formal argument m
# and free variable i, and functions set, get, setinverse, and getinverse
# the set function has formal argument y and free variables m and i
# the set function uses the <<- operator to assign the value of the matrix from the calling environment, y to the formal
# argument m in the Environment makeCacheMatrix was defined in.
# the setinverse function has formal argument solve and uses the <<- operator to assign the value of solve from 
# the calling environment to the free variable i in the Environment makeCacheMatrix was defined in.
# 
# cacheSolve has formal argument m
# and free variables - i and data which get their values from the environment cacheSolve was defined in.

# The following steps demonstrate how to create special matrices, calculate their inverses, and cache them.
# 1. Create a special matrix that can cache its inverse!
## sm <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

# 2. Calculate the inverse of the special matrix, sm and display
## cacheSolve(sm)

# 3. Create another special matrix that can cache its inverse
## sm2 <- makeCacheMatrix(matrix(5:8, nrow=2, ncol=2))

# 4. When we try to recalculate the first matrix's inverse, cacheSolve finds it in the cache,
#    displays the getting cached data message, and the inverse of the matrix found in the cache
## cacheSolve(sm)

# 5. Calculate the inverse of the second special matrix (sm2) and display; it's not in cache yet
## cacheSolve(sm2)

# 6. When we try to recalculate sm2's inverse, cacheSolve finds it in the cache,
#    displays the getting cached data message, and the inverse of the matrix found in the cache     
## cacheSolve(sm2)

# We can see what's in each function's environment with these commands
# ls(environment(makeCacheMatrix))
# shows cacheSolve, makeCacheMatrix, sm, and sm2
# 
# ls(environment(cacheSolve))
# shows cacheSolve, makeCacheMatrix, sm, and sm2

# We can retrieve the value of a symbol in an environment with these commands
# get("sm", environment(makeCacheMatrix))
# shows the 4 functions set, get, setinverse, and getinverse
# get("sm2", environment(cacheSolve))
# shows the 4 functions set, get, setinverse, and getinverse

# We can see what's in makeCacheMatrix individual function's environments with these commands
# ls(environment(sm$set))
# ls(environment(sm$get))
# ls(environment(sm$setinverse))
# ls(environment(sm$getinverse))
# The output shows the symbols, get, getinverse, i, m, set, and setinverse for each function.

## Write a short comment describing this function
## makeCacheMatrix uses a matrix you specify and creates a special matrix that can cache its inverse
##
## makeCacheMatrix returns a list with functions to 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has been calculated before, it will be retrieved from a cache.  Otherwise, the inverse
## is calculated, cached, and returned.
cacheSolve <- function(m, ...) {
    i <- m$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- m$get()
    i <- solve(data, ...)
    m$setinverse(i)
    i
}
