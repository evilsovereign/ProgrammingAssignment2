## Here are two functions created for RProgramming programming assignment 2. 
## These functions will take a given invertible matrix, solve for the inverse of the given matrix, 
## and cache that inverse matrix for future calculations.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {             ## input x will be a matrix
        m <- NULL                                       ## m will be solved matrix, reset to NULL every time makeVector is called
        set <- function(y) {                            ## takes input matrix y
                x <<- y                                 ## saves input matrix through superassignment
                m <<- NULL                              ## resets solved matrix to NULL
        }
        get <- function() {x}                           ## returns value of original matrix
        setinv <- function(solve) {m <<- solve}         ## called by cacheSolve during first access, store the value using superassignment
        getinv <- function() {m}                        ## return the cached value to cacheSolve on subsequent accesses
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)                           ## list of internal functions ('methods') so a calling 
                                                        ## function knows how to access those methods
}


## The function cacheSolve calculates the inverse of the matrix returned from the makeCacheMatrix function.
## This function checks to see if the inverse has already been calculated (and the given matrix has not changed).
## If the inverse has already been calculated, the function returns the inverse matrix that was stored in cache

cacheSolve <- function(x, ...) {                        ## function with input x, x is object created by makeCacheMatrix
        m <- x$getinv()                                 ## accesses object x and get the inverse matrix, assigns to 'm'
        if(!is.null(m)) {                               ## checks to see if m already has cached value (m not NULL)
                message("getting cached data")          ## return this message if m has cached value
                return(m)                               ## return the inverse matrix
        }
        data <- x$get()                                 ## gets the original matrix and stores it in 'data'
        m <- solve(data, ...)                           ## solve for the inverse matrix, store in 'm'
        x$setinv(m)                                     ## store the solved matrix
        m                                               ## return inverse matrix
}
