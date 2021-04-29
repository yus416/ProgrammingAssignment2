## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special list containing functions to:
# set the matrix - set()
# get the matrix - get()
# set the inverse of matrix - setinverse()
# get the inverse of matrix - getinverse()


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        #return list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## makeCacheMatrix does not compute inverse but it can set inverse with setinverse(). 
#The inverse is computed by the function cacheSolve for a matrix set by set() function of makeCacheMatrix.
#If the inverse has already been calculated or set, then the cacheSolve retrieves the inverse from the cache. 
#Computing the inverse of a square matrix was done with the solve function in R. 


cacheSolve <- function(x, ...) {
        ## get inverse matrix
        m <- x$getinverse()
        #if it was already set with setinverse, return cached matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get square matrix
        data <- x$get()
        #compute the inverse of square matrix
        m <- solve(data, ...)
        #set the inverse
        x$setinverse(m)
        m
}


## USAGE EXAMPLES
#create test matrix
testm <- matrix(c(3,1,4,2), ncol=2)
#build a set of functions and return the functions within a list to the parent environment
testf <- makeCacheMatrix()
#initially there is no matrix input
testf$get()
#nothing is returned from cach or computed
cacheSolve(testf)
#set the test matrix
testf$set(testm)
#inspect the test matrix
testf$get()
#compute the inverse
cacheSolve(testf)
#get the inverse
testf$getinverse()
