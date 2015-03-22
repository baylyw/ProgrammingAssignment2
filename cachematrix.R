## This program calculates the inverse of a matrix. 
## If it is called to calculate the inverse of the same matrix twice in a row it reports the cached inverse instead of 
## calculating it again

## The function makeCacheMatrix returns a set of four functions that are used to store and cache the inverse of a matrix
## and are important for determining whether the inverse has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        # inv <- NULL sets inverse value to NULL as a placeholder
        inv <- NULL
        # defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, inv, to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # returns the matrix, x
        get <- function(){
                x 
        } 
        # sets the inverse, inv to inverse
        setInv <- function(inverse){
                inv <<- inverse
        } 
        getInv <- function() {
                inv
        }
        # returns the 'special vector' containing all of the functions just defined.
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function calls the functions stored in makeCacheMatrix, checks to see if the inverse has already been cached,
## and reports either the new or cached inverse. 

cacheSolve <- function(x, ...) {
        # x is a list of functions and the matrix from makeCacheMatrix
        # assign to inv in cacheSolve, the value from getInv(inv) 
        inv <- x$getInv()
        #If the inverse stored under the parameters "matrix x" is not NULL, return it.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # assign to data the vector x
        data <- x$get()
        # Calculate the inverse and assign it to inv
        inv <- solve(data, ...)
        # Store the inverse under the parameters "matrix x".
        x$setInv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv     
}
