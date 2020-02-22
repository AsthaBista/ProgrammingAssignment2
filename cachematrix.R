## This program lets you store the inverse of a matrix in the parent
## environment and obtain the cached inverse of matrix using
## lexical scoping. This code consists of two main function.
## The makeCacheMatrix function creates a special object containing
## a set of functions that help in setting and getting the matrix and
## the inverse of that matrix. The second function cacheSolve helps in
## calculating the inverse of the matrix if the previous value is null
## and helps in setting the correct value for later use.

## This function creates four main functions: set(), get(), setInverse(),
## and getInverse(). This function also creates objects x and i.
## Here, x is a invertible square matrix.

makeCacheMatrix <- function(x = matrix()) {
        ##Set the value of inverse i to NULL
        i <- NULL
        ## Set the value of x in y and i to NULL and store in parent environment
        set <- function(y) {
          x <<- y
          i <<- NULL
        }

        ## The get() function returns the value of x
        get <- function() x

        ## Create functions to set and get inverse of matrix x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i

        ## List the functions in a more readable format
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function checks if there is a previous value of inverse.
## It uses the functions in the makeCacheMatrix function to
## get the matrix. Then it calculates the inverse of matrix and
## then it sets the inverse in the parent environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()

        ## If the inverse is NULL, it returns the calculated inverse
        ## If the inverse is not NULL, it prints the given message
        ## and returns inverse
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
