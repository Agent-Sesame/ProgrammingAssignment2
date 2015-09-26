## If function makecacheMatrix supplied with pre-existing matrix 'x' from global
## environment it will cache the inverse of 'x' using the 'solve' function.
## Quickly create a text inversable matrix using a 3rd function 'makeMatrix'.

## Example matrix 'x' supplied to makecacheMatrix: 
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## Expected inverse of 'x' output from cacheSolve:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## These three functions running on my home platform conform to expected output.

makecacheMatrix <- function(x = matrix()) {
        
        ## Set variable 'm' representing matrix inverse to NULL in parent 
        ## environment.
        
        m <- NULL
        
        ## In parent environment set variable 'set' to a function, define that
        ## function's current environment to 1) replace 'x' with 'y' in the 
        ## main function makecacheMatrix parent environment, and 2) sets 
        ## variable 'm' to NULL in parent environment.
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## In parent environment set variable 'get' to a function, defining that
        ## function to return 'x'.
        
        get <- function() x
        
        ## In parent environment set variable 'setinverse' to the function
        ## 'solve' and set variable 'm' from current environment to 'solve' in
        ## parent environment.
        
        setinverse <- function(solve) m <<- solve
        
        ## In parent environment set variable 'getinverse' to function that
        ## returns value of 'm'.
        
        getinverse <- function() m
        
        ## Listify all parent environment variables established above.
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Function intends to take a argument 'x' defined as matrix output from 
## function 'makeCacheMatrix( )' and solve the inverse of that matrix

cacheSolve <- function(x, ...) {
        
        ## In parent environment set variable 'm' to stored function $getinverse
        
        m <- x$getinverse()
        
        ## If in parent environmnet 'm' is not null, display message on screen
        ## and return cached value 'm'.
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## In parent environment set variable 'data' to stored function 
        ## $get, the stored matrix.
        
        data <- x$get()
        
        ## In parent environment set variable 'm' to the function solve() with
        ## the variable 'data' as argument.
        
        m <- solve(data)
        
        ## In parent environmet call listed function 'setinverse'.
        
        x$setinverse(m)
        
        ## Return 'm'
        
        m
}

makeMatrix <- function(mdim = 2) {
        
        ## Quickly generate test object in global environment using argument
        ## 'mdim' as variable for matrix data, row, and column definition.
        
        x <<- matrix(rnorm(mdim ^ 2), nrow = mdim, ncol = mdim)
        
        return (x)
        
}