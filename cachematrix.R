## Togther these functions takes a square matrix and returns the inverse of 
##that martix.  

## The makeCacheMatrix function takes a square matrix as it argument and returns 
## a list of function which are use to store the original matrix and its inverse
## when it is calculated. 


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## The cacheSolve function takes a the list object created from the 
## MakeCacheMatrix function as its argument and checks to see if the inverse 
## has already been caluclated and stored in the list (see line 33). 
## If the inverse is already stored it prints the message 'getting cached data' 
## and returnes the cached inverse. 
## If it isn't calculated then it calculated the inverse if the matrix and 
## stores it in the list object. 

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
                        }
        data <- x$get()
        inverse <- solve(data,)
        x$setinverse(inverse)
        inverse
}


