## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Special Matrix (list containing funcions set and get)
makeCacheMatrix <- function(x = matrix()) {
    #s will be the solve
    s <- NULL
    
    #setting matrix
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    
    #getting matrix
    get <- function() x
    
    #setting solve (inverse) of matrix
    setsolve <- function(solve) s <<- solve
    
    #getting solve of matrix
    getsolve <- function() s
    
    #list of functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


#Calculation of the inverse
cacheSolve <- function(x, ...) {

    s <- x$getsolve()
    
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    
    s <- solve(data, ...)
    
    x$setsolve(s)
    
    #Return matrix inverted
    s
}

