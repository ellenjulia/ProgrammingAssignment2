## The following two functions work in tandem to streamline the computation of matrix inverses. 
## The makeCacheMatrix function enables the caching of the inverse of a matrix once it has been 
## computed so that repeated computation of the inverse is not necessary. The cacheSolve function,
## which takes the list returned by makeCacheMatrix as its argument, first determines whether the 
## inverse has been previously cached and then either returns the cached value or computes the 
## inverse, storing this value in the list for later use. 



## makeCacheMatrix takes a matrix as its argument (the default value is the empty matrix) and
## returns a list consisting of four functions: set(), get(), setinv(), and getinv(). 

## For example, one can call "obj <- makeCacheMatrix()" and "obj" is now a list of the four 
## functions, taking the empty matrix as input. If m is a matrix, one can use "obj$set(m)" to 
## set the value of the matrix, "obj$get()" to retrieve the value of the matrix, 
## "obj$setinv(inverse)" to manually set the value of the inverse to "inverse", and "obj$getinv()" 
## to retrieve the value of the inverse. Alternately, calling "obj <- makeCacheMatrix(m)" both 
## creates  the list  of functions and sets the value of the matrix. "obj$getinv()" returns NULL 
## unless the  inverse has been previously cached for the particular matrix m (any new call to the 
## function makeCacheMatrix() or obj$set() will result in obj$getinv() returning NULL).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## Any new call to makeCacheMatrix will reset inv to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL ## Setting a new matrix will reset inv to NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse  ## When setinv is called, its argument is set to inv
    getinv <- function() inv  ## Returns the value of inv when called
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) ## The list object names are chosen to be identical to the functions 
                          ## stored.
}


## cacheSolve takes the list "set, get, setinv, getinv" from makeCacheMatrix as its argument. 
## It retrieves the value of the inverse stored by makeCacheMatrix(). If this is not NULL (if an 
## inverse of the matrix has been previously cached), it returns this value with no additional 
## computation. If no matrix inverse has been cached, it computes the inverse, stores it for later
## use, and returns the computed value. Further calls to the same list will contain the cached value
## of the inverse. Hence the same inverse need not be computed more than once. 

## For example, we first create an appropriate input for cacheSolve using the matrix m (whose 
## inverse needs to exist) by calling "obj <- makeCacheMatrix(m)". The first call "cacheSolve(obj)" 
## will compute the inverse, cache it, and return the value (assuming that we have not used 
## "obj$setinv()" to manually set the value of the inverse). When we call "cacheSolve(obj)" again, 
## it will return the cached value. If the list obj is modified to correspond to a different matrix,
## either by calling makeCacheMatrix() again or obj$set(), the cached value of the inverse is reset 
## to NULL. 

## WARNING: cacheSolve does not identify whether the value retrieved via obj$getinv() is correct. 
## Manually setting the value of the inverse via "obj$setinv("num")" will cause cacheSolve(obj) to
## return "num". 

cacheSolve <- function(x, ...) {
    inv <- x$getinv() ## Retrieve the value of the inverse, which defaults to NULL in the absence 
                      ## of a cached value
    ## Check to see if there is a previously cached inverse. If so, tell the user a cached value 
    ## is being retrieved and return the cached inverse. 
    if(!is.null(inv)) {  
        message("getting cached data")  
        return(inv)  
    }
    ## If the cached value is NULL, retrieve the matrix, compute the inverse, cache the inverse for 
    ## future calls, and return the value of the inverse. 
    data <- x$get()  
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
