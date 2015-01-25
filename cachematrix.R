##This fucntion creates a special matrix object which is actually a list
##containing functions:
makeCacheMatrix <- function(x = matrix()) {
    #Return a list of functions.
    
    #Instantiate the object to NULL
    inv <- NULL
    
    #set the value of the matrix.
    set <- function(y)
    {
        #superassignment of x - this ensures that when the set function is 
        #called the incoming matrix (y), is assigned to variable x which is 
        #defined outside the set function environment.
        x <<- y
        
        #If the matrix changes or is initiated the inverse value is set to NULL
        #This basically invalidates the cached value if it existed and will 
        #force a recalculation of the inverse.
        inv <<- NULL
    }
    
    #get the value of the matrix.
    get <- function() x
    
    #set the value of the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    #get the value of the inverse of the matrix
    getinverse <- function() inv
    
    #list containing the functions above.
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This function calculate the inverse of the special matrix created by the 
##function above.  However, it first checks if the inverse has already been
##calculated If so, it gets the inverse from the cache. Otherwise, it calculates
##the inverse of the data and sets the value of the inverse in the cache via the
##setinverse function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #Get the existing inverse value.
    inv <- x$getinverse()
    
    #Check if inverse have already been calculted, if so, it returns the cached
    #value.
    if(!is.null(inv))
    {
        message("getting cached data")
        
        return(inv)
    }
    
    #Get the matrix.
    data <- x$get()
    
    #Assume the matrix is square and culculate the inverse. 
    inv <- solve(data)
    
    #Set the inverse value.
    x$setinverse(inv)
    
    inv    
}