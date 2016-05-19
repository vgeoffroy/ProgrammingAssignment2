## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

## The first function creates a special matrix, which is a list containing a function to:
## 1) set the value of the matrix "set",
## 2) get the value of the matrix "get", 
## 3) set the value of the inverse "setinverse", 
## 4) get the value of the inverse "getinverse".

## It uses the operator <<- which enables assigning a value to an object in a different environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  # The result of the inversion is stored.
        set <- function(y){  # Function which sets the input matrix.
                x <<- y
                i <<- NULL  # Initialization of i.
        }
        get <- function() x  # Function which returns the input matrix.
        setinverse <- function(inverse) i <<- inverse  # Function which sets the matrix inverse.
        getinverse <- function() i  # Function which returns the matrix inverse.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of a special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it "get"s the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'.
        i <- x$getinverse()  # Get the matrix inverse.
        if(!is.null(i)){  # If the matrix inverse is already known.
                message("getting cached data")
                return(i)  # Return the matrix inverse previously calculated.
        }
        data <- x$get()  # If it is not known, get the matrix object.
        i <- solve(data, ...)  # Calculate the matrix inverse.
        x$setinverse(i)  # Set to the object.
        i  # Print the matrix inverse.
}
