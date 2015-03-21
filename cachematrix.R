## The 1st function "makeCacheMatrix" accepts a square matrix and creates a list (consisting four functions)
## that can be passed as an argument to 2nd function "cacheSolve." The 1st time cacheSolve 
## receives this argument, it will calculate the inverse of the original matrix and cache it in a fixed
## environment. The 2nd time cacheSolve is passed the same argument it will retrieve the cached inverse
## and return that.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #creates a list of 4 functions
        inverse <- NULL #initializes local variable "inverse" and sets it to NULL
        set <- function(y) { #caches the variable passed into it in specific environment
                x <<- y 
                inverse <<- NULL #initializes variable "inverse" in specific environment
        }
        get <- function() x #assigns get the local value of "x" -- the orginal matrix.
        setinverse <- function(solve) inverse <<- solve #caches argument in variable in specific environment
        getinverse <- function() inverse #gets cached value of inverse from specific environment
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse) #names the list's elements (functions)
}


## takes as an argument the list of functions created in makeCacheMatrix and uses them
## to see if the inverse of a square matrix has been cached. If so, it retrieves and returns inverse
## If not, it calculates inverse, caches it and returns it

cacheSolve <- function(x, ...) {
                inverse <- x$getinverse() #assigns the value of $getinverse() (either null or matrix's inverse)
                if(!is.null(inverse)) {
                        message("getting cached data") #if inverse != null, return inverse
                        return(inverse)
                }
                data <- x$get() #otherwhise sets variable data with original matrix from x$get()
                inverse <- solve(data, ...) #passes matrix as arg to function solve, sets variable "inverse"
                x$setinverse(inverse) #caches inverse of matrix
                inverse #returns inverse
                print("Here is the value of inverse")
                print(inverse)
        }
