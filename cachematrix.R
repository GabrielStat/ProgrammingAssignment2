## Put comments here that give an overall description of what 
## your functions do

## Write a short comment describing this function

## 1. As the instructor said, a good programer is lazy so 
## in my pursuit of perfection :) I have introduced minimum 
## changes the original code

## 2. Thanks to all participants of the thread 
## "Possible use of makeVector() and cachemean() functions - code 
## explained" without which it would be REALLY difficult to 
## understand all of this

## Thanks guys!!!

## creating a function which has a matrix (mtrx) as an input and ## a list "set", "get", "setinv", "getinv"
## as an uotput and being able to cache the inverse matrix

makeCacheMatrix <- function(mtrx = matrix()) {
	  i <- NULL
        set <- function(y) {
                mtrx <<- y  ## assigning y to mtrx that will be 						 ## available outside of this current 						 ## environment
                i <<- NULL
        }
	   ## creating functions that will be directly available 		   
	   ## from "cacheSolve" function
        get <- function() mtrx
        setinv <- function(inv) i <<- inv
        getinv <- function() i

	   ## creating an output object: list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(ObjMatrix, ...) {
        ## Return a matrix that is the inverse of 'ObjMatrix'
        i <- ObjMatrix$getinv() ## geting the inverse matrix
        if(!is.null(i)) { ## case when the reverse matrix was 
                          ## cached
                message("getting cached data")
                return(i)
        }
        data <- ObjMatrix$get() ## getting the input matrix to 							## calculate the reverse one
        i <- solve(data) ## calculating the inverse matrix using 					    ## "solve" function
        ObjMatrix$setinv(i) ## calling the function to cache the 						  ## inverse matrix
        i
}


