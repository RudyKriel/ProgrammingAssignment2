
## the makeCacheMatrix function allows the calling script to store the inverted matrix in cache
## this is used in conjuction with the cacheSolve function
## the makeCacheMatrix function should be called once while the matrix we are inverting and 
## working with the same unchanged matrix. If the matrix changes this must be called again

##a list of functions to retrieve and store the matrix provided and its inversion
makeCacheMatrix <- function(x = matrix()) {
      
      inverseM <- NULL
      
      set <- function(y) {
            x <<- y
            inverseM <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inv) inverseM <<- inv
      getinverse <- function() inverseM
      ##Return the list of functions
      list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


##A function to invert the matrix if not inverted yet or just retrieve from cache if already inverted
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverseM <- x$getinverse()
      
      ##if the stored inverse matrix is not null 
      ##just return the inverse matrix in cache 
      
      if (!is.null(inverseM)){
            
            message("get cached inverted matrix")
            return(inverseM)
      }
      
      ##else do the inversion, store it in cache and return the newly inverted matrix
      message("cache the data")
      dataM <- x$get()
      inverseM <- solve(dataM)
      x$setinverse(inverseM)
      return(inverseM)
}

##Recources used in this assignment:
##1. http://www.johnmyleswhite.com/notebook/2009/12/16/quick-review-of-matrix-algebra-in-r/
##2. http://rpubs.com/yelangya3826850/CacheTime-consumingComputations
##3. http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/
