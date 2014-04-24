makeCacheMatrix <- function(x=matrix()) {
  inverse = NULL         #This object stores the inverse of the matrix, set to NULL bydefault
  set <- function(y){
    x <<- y
    inverse <<- NULL
  } #this function sets variable x to y in the global environment, then sets inverse to NULL
  get<-function() x #prints x
  setsolve<-function(solve) inverse <<- solve #this function calculates the inverse
  getsolve <- function() inverse #prints the inverse
  list(set=set,
       get=get,
       setsolve=setsolve,
       getsolve=getsolve) #the list of functions to be returned
}

cachesolve <- function(x, ...){
          inverse <- x$getsolve () #sets inverse to the values calculated before
          if (!is.null(inverse)){
            message("Getting cached data")
            return(inverse)
          } #tests if the matrix changed, if not, returns cache
          
          data<-x$get()
          inverse <- solve(data, ...) 
          x$setsolve(inverse)
          inverse #if the matrix changed, it will be assigned to the data variable then the inverse recalculated and printed 
}