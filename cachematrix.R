
## These funtions take an invertable matrix and cashes the Inverse
##  matrix 


##This function creates a special "matrix" object that will cache
##  the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     
     get<-function() x
     setmatrix<-function(solve) m<<- solve
     getmatrix<-function() m
     list(set=set, get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)

}

## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix and caches the result.
## If the reult is already cached then the funtion returns 
## the cached result

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x' matrix
     
     m<-x$getmatrix()   # accesses the object 'x' and get the cached inverted matrix
     
     if(!is.null(m)) {  # if inverted matrix cached (not NULL) then     
          message("getting cached matrix")  # ... send this message to the console
          return(m)                         # ... and return the cached inverted matrix 
     }
     
     matrix<-x$get()         # no cached matrix so get the matrix object
     m <- solve(matrix, ...) # calculate the inverted matrix
     x$setmatrix(m)          # store the inverted matrix object created in makeCacheMatrix
     m                       # return the inverted matrix
}
