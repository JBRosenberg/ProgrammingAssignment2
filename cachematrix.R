## Pair of functions that create special "matrix" object
## Which contains the matrix and its inverse, if computed,
## Second function solves and set the matrix inverse


##Takes matrix as arg and creates "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  
  i<-NULL
  
  #Set function caches object's initial values
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  ##Function for retrieving the matrix of the object
  get<-function(){x}
  
  
  ##Function to set inverse of object
  setinverse<-function(inverse){i<<-inverse}
  
  
  ##Function to retrieve inverse of object
  getinverse<-function(){i}
  
  
  ##Allow to subset functions
  list(set = set, get = get, setinverse = setinverse,
       getinverse=getinverse)
}


## Function takes "matrix object" and calculates its inverse
## If not previously calculated
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  
  ##Check to see if object inverse is set, if not calculate and set
  if(is.null(inv))
  {
    d<-x$get()
    inv<-solve(d)
    x$setinverse(inv)
    return(inv)
  }
  
  ##If inverse already set, return cached value
  else
  {
    return(inv)
  }
  
  
}
