## this fuction gets a matrix and returns a list of functions
## the matrix object has functions below: 
##set: inputs a matrix 
##get: returns a matrix
##get_inverse: outputs the cached inverted matrix
##set_inverse: caches an already calculated inverted matrix (by function: cachesolve)

makeCacheMatrix <- function(x = matrix()) 
{

          inv=NULL
          set<-function(y)
          {
                      x<<-y
                      inv<<-NULL
                 
          }
          get<-function () x
          set_inverse<-function(i) inv<<-i
          get_inverse<-function() inv
          list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
    

  
 }


## This function checks if the inversion already calculated. If yes, retrieves 
## the cached inverted matrix. If no it calculates the inverted matrix using solve(x)
##and returns i : the calculated inverted matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i<-x$get_inverse()
        if(!is.null(i))
        {
                    message("getting cached data")
                    return(i)
          
        }
        matrix<-x$get()
        i<-solve(matrix)
        x$set_inverse(i) 
        i
  
}
