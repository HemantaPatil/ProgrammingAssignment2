## makeCacheMatrix function creates a special "vector", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<-solve
        getmatrix<-function() m
        list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

## cacheSolve function inverse the matrix of the special "vector" 
## created with the makeCacheMatrix function. 
## it first checks to see if the matrix inverse has already been done. 
## If yes, return matrix from the cache and skips the computation.
## If no, inverse the matrix and sets the value of the matrix in the cache 
## via the setmatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix,...)
        x$setmatrix(m)
        m
}
