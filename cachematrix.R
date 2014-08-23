
## makeCacheMatrix function creates and inverse the matrix and cache it

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
