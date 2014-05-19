
#  In this Script we made a pair a functions to allow caching of pre-computed results , in this case the precomputed
#  result is de Inverse of a given matrix 



## With this function is created one object that includes the matrix itself and the inverseM  , also have functions to set and get 
## the inverse of the matrix , but don't actually do the computation , only return the previous saved/set value
## at the begin the value of inverseM is set to NULL , to allow the deteccion of not computed value

makeCacheMatrix <- function(x = matrix()) {
        inverseM<-NULL   # initialization of inverseM to NULL 
        set <-function(y){
                x<<-y     # set value for matrix
                m<<-NULL  # set value inverseM to NULL 
        }
        get <-function() x
        setInverse <-function(inv) inverseM<<-inv  # save value of inverseM
        getInverse <- function() inverseM          # return value of inverseM
        # list that contains the functions
        list(set=set,get=get,setInverse=setInverse,
                getInverse=getInverse)
}


## Write a short comment describing this function

# this function is the one that allows to encapsulate the compute / caching of the solve computation
# returns the inverse of the matrix , the first time the computation is done , next calls to this function 
# with the same matrix returns the precomputed value computed an stored at the first call
cacheSolve <- function(x, ...) {
        
        ## get the previous stored value for inverse 
        inverseM<-x$getInverse()
        # test if stored value !NULL 
        if(!is.null(inverseM)){
                # success caching of computed result
                message("getting cached data")
                return(inverseM)
        }
        # must be done computation
        inverseM<-solve(x$get())
        # computation result is stored for caching 
        x$setInverse(inverseM)
        # computation result is returned
        inverseM
}

