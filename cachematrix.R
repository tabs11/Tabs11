# Tabs11

makeCacheMatrix=function(a=matrix()) {
	#a: square invertible matrix
	if(det(a)==0){
	  print("Matrix not invertible")
	}
	
	else
	inver.matrix=NULL
	#set the value of the matrix
	set=function(b) {
		a <<- b
    	inver.matrix <<- NULL
  	}
  	#get the value of the matrix
  	get=function() a
  	#set the value of inverse of the matrix
  	setinver=function(inverse) inver.matrix <<- inverse
  	#get the value of inverse of the matrix
  	getinver=function() inver.matrix
  	#this list is used as the input to cacheSolve()
  	list(set = set, get = get,setinver = setinver,getinver = getinver)
	

}

cacheSolve=function(a, ...) {
	#a:output of makeCacheMatrix()
        inver.matrix=a$getinver()
	# if the inverse has already been calculated
	if(!is.null(inver.matrix)) {
		# get it from the cache and skips the computation. 
		print("getting cached data")
    	return(inver.matrix)
      	}
      	## otherwise, calculates the inverse 
	else{
		matrix=a$get()
    		inver.matrix=solve(matrix,...)
    		# sets the value of the inverse in the cache.
    		a$setinver(inver.matrix)
    	#return: inverse of the original matrix input to makeCacheMatrix()
       	return(inver.matrix)
	}
}

