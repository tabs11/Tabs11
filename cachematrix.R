# Tabs11

makeCacheMatrix=function(a=matrix()) {
	if(det(a)==0){
	  print("Matrix not invertible")
	}
	
	else
	inver.matrix=NULL
	set=function(b) {
		a <<- b
    	inver.matrix <<- NULL
  	}
  	get=function() a
  	setinver=function(inverse) inver.matrix <<- inverse
  	getinver=function() inver.matrix
  	list(set = set, get = get,setinver = setinver,getinver = getinver)
	

}

cacheSolve=function(a, ...) {
	inver.matrix=a$getinver()
	# if the inverse has already been calculated
	if(!is.null(inver.matrix)) {
		print("getting cached data")
    	return(inver.matrix)
      	}
	else{
		matrix=a$get()
    		inver.matrix=solve(matrix)
    		a$setinver(inver.matrix)
    	return(inver.matrix)
	}
}

