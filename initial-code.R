
makeObjectsComparableByDimensionNames <- function(lst, dim=c(1,2), index.return=FALSE){
	if(length(dim)==1 & dim[1]==1){
		stop("This function require more than 1 dimension")
	}

	#check that all dimensions exist in all provided objects
	if(!sum(unlist(lapply(lst, function(x){length(dim(x))})) == length(lst)) == length(lst)){
		stop("all elements need to have the same dimensions")
	}

	idn <- .intersectDimensionNames(lst, dim)
	idi <- .intersectDimensionIndex(lst, idn, dim)
	mdi <- .matchIntersectedDimensionIndex(lst,idi, dim)
	
	if(!index.return){
		#return reduced object (can only deal with two dim right now)
		.applyIntersectionAndMatch(lst, idi, mdi, dim)
	}else{
		list(intersectDimensionIndex=idi, matchIntersectedDimensionIndex=mdi)
	}
}

#has to be of length >1
.intersectDimensionNames <- function(lst, dim=c(1,2)){
	lapply(dim, function(x,lst){
		Reduce(intersect,
			lapply(lst, function(y,x){
				dimnames(y)[[x]]
			},x)
		)
	},lst)
}

#returns index for elements to keep
.intersectDimensionIndex <- function(lst, idi, dim=c(1,2)){
		lapply(dim, function(x, lst, idi){
			lapply(lst, function(y,x, idi){
				which(dimnames(y)[[x]] %in% idi[[x]])
			},x, idi)
			
		},lst, idi)
}

#returns list with matches against the first element for all dimensions
.matchIntersectedDimensionIndex <- function(lst, idi, dim=c(1,2)){
		lapply(dim, function(x, lst, idi){
			z <- dimnames(lst[[1]])[[x]][idi[[1]][[x]]]

			lapply(1:length(lst), function(y, x, z, idi, lst){
				match(z, dimnames(lst[[y]])[[x]][idi[[y]][[x]]])
			}, x, z, idi, lst)

		},lst, idi)
}

#return sub-set (this function limits the subset to only two dim, please make better)
.applyIntersectionAndMatch <- function(lst, idi, mdi, dim=c(1,2)){

		#dim 1 - rows
		if(1 %in% dim){
			lst <- lapply(1:length(lst), function(x, lst, idi, mdi){
				lst[[x]][idi[[x]][[1]],][mdi[[x]][[1]],]
			}, lst, idi, mdi)
		}

		#dim 2 - cols
		if(2 %in% dim){
			lst <- lapply(1:length(lst), function(x, lst, idi, mdi){
				lst[[x]][,idi[[x]][[2]]][,mdi[[x]][[2]]]
			}, lst, idi, mdi)
		}

}


#live test
lst.m <- makeObjectsComparableByDimensionNames(list(rv, a.red), dim=2)
