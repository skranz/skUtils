#' Some functions that are useful for manipulating or creating matrices and data.frames and working with lists of vectors, lists of lists or lists of matrices

#' Transforms a matrix into grid in long format
matrix.to.grid = function(mat,x=1:NROW(mat),y=1:NCOL(mat),x.name="x",y.name="y") {
  grid = cbind(expand.grid(x,y),as.numeric(mat))
  colnames(grid) = c(x.name,y.name,"val")
  grid
}

examples.matrix.to.grid = function() {
  mat = matrix(1:6,3,2)
  matrix.to.grid(mat)
}

#' Transforms a grid in long format into a matrix
grid.to.matrix = function(grid,nrow=length(unique(grid[,1])),ncol=length(unique(grid[,2])),val.col=3) {
  mat = matrix(grid[,val.col],nrow,ncol)
  mat
}

examples.grid.to.matrix = function() {
  mat = matrix(1:6,3,2)
  matrix.to.grid(mat)
}

#' rbinds a list of matrices, a list of lists, or a list of vectors into a data.frame (or matrix)
#' each column is a list
#' Assume that all columns in the sublists are in the same order
rbind.list = function(li, cols=NULL, check.common.cols=FALSE) {
  store.objects("rbind.list")
  #restore.objects("rbind.list")
  
  if (length(li)==1) {
    return(li[[1]])
  }
  el1 = li[[1]]
  
  if (check.common.cols) {
 
    if (is.matrix(el1)) {
      names = colnames(li[[1]])
      for (i in 2:length(li)) {
        names = intersect(names,colnames(li[[i]]))
      }
      if (length(names)<1) {
        warning("Different rows of li have no common columns!")
        return(NULL)
      }
      len = sapply(li,NROW,simplify=TRUE)
      mat = matrix(NA,sum(len),length(names))
      colnames(mat) = names
      rowstart = 1
      for (i in 1:length(li)) {
        rowend = rowstart+len[i]-1
        mat[rowstart:rowend,] = li[[i]][,names]
        rowstart = rowend+1
      }
      return(mat)
    } else if (is.list(el1)) {

      names = names(el1)
      for (i in 2:length(li)) {
        names = intersect(names,names(li[[i]]))
      }
      if (length(names)<1) {
        warning("Different rows of li have no common fields!")
        return(NULL)
      }
      ret = as.data.frame(do.call("rbind",li)[,names])
      return(ret)
    } else {
      stop("So far only implemented for lists of matrices and lists of lists!")
    }
  }
  if (is.matrix(el1)) {
    if (is.null(cols)) {
      return(do.call("rbind",li))
    } else {
      return(do.call("rbind",li)[,cols])
    }
  } else {
    if (is.null(cols)) {
      return(as.data.frame(do.call("rbind",li)))
    } else {
      return(as.data.frame(do.call("rbind.data.frame",li)[,cols]))
    }    
  }
}

examples.rbind.list = function() {
  library(restoreDebug)
  
  li1 = list(a=1,b="Hi",c="674")
  li2 = list(a=5,b="how",d=6762)
  li = list(li1,li2)  
  rbind.list(li)

  li1 = list(a=1,b="Hi",c="674")
  li2 = list(a=5,b="how",d=6762)
  li = list(li1,li2)  
  rbind.list(li,check.common.cols=TRUE)
}

#' Paste together columns of a matrix or data.frame
paste.matrix.cols = function(mat,cols=1:NCOL(mat),...) {
  if (NROW(cols)==2) {
    return(paste(mat[,cols[1]],mat[,cols[2]],...))
  } else if (NROW(cols)==3) {
    return(paste(mat[,cols[1]],mat[,cols[2]],mat[,cols[3]],...))
  } else {
    code = paste("mat[,",cols,"]",collapse=",")
    code = paste("paste(",code,",...)",sep="")
    return(eval(parse(text=code)))
  }
}

#' Paste together rows of a matrix or data.frame
paste.matrix.rows = function(mat,rows=1:NROW(mat),...) {
  if (NROW(rows)==2) {
    return(paste(mat[rows[1],],mat[rows[2],],...))
  } else if (NROW(rows)==3) {
    return(paste(mat[rows[1],],mat[rows[2],],mat[rows[3],],...))
  } else {
    code = paste("mat[",rows,",]",collapse=",")
    code = paste("paste(",code,",...)",sep="")
    return(eval(parse(text=code)))
  }
}

#' Add a vector v to each row of m
add.rowvec = function(m,v) {
  return(t(t(m) +v))
} 


#' Generates a matrix in which all rows are equal to row
row.matrix = function(row,col,nrow=length(col),dim=1) {
  matrix(row,nrow=nrow,ncol=length(row),byrow=TRUE)
}
#' Generates a matrix in which all cols are equal to col
col.matrix = function(row=NULL,col,ncol=length(row),dim=2) {
  matrix(col,nrow=length(col),ncol=ncol,byrow=FALSE)  
}
example.col.matrix = function(){
  x = 1:4
  y = 100:102
  row.matrix(x,y)
  col.matrix(x,y)
}

# A function similar to expand.grid, but different ordering of columns
# use should be deprecated!
make.grid.matrix = function(x=lapply(x.dim,function(n) 1:n),x.dim=NULL,n=NULL) {
  store.objects("make.grid.matrix")
  # restore.objects("make.grid.matrix")
  
	if (!is.list(x)) {
  	# Simply a matrix
  	if (is.null(n) & is.null(x.dim)) {
    	return(x)
  	}
  	
		mat = matrix(NA,nrow=NROW(x)^n,ncol=n)
		for (i in 1:n) {
			mat[,i] = rep( rep(x,each=NROW(x)^(n-i)), times = NROW(x)^(i-1))
	  }
	  return (mat)
  } else {
	  n = length(x)
	  if (is.null(x.dim)) {
	  	x.dim = sapply(x,length)
  	}
	  mat = matrix(NA,nrow=prod(x.dim),ncol=n)
	  x.dim = c(1,x.dim,1,1)
		for (i in 1:n) {
			mat[,i] = rep( rep(x[[i]],each=prod(x.dim[(i+2):(n+2)])), times = prod(x.dim[1:i]))
	  }
	  return (mat)
	}
}



#' Gives the corresponding rows for a permutated grid.matrix given a permutation
#' x.perm of the elements of the original list x 
grid.matrix.permutation = function(x,perm.col) {
  store.objects("grid.matrix.permutation")
  # restore.objects("grid.matrix.permutation")
  
	
	stopifnot(is.list(x))
	x.dim = sapply(x,length)

	x.dim.perm = x.dim[perm.col]
	gm = make.grid.matrix(x.dim=x.dim)
	rows.perm = rep(0,NROW(gm))
	
	nc = length(x.dim)
	if (nc==1) {
		return(1:NROW(gm))
	}
	for (mycol in 1:(nc-1)) {
		rows.perm = rows.perm + (gm[,perm.col[mycol]]-1)*prod(x.dim.perm[(mycol+1):nc])
	}
	rows.perm = rows.perm + gm[,perm.col[nc]]
	return(rows.perm)
}

examples.grid.matrix.permutation = function() {
  x= list(c("A","B"),c("a","b"),c("0","1"))
  perm.col = c(3,1,2)
  x.perm =x[perm.col]
 
  gm.x = make.grid.matrix(x)
  gm.perm = make.grid.matrix(x.perm)
 
  rows.perm = grid.matrix.permutation(x,perm.col)
 
  cbind(gm.x,gm.perm[rows.perm,])
}
