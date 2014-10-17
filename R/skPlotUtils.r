#' Plot several lines
plot.multi.lines = function(mat=NULL,xvar,yvar,ynames=yvar,col=NULL,ylim=NULL,xlab=xvar,ylab="",
        legend.pos=NULL,legend.title=NULL,add=FALSE,lwd=1,...) {
  
  if (is.null(ylim)) {
		ylim = range(mat[,yvar])
  	if (!is.null(legend.pos)) {
      #if (legend.pos=="topleft" | legend.pos == "topright") {
          #ylim[2] = ylim[2]+0.2*diff(ylim)
      #}
    }               		
	}
  ny = NROW(yvar)
  if (is.null(col)) {
    if (ny<=5) {
      col=c("blue","red","green","black","orange")[1:ny]
    } else {
      col = rainbow(ny)
    }
	}

	if (!add) {
  	plot(mat[,xvar],mat[,yvar[1]],type="n",ylim=ylim,xlab=xlab,ylab=ylab,...)
	}
	# Draw lines
 	for (i in 1:NROW(yvar))
		lines(mat[,xvar],mat[,yvar[i]], col=col[i],lwd=lwd)

	# Draw lines once more dotted, so that we can better see lines that are on top of each other better
	if (NROW(yvar)>1) {
 		# Draw lines
 		for (i in (NROW(yvar)-1):1) 
			lines(mat[,xvar],mat[,yvar[i]], col=col[i],lty=2,lwd=lwd)
	}

	# Draw legend, if desired	
	if (!is.null(legend.pos)) {
		legend(legend.pos, legend=ynames, fill=col,title=legend.title)
	}
}

    
#' My wrapper to the lattice function levelplot. Allows for some own color schemes
#' The parameter focus specifies at which z range stronger color changes shall appear
sk.levelplot = function(x=NULL,y=NULL,z=NULL, xnames = NULL, ynames=NULL, grid.xyz = NULL,
                        col.scheme = "darkredgreen", na.col = NULL, at = NULL, at.scheme = "interval",
                        focus = 0,  cuts=15,col.regions=NULL, xlab=NULL,ylab=NULL, 
                        panel = panel.levelplot, zlim=NULL, reverse.colors=FALSE, ...) {
  library(lattice)
  
  store.objects()
  # restore.objects("sk.levelplot")
  
  if (!is.null(z)) {
    z.vec = as.vector(z)
  } else {
    z.vec = grid.xyz[,3]
  }

  # Make cutpoints
  if (is.null(zlim))
    zlim = range(z.vec[is.finite(z.vec)])
    
  if (is.null(at)) {
    if (at.scheme == "interval") {
      at.rel = seq(0,1,length=cuts)
      at.rel = at.rel ^ (abs(focus)^sign(-focus))
      at = zlim[1] + diff(zlim)*at.rel
    } else if (at.scheme == "pretty") {
      at = pretty(z.vec,cuts)
    }
  }
  
  # Select colors
  num.color = cuts+1
  if (col.scheme == "grey") {
    col.regions = grey(seq(0,1,length=num.color))
    if (is.null(na.col)) {
      na.col="darkblue"
      na.col=hsv(1/6,1/2,1/2)
    }
  } else if (col.scheme == "darkredgreen" | col.scheme == "default" ) {
    col.regions = c(rainbow(num.color,start=5/6, end = 1/3,v = (0.3+0.7*(1:num.color)/num.color))) # From Magenta into green
    col.regions = c(rainbow(num.color,start=5/6, end = 1/4,v = (0.3+0.7*(1:num.color)/num.color))) # From Magenta into green

    if (is.null(na.col))
      na.col="grey"
  } else if (col.scheme == "own") {
    col.regions = col.regions[1:num.color]
  } else {
    stop(paste("col.scheme", col.scheme, " not implemented."))
  }
  
  if (reverse.colors) {
    col.regions=rev(col.regions)
  }
  
  # Set NA ROWS below the lowest value and assign NA color
  if (sum(!is.finite(z.vec)) > 0) {
    at = c(zlim[1]-1,at)
    at = c(zlim[1]-3,at)
    col.regions = c(na.col,col.regions)
    if (!is.null(z)) {
      z[!is.finite(z)] = zlim[1]-2
    } else {
      grid.xyz[!is.finite(z.vec),3] = zlim[1]-2
    }
  }
                               
  if (!is.null(z)) {
    if (!is.null(xnames))
      rownames(z) = xnames
    if (!is.null(ynames))
      colnames(z) = ynames
      
    row.values = 1:NROW(z); col.values = 1:NCOL(z)
    if (is.numeric(x))
      row.values=x
    if (is.numeric(y))
      column.values=y
    pl = levelplot(x=z,row.values = row.values, column.values = column.values,
              at=at,col.regions=col.regions,xlab=xlab,ylab=ylab,panel=panel,...)
  } else {
    if (is.null(xlab)) 
      xlab = names(grid.xyz)[1]
    if (is.null(ylab)) 
      ylab = names(grid.xyz)[2]
      
    colnames(grid.xyz) = c("x","y","z")
    grid.xyz = as.data.frame(grid.xyz)
    pl = levelplot(z~x*y,data=grid.xyz, at=at,col.regions=col.regions,xlab=xlab,ylab=ylab,
                   panel=panel,...)
  }
  
  print(pl)
}
