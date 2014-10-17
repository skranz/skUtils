#' APPROXEQ Are a and b approximately equal (to within a specified tolerance)?
#' p = approxeq(a, b, thresh)
#' 'tol' defaults to 1e-3.
approxeq = function(a, b, tol=1e-3) {
  isTRUE(all.equal(a,b,tol=tol, check.attributes=FALSE))
}

all.eq = function(...) {
  isTRUE(all.equal(...,check.attributes=FALSE))
}

#' Calculates the 2dimensional paretofrontier of the points val1 and val2
#' The function returns the indices of the points that lie on the Pareto Frontier
#' ordered by val1 and val2.
sk.pareto.frontier = function(val1,val2, tol=0, ord = NULL) {
  if (is.null(ord))
    ord = order(val1,val2,decreasing=TRUE)
  
  val2 = val2[ord]
	cummax2 = c(-Inf,cummax(val2[-NROW(val2)]))
	val2.inc = val2 > cummax2 + tol
	ord = ord[val2.inc]	
  return(ord)
}


#' Finds position where the function f becomes zero
#' First tries find.root and if this fails tries optimize
findzero = function(f, lower, upper, tol = .Machine$double.eps*10,result.tol = tol, try.uniroot=TRUE,...) {
  
  if (try.uniroot) {
    ret = tryCatch(uniroot(f,lower=lower,upper=upper,...), error = function(e) NULL)
    if (!is.null(ret)) {
      return(ret$root)
    }
  }

  f.sqr = function(...) f(...)^2
    
  ret = tryCatch(optimize(f.sqr,lower=lower,upper=upper,tol=tol,...), error = function(e) NULL)
  if (is.null(ret)) {
    warning("findzero: error in optimize")
    return(NA)
  } else if (abs(ret$objective)>result.tol) {
    warning("findzero: no solution found with optimize (min = ",ret$objective," > result.tol=",result.tol,")")
    return(NA)
  }
  ret$min
}

#' A wrapper for optimization. Allows to specify which variables shall be free
#' Has the same syntax for one and multidimensional optmization
#' Uses optim, omptimize or a grid search
sk.optim = function(par,f, lower = NULL, upper = NULL, free.par = 1:NROW(par), method="default",
                    num.grid.steps = NULL,maximize=TRUE,f.can.take.matrix = FALSE,tol=.Machine$double.eps^0.25,...) {
  store.objects()
  # restore.objects("sk.optim")
  
  
  n.par = NROW(par)
  if (!is.numeric(free.par)) 
    free.par = which(free.par)
  n.free = NROW(free.par)
  if (n.free == 0) {
    warning("No free parameters!")
    return(list(par=par,value=f(par,...)))
  }
  if (n.free != n.par) {
    sgn = 1
    if (maximize & n.free > 1 & method != "grid")
      sgn = -1
    g = function(x,...) {
      if (is.matrix(x)) {
        mat = matrix(par,NROW(x),NROW(par),byrow=TRUE)
        mat[,free.par] = x
        return(sgn*f(mat,...))
      } else {
        p = par
        p[free.par]=x
        return(sgn*f(p,...))
      }
    }
  } else {
    g = f
  }
  
  if (method=="default") {
    if (free.par == 1 & !is.null(lower)) {
      method = "optimize"
    } else if (!is.null(lower)) {
      method = "L-BFGS-B"
    } else {
      method = "Nelder-Mead"
    }
  }
      
      
  
  if (method == "grid") {
    if (is.null(num.grid.steps))
      stop("You have to specify num.grid.steps if you are using the grid method")
    num.grid.steps = rep(num.grid.steps,length.out=n.par)
    steps.li = lapply(free.par, function(i) seq(lower[i],upper[i],length=num.grid.steps[i]))
    
    par.mat = make.grid.matrix(x=steps.li)
    if (f.can.take.matrix) {
      val = g(par.mat,...)
    } else {
      val = sapply(1:NROW(par.mat), function(ind) g(par.mat[ind,],...))
    }
    if (maximize) {
      opt.ind = which.max(val)
    } else {
      opt.ind = which.min(val)
    }
    par.opt = par
    par.opt[free.par]= par.mat[opt.ind,]
    return(list(par=par.opt, value = val[opt.ind]))
  }
 
  
  # Use optimize
  if (n.free == 1) {
    ret = optimize(g,lower = lower[free.par], upper = upper[free.par],
                  maximum = maximize,tol=tol,...)
    par.opt = par 
    if (maximize) {
      par.opt[free.par] = ret$maximum
    } else { 
      par.opt[free.par] = ret$minimum
    }
    return(list(opt.ret = ret, par=par.opt, value = ret$objective))
  }
  
  if (method == "L-BFGS-B") {
    ret = optim(par[free.par],g,method=method,tol=tol,...)
  } else {
    if (is.null(lower)) {
      ret = optim(par[free.par],g,method=method,tol=tol)
    } else {
      stop("Only methods grid and L-BFGS-B so far implemented for constrained, multivariable optimization")
    }
  } 
  par.opt = par 
  par.opt[free.par] = ret$par
  return(list(opt.ret = ret, par=par.opt, value = ret$value))      
}


#' Helper function to discretize a continous distribution.
#' F.vec is a finite vector containing the value of the cdf at M different points.
#' The function generates an M dimension vector of probabilities summing up to 1
#' that discretize the distribution
discretize.given.F.vec = function(F.vec) {
  M = NROW(F.vec)
  phi = numeric(M)
  phi[1] = F.vec[1] + 0.5 * (F.vec[2]-F.vec[1])
  phi[M] = 1-F.vec[M] + 0.5 * (F.vec[M]-F.vec[M-1])
  if (NROW(F.vec) > 2) {
    ind.left = 1:(M-2)
    ind.mid = ind.left +1
    ind.right = ind.left +2
    phi[ind.mid] = 0.5*((F.vec[ind.mid]-F.vec[ind.left]) + (F.vec[ind.right]-F.vec[ind.mid]))
  }    
  return(phi)  
}

#' Calculate numerically the expected value given a cdf
calc.mean.from.F.fun = function(F.fun,x.min=0,x.max=Inf,abs.tol = 10^(-10),
                            x.seq=NULL,use.num.integrate=TRUE,...) {
  if (x.min >= 0 & use.num.integrate) {
    H.fun = function(x,...) {1-F.fun(x,...)} 
    mx = integrate(H.fun,lower=x.min,upper=x.max,abs.tol=abs.tol,...)$value
    return(mx)
  } else {
    if (is.null(x.seq)) {
      x.seq = seq(x.min,x.max,length=1000)
    }
    F.vec = F.fun(x.seq,...)
  	prob = discretize.given.F.vec(F.vec)
    mx = sum(prob*x.seq)
  }
  mx
} 

