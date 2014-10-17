#' Some functions that are useful for lists and environments in particular generating, transforming, copying and assigning values


#' Creates a list that is named by the names of its arguments
named.list = function(...) {
  li = list(...)
  li.names = names(li)
  
  names = unlist(as.list(match.call())[-1])
  if (!is.null(li.names)) {
    no.names = li.names == ""
    names(li)[no.names] = names[no.names]
  } else {
    names(li) = names
  }
  li
}
nlist = named.list

#' Copies an environment
copy.env = function(dest=sys.frame(sys.parent(1)),source=sys.frame(sys.parent(1)),
                    names = NULL, name.change = NULL, exclude=NULL) {
  #store.objects()  # DO NOT STORE OBJECTS!!!!
  #restore.objects("copy.env")
  
  if (is.null(name.change)) {
    if (is.environment(source)) {
      if (is.null(names))
        names = ls(envir=source)
      names = setdiff(names,exclude)
      for (na in names) {
        assign(na,get(na,envir=source), envir=dest)
      }
    }
    if (is.list(source)) {
      if (is.null(names))
        names = names(source)
      names = setdiff(names,exclude)
      for (na in names) {
        assign(na,source[[na]], envir=dest)
      }
    }
  } else {
    if (is.environment(source)) {
      if (is.null(names))
        names = ls(envir=source)
      names = setdiff(names,exclude)
      for (na in names) {
        ind = match(na,name.change[,1])
        if (!is.na(ind)) {
          assign(name.change[ind,2],get(na,envir=source), envir=dest)
        } else {         
          assign(na,get(na,envir=source), envir=dest)
        }
      }
    }
    if (is.list(source)) {
      if (is.null(names))
        names = names(source)
      names = setdiff(names,exclude)
      for (na in names) {
        ind = match(na,name.change[,1])
        if (!is.na(ind)) {
          assign(name.change[ind,2],source[[na]], envir=dest)
        } else {         
          assign(na,source[[na]], envir=dest)
        }
      }
    }
  }      
}

#' Assigns all columns of df into variables with the same name in environment env
assign.cols  = function(df, dest=sys.frame(sys.parent(1))) {
  if (is.matrix(df)) {
    df = as.data.frame(df)
  }
  li = as.list(df)
  copy.into.env(dest=dest,source=df)
}

examples.assign.cols = function() {
  df = data.frame(a = 1:10,b=runif(10,0,1))
  env = new.env()
  assign.cols(df,env)
  a = 0; b= 0;
  assign.cols(df)
  eval(expression(a*b),envir=env)
}

copy.into.env = function(dest=sys.frame(sys.parent(1)), source=sys.frame(sys.parent(1)), names = NULL, name.change = NULL,exclude=NULL) {
  copy.env(dest,source,names,name.change,exclude)
}

copy.into.list = function(dest=NULL, source=sys.frame(sys.parent(1)), names = NULL,exclude=NULL,overwrite=TRUE) {
  if (is.null(dest)) {
    if (is.list(source)) {
      return(source)
    } else {
      dest=list()
    }
  }
  stopifnot(is.list(dest))
  
  if (!is.null(overwrite)) {
    if (!overwrite) {
      exclude = c(exclude,names(dest))
    }
  } 
  if (is.environment(source)) {
    if (is.null(names))
      names = ls(envir=source)
    
    names = setdiff(names,exclude)
    if (!is.null(overwrite)) {
      for (na in names) {
        dest[[na]]=get(na,envir=source)
      }
    } else {
      for (na in names) {
        if (is.null(dest[[na]]))
          dest[[na]]=get(na,envir=source)
      }
    }
  }
  if (is.list(source)) {
    if (is.null(names))
      names = names(source)
    names = setdiff(names,exclude)
    if (!is.null(overwrite)) {
      for (na in names) {
        dest[[na]]=source[[na]]
      }
    } else {
      for (na in names) {
        if (is.null(dest[[na]]))
          dest[[na]]=source[[na]]
      }
    }
  }
  return(dest)
}

examples.copy.into.list = function () {
  dest = list(a=5,b=NULL)
  source = list(a="A",b="B",c=1)
  copy.into.list(dest=dest,source=source,overwrite=TRUE)
  copy.into.list(dest=dest,source=source,overwrite=FALSE)
  copy.into.list(dest=dest,source=source,overwrite=NULL)
} 

#' Clones an environment and its children
clone.environment = function(env, made.clones=as.environment(list(org = list(), copy = list())), clone.parents=TRUE, clone.global = FALSE, exclude = NULL, clone.children = TRUE) {
  
  penv = parent.env(env)
  #Clone parents
  if (clone.parents & (!(identical(penv,emptyenv())
                         | (identical(penv,globalenv()) & ! clone.global)))) {
    cpenv = clone.environment(penv, made.clones = made.clones, clone.parents = TRUE)
  } else {
    cpenv <- penv
  } 
  
  if (length(made.clones$org) > 0) {
    for (i in 1:length(made.clones$org)) {
      if (identical(made.clones$org[[i]],env)) {
        return(made.clones$copy[[i]])
      }
    }
  }
  
  cenv = new.env(parent=cpenv)
  
  ind = length(made.clones$org)+1
  made.clones$org[[ind]] = env
  made.clones$copy[[ind]] = cenv
  
  #Clone children
  names  = setdiff(ls(env),exclude)
  #browser()
  if (clone.children) {
    for (na in names) {
      
      # If an error occurs here, the variable [[
      obj = env[[na]]
      
      
      if (is.environment(obj)) {
        obj = clone.environment(obj, made.clones = made.clones, clone.parents = TRUE, clone.global = clone.global)
        cenv[[na]] <- obj
      } else { 
        cenv[[na]] <- obj
      }
    }
  } else {
    for (na in names) {
      cenv[[na]] <- env[[na]]
    }
  }    
  class(cenv) <- class(env)
  return(cenv)  
}

clone.env = function(...) clone.environment(...)

currentenv = function() {
  sys.parent(1)
}

#' Need to check what it does
set.default = function(env,name,x, overwrite.null = TRUE, inherits = TRUE) {
  if (is.environment(env)) {
    if (!exists(name,envir=env, inherits = inherits)) {
      env[[name]] <- x
    } else {
      if (overwrite.null & is.null(env[[name]])) {
        env[[name]] <- x
      }
    }
    return(env)
  } else if (is.list(env)) {
    if (overwrite.null) {
      if (is.null(env[[name]])) {
        env[[name]] <- x
      }
    } else if (!(name %in% names(env))) {
      env[[name]] <- x
    }
    return(env)
  }
}


