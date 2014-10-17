#include <Rcpp.h>

using namespace Rcpp; using namespace std;
 

extern "C" {
  SEXP C_which_colMins ( SEXP mat );
}

SEXP C_which_colMins ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
    int n = X.ncol();
    Rcpp::IntegerVector V(n);
    for (int i=0; i<n; i++) {
       Rcpp::NumericVector W = X.column(i);
       V[i] = std::distance(W.begin(),std::min_element(W.begin(), W.end())) +1;  // from the STL
    }
   return(V);
  
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_which_rowMins ( SEXP mat );
}

SEXP C_which_rowMins ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
	int n = X.nrow();
	Rcpp::IntegerVector V(n);
	for (int i=0; i<n; i++) {
	Rcpp::NumericVector W = X.row(i);
	V[i] = std::distance(W.begin(),std::min_element(W.begin(), W.end())) +1;  // from the STL
	}
	return(V);
	
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_colMins ( SEXP mat );
}

SEXP C_colMins ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
    int n = X.ncol();
    Rcpp::NumericVector V(n);
    for (int i=0; i<n; i++) {
       Rcpp::NumericVector W = X.column(i);
       V[i] = *std::min_element(W.begin(), W.end());  // from the STL
    }
   return(V);
  
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_rowMins ( SEXP mat );
}

SEXP C_rowMins ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
    int n = X.nrow();
    Rcpp::NumericVector V(n);
    for (int i=0; i<n; i++) {
       Rcpp::NumericVector W = X.row(i);
       V[i] = *std::min_element(W.begin(), W.end());  // from the STL
    }
   return(V);
  
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_which_colMaxs ( SEXP mat );
}

SEXP C_which_colMaxs ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
	int n = X.ncol();
	Rcpp::IntegerVector V(n);
	for (int i=0; i<n; i++) {
	Rcpp::NumericVector W = X.column(i);
	V[i] = std::distance(W.begin(),std::max_element(W.begin(), W.end())) +1;  // from the STL
	}
	return(V);
	
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_which_rowMaxs ( SEXP mat );
}

SEXP C_which_rowMaxs ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
	int n = X.nrow();
	Rcpp::IntegerVector V(n);
	for (int i=0; i<n; i++) {
	Rcpp::NumericVector W = X.row(i);
	V[i] = std::distance(W.begin(),std::max_element(W.begin(), W.end())) +1;  // from the STL
	}
	return(V);
	
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_colMaxs ( SEXP mat );
}

SEXP C_colMaxs ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
	int n = X.ncol();
	Rcpp::NumericVector V(n);
	for (int i=0; i<n; i++) {
	Rcpp::NumericVector W = X.column(i);
	V[i] = *std::max_element(W.begin(), W.end());  // from the STL
	}
	return(V);
	
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}
extern "C" {
  SEXP C_rowMaxs ( SEXP mat );
}

SEXP C_rowMaxs ( SEXP mat ) {
//
		try {
	
//
    Rcpp::NumericMatrix X(mat);
	int n = X.nrow();
	Rcpp::NumericVector V(n);
	for (int i=0; i<n; i++) {
	Rcpp::NumericVector W = X.row(i);
	V[i] = *std::max_element(W.begin(), W.end());  // from the STL
	}
	return(V);
	
//
	}  catch(  std::exception  &ex  )  {
		forward_exception_to_r(  ex  );
	}  catch(...)  {
		::Rf_error(  "c++  exception  (unknown  reason)"  );
	}	
	
  Rf_warning("your C program does not return anything!");
  return R_NilValue;
}

