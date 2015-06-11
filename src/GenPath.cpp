#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix GenPath(double nobj, NumericVector loc){
  int nrow_ret = loc.size(),progress=1;
  NumericMatrix rankoutput(nrow_ret,nobj);
  for (int i = 0; i<nobj; ++i){
    rankoutput(0,i) = i+1;
  }
  IntegerVector order_vec = seq_len(int(nobj));
  NumericVector sign_vec(nobj,-1);
  sign_vec(0) = 0;
  int num_unmarked = nobj-1;
  while(num_unmarked != 0){
    
    num_unmarked--;
    progress++;
  }
  return(rankoutput);
}