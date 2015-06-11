#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix FindV(NumericMatrix obs, NumericVector pi0){
  NumericMatrix ret(obs.nrow(),obs.ncol()-1);
  NumericVector pi0_perm(pi0.size());
  NumericVector pi0_dummy(pi0.size());
  
  int index;
  for (double i=0; i<pi0.size(); ++i){
    index = int(pi0(i)-1);
    pi0_perm(index) = i; 
  }
  for (int i=0;i<obs.nrow();++i){
    pi0_dummy=seq_len(pi0.size());
    for (int j=0;j<ret.ncol();++j){
      ret(i,j) = abs(obs(i,pi0_perm(j))-pi0_dummy(j));
      for (int k=j; k<ret.ncol();++k){
        if (obs(i,pi0_perm(k)) < obs(i,pi0_perm(j))){
          --pi0_dummy(k);
        }
      }
    }
  }
  
  return(ret);
}

// [[Rcpp::export]]
NumericMatrix EncodeV(NumericMatrix obs, NumericVector pi0){
  int num_v_len = obs.ncol()-1;
  int ret_col = 0;
  int* log_loc;
  log_loc = new int[obs.ncol()];
  log_loc[0] = 0;
  for (int i=obs.ncol(); i>1; --i){
    ret_col += ceil(log2(i));
    log_loc[obs.ncol()-i+1] = ret_col;
  }
  NumericMatrix ret(obs.nrow(),ret_col);
  NumericVector pi0_perm(pi0.size());
  NumericVector pi0_dummy(pi0.size());
  
  int num_v_curr;
  int index;
  for (double i=0; i<pi0.size(); ++i){
    index = int(pi0(i)-1);
    pi0_perm(index) = i; 
  }
  for (int i=0;i<obs.nrow();++i){
    pi0_dummy=seq_len(pi0.size());
    for (int j=0;j<num_v_len;++j){
      num_v_curr= abs(obs(i,pi0_perm(j))-pi0_dummy(j));
      
      for (int k=log_loc[j+1]-1;k>log_loc[j]-1;--k){
        ret(i,k) = num_v_curr % 2;
        num_v_curr = num_v_curr/2;
      }
      
      
      for (int k=j; k<num_v_len;++k){
        
        if (obs(i,pi0_perm(k)) < obs(i,pi0_perm(j))){
          --pi0_dummy(k);
        }
      }
    }
  }
  
  return(ret);
}
