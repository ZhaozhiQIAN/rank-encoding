#include <Rcpp.h>
#include <math.h>
#include <utility>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix GenPath(double nobj, NumericVector loc){
  int nrow_ret = loc.size(),progress=1;
  NumericMatrix rankoutput(nrow_ret,nobj);
  for (int i = 0; i<nobj; ++i){
    rankoutput(0,i) = i+1;
  }
  int current_row=1;
  IntegerVector order_vec = seq_len(int(nobj));
  NumericVector sign_vec(nobj,-1);
  sign_vec(0) = 0;
  int num_unmarked = nobj-1;
  int largest,largest_index;
  int dir;
  while(num_unmarked != 0){
    // finds the largest element with a nonzero direction
    largest = 0;
    largest_index = 0;
    for (int i=0; i<nobj; ++i){
      if (order_vec(i)>largest && sign_vec(i)!=0){
        largest = order_vec(i);
        largest_index = i;
      }
    }
    // swap elements and sign_vec
    dir = sign_vec(largest_index);
    std::swap(order_vec(largest_index),order_vec(largest_index+dir));
    std::swap(sign_vec(largest_index),sign_vec(largest_index+dir));
    largest_index += dir;
    // if need to unmark element
    if (largest_index == 0 || largest_index == nobj-1){
      sign_vec(largest_index) = 0;
      --num_unmarked;
    } else if (largest < order_vec(largest_index+sign_vec(largest_index))){
      sign_vec(largest_index) = 0;
      --num_unmarked;
    }
    // reset sign
    for (int i=0; i<nobj; ++i){
      if (order_vec(i)>largest){
        if(i<largest_index){
          sign_vec(i) = 1;
          ++num_unmarked;
        } if (i > largest_index){
          sign_vec(i) = -1;
          ++num_unmarked;
        }
      }
    }
    progress++;
    // record ranking
    if (std::find(loc.begin(),loc.end(),progress)!=loc.end()){
      for (int i=0;i<nobj;++i){
        rankoutput(current_row,i) = std::find(order_vec.begin(),order_vec.end(),i+1)-order_vec.begin()+1;
      }
      ++current_row;
    }
  }
  return(rankoutput);
}
