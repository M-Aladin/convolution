#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace RcppArmadillo;
using namespace arma;

//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]

arma::mat cubeMeans(arma::cube X){
    int nSlice = X.n_slices;
    int nCol = X.n_cols;
    int nRow = X.n_rows;

    arma::vec Vtmp(nCol);
    arma::mat Mtmp(nRow, nCol);
    arma::mat Means(nCol, nSlice);
    for (int i = 0; i < nSlice; i++){
        Mtmp = X.slice(i);
        for(int j = 0; j < nCol; j++){
            Vtmp(j) = sum(Mtmp.col(j)) / nRow;
        }
        Means.col(i) = Vtmp;
    }
    return(Means);
}

/*** R
xl <- array(1:10e4, c(20, 10 ,10))
cubeMeans(xl)

*/
