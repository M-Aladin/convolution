#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace RcppArmadillo;
using namespace arma;

//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
arma::cube conv3d_cpp(arma::cube img, arma::cube kernel){
    int dimA = img.n_rows;
    int dimB = kernel.n_rows;
    int dimC = dimA + dimB;

    arma::uvec d1(3);
    arma::uvec d2(3);

    // arma::cube out;
    // out.zeros();


    d1[0] = img.n_rows; d1[1] = img.n_cols; d1[2] = img.n_slices;
    d2[0] = kernel.n_rows; d2[1] = kernel.n_cols; d2[2] = kernel.n_slices;

    // cout << "d1=" << d1 << endl;
    // cout << "d2=" << d2 << endl;

    arma::uvec mn = d1 + 2 * (d2 - 1);

    arma::cube out(mn[0], mn[1], mn[2]);
    out.zeros();
    // cout << "mn=" << mn << endl;

    int m = img.n_rows,      n = img.n_cols,     p = img.n_slices;
    int m1 = kernel.n_rows, n1 = kernel.n_cols, p1 = kernel.n_slices;
    // cout << "m,n,p:" << m << n << p << endl;
    // cout << "m1,n1,p1:" << m1 << n1 << p1 << endl;
    int x, y, z;

    for (int x1 = 0; x1 < m; x1++) {
        for (int x2 = 0; x2 < m1; x2++) {
            for (int y1 = 0; y1 < n; y1++) {
                for (int y2 = 0; y2 < n1; y2++) {
                    for (int z1 = 0; z1 < p; z1++) {
                        for (int z2 = 0; z2 < p1; z2++) {
                            x = x1 + x2;
                            y = y1 + y2;
                            z = z1 + z2;
                            out(x, y, z) += img(x1, y1, z1) * kernel(x2, y2, z2);
                            // cout << x1 << x2 << y1 << y2 << z1 << z2 << endl;
                        }
                    }
                }
            }
        }
    }
    return(out);
}




/*** R
A <- array(1:18, dim = c(3, 3, 2))
C <- array(1:16, dim = c(2, 4, 2))

ac <- conv3d_cpp(A, C)
dim(ac)
ac

# G5 = array(1:24576, dim = c(512, 16, 3))     # takes about 4 seconds
# H5 = array(1:2560,  dim = c(64, 8, 5))
# conv3d_cpp(G5, H5)

G3 = array(1:393216, dim = c(512, 64, 3))     # takes about 5 seconds in C++
H3 = array(1:40960, dim = c(64, 32, 5))
conv3d_cpp(G3, H3)

# G1 = array(1:393216, dim = c(512, 256, 3))       # 40 seconds with C++
# H1 = array(1:40960, dim = c(64, 128, 5))
# conv3d_cpp(G1, H1)
*/
