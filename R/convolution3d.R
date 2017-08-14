
#' Convolution 3D for arrays
#' @param img the object to convolve
#' @param kernel the kernel
#' @export
conv3d <- function(img, kernel) {
    dimA = dim(img)[1]
    dimB = dim(kernel)[1]
    dimC = dimA + dimB

    d1 = dim(img)
    d2 = dim(kernel)

    m  <- dim(img)[1]; n  <- dim(img)[2];  p  <- dim(img)[3]
    m1 <- dim(kernel)[1]; n1 <- dim(kernel)[2];  p1 <- dim(kernel)[3]

    mn <- d1 + 2 * (d2 - 1)
    #out = array(0, dim = c(dimC+4, dimC+4, dimC+4))
    out = array(0, dim = mn)

    for (x1 in 1:m) {
        for (x2 in 1:m1) {
            for (y1 in 1:n) {
                for(y2 in 1:n1) {
                    for (z1 in 1:p) {
                        for (z2 in 1:p1) {
                            x = x1 + x2
                            y = y1 + y2
                            z = z1 + z2
                            out[x,y,z] = out[x,y,z] + img[x1,y1,z1] * kernel[x2,y2,z2]
                            #cat(x,y,z, "\n")
                        }
                    }
                }

            }
        }
    }
    out
    # remove all slices that are zero
    d3 <- out[,,apply(out, 3, function(x) !all(x==0))]
    # remove all columns that are zero
    d2 <- d3[,apply(d3, 2, function(x) !all(x==0)),]
    # remove all rows that are zero
    d1 <- d2[apply(d2, 1, function(x) !all(x==0)),,]
    d1
}
