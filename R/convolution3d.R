
conv3d <- function(A, B) {
    dimA = dim(A)[1]
    dimB = dim(B)[1]
    dimC = dimA + dimB
    
    d1 = dim(A)
    d2 = dim(B)
    
    m  <- dim(A)[1]; n  <- dim(A)[2];  p  <- dim(A)[3]
    m1 <- dim(B)[1]; n1 <- dim(B)[2];  p1 <- dim(B)[3]
    
    mn <- d1 + 2 * (d2 - 1)
    
    C = array(0, dim = c(dimC, dimC, dimC))
    #C = array(0, dim = c(m1+m-1, n+n1-1, p+p1-1))
    
    for (x1 in 1:dim(A)[1]) {
        for (x2 in 1:dim(B)[1]) {
            for (y1 in 1:dim(A)[2]) {
                for(y2 in 1:dim(B)[2]) {
                    for (z1 in 1:dim(A)[3]) {
                        for (z2 in 1:dim(B)[3]) {
                            x = x1+x2
                            y = y1 + y2
                            z = z1 +  z2
                            C[x,y,z] = C[x,y,z] + A[x1,y1,z1] * B[x2,y2,z2]
                        }
                    }
                }
                
            }
        }
    }
    # remove all slices that are zero
    d3 <- C[,,apply(C, 3, function(x) !all(x==0))] 
    # remove all columns that are zero
    d2 <- d3[,apply(d3, 2, function(x) !all(x==0)),] 
    # remove all rows that are zero
    d1 <- d2[apply(d2, 1, function(x) !all(x==0)),,] 
    d1
}