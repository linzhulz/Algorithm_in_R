'''
DDL:
Nov 22 at 11:59pm 

Requirements:
Please refer to the 2-D shortest path algorithm handput to perform the following implementation

in R script:

1) Generate 20 pairs of (x, y) coordinates using randoamm generation commands in R.

2) Draw Median Line to cut into 2 parts s1 s2.

3) Find the shortest distance and its 2-pair coordinates in s1.

4) Find the shortest distance and its 2-pair coordinates in s2.

5) Define the area shape surrounding the Median line.

6) Find the shortest distance and its 2-pair coordinates across Median line.

7) Find the shortest distance among 3), 4), and 6)
'''

#####################
# 1) Generate 20 pairs of (x, y) coordinates
#####################
set.seed(1)
pairs = matrix(sample(-500:500, size = 20*2, replace = TRUE), nrow =20, ncol =2)

#####################
# 2)-7) The closestPair function
#####################
# function define the distance
dist = function(p, q){
  return(sqrt((p[1] - q[1])^2 + (p[2] - q[2])^2))
}
# function define the band situation, for task 2), 5), 6), 
bandmin = function(ps, m, theta){
  band <- ps[ps[,1] > (m - theta) & ps[,1] < (m + theta),]
  bandmin = Inf
  if(length(band) <= 2){
    return(Inf)
  }else if(length(band[,2]) < 8){
    band = band[order(band[,2], decreasing = FALSE), ]
    for(i in 1:(length(band[,2]) - 1)){
      for(j in (i + 1):length(band[,2])){
        d = dist(band[i,], band[j,])
        bandmin = min(d, bandmin)
      }
    }
  }else{
    for(i in 1:(length(band[,2] - 7))){
      for(j in (i + 1):(i + length(band[,2] - 1))){
        d = dist(band[i,], band[j,])
        bandmin = min(d, bandmin)
      }
    }
  }
  return(bandmin)
}

# main function
closestPair <- function(ps){
  if(length(ps) <= 2){
    mindist = Inf
    return(mindist)
  }else if(length(ps[,1]) == 2){
    mindist = dist(ps[1,], ps[2,])
    return(mindist)
  }else{
    ps = ps[order(ps[,1], decreasing = FALSE), inplace = TRUE]
    m = median(ps[,1])
    pl = ps[ps[,1] <= m, ] # pairs on left plane
    pr = ps[ps[,1] > m, ] # pairs on left plane
    # find the shortest distance in left and right plane
    thetal = closestPair(pl)
    thetar = closestPair(pr)
    theta = min(thetal, thetar)
    theta_band = bandmin(ps, m, theta)
    mindist = min(theta, theta_band)
    return(mindist)
  }
}

closestPair(pairs)
# output: 28.63564