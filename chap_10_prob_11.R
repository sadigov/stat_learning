#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
#
# EPage 431
#
#-----

save_plots = F

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
# 
DF = read.csv("Ch10Ex11.csv",header=FALSE)
DF = t(DF) # want each row to represent a sample ... should have n=40 samples/rows

# Part (b):
#
D = dist(DF)                  # "n x n matrix of Euclidean distance dissimilarities"
D = as.dist( 1 - cor(t(DF)) ) # cor computes the correlation of *columns* so we need to take the transpose of DF
hclust.cor = hclust( D, method="complete" )
#hclust.cor = hclust( D, method="average" )
#hclust.cor = hclust( D, method="single" )

# How well does our clustering predict health vs. diseased:
# 
print( table( predicted=cutree( hclust.cor, k=2 ), truth=c( rep(0,20), rep(1,20) ) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_11_dendrogram.eps", onefile=FALSE, horizontal=FALSE) }
plot( hclust.cor, xlab="", sub="", cex=0.9 )
if( save_plots ){ dev.off() }

# Part (c):
# 
# Compute the unpaired t-test between the means of the gene response in each cluster: 
#
predicted=cutree( hclust.cor, k=2 ) 

n1 = apply( DF[ predicted==1, ], 2, length ) # the number of samples (number of patients in each cluster)
n2 = apply( DF[ predicted==2, ], 2, length )

m1 = apply( DF[ predicted==1, ], 2, mean ) # the means across the 1000 genes in each cluster
m2 = apply( DF[ predicted==2, ], 2, mean )

v1 = apply( DF[ predicted==1, ], 2, var ) # the variances across the 1000 genes in each cluster
v2 = apply( DF[ predicted==2, ], 2, var )

pooled_variance = sqrt( v1 / n1 + v2 / n2 )

t_value = ( m1 - m2 ) / pooled_variance 

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_11_t_value.eps", onefile=FALSE, horizontal=FALSE) }
plot( t_value, xlab="gene index", ylab="unpaired t-value" )
if( save_plots ){ dev.off() }





