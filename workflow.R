#generate initial graph set as linear numerical code
a<-alkenum(wordlength,maxedgevec)
#convert codes in a to connectivity matrices, marking ill-written codes with remaining zeros. The matrices are "igraph-ready"
b<-code2cm(a,wordlength)
#Use igraph package to filter out isomorphic graphs
c<-isofilter(b,a)
#display all unique isomers
c[[1]]
