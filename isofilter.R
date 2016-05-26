isofilter<-function(b,a){
b2<-b[!unlist(lapply(b,function(x) min(x)==0))]
a2<-a[!unlist(lapply(b,function(x) min(x)==0))]
#turn into igraph objects
list1g<-lapply(b2[unlist(lapply(b2,is.matrix))],function(x) graph.edgelist(x,directed=F))
#plot a selection if you wish
#par(mfrow=c(3,3))
#lapply(list1g,plot)
#Check for automorphs using package::igraph
len<-length(list1g)
im<-seq(1:len)
for(i in 1:len){
	for(j in 1:len){
		if(j>i) {if(i %in% im) {if(length(isomorphisms(list1g[[i]],list1g[[j]]))>0) {im[j]<-i} else {next}} else {next}} else {next}
	}
im
}
endlist<-c()
for(i in unique(im)){
endlist<-c(endlist,max(a2[im==i]))
}
list(endlist,list1g,im)
}
