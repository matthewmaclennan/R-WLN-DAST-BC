code2cm<-function(c11,wordlength){
c12<-lapply(strsplit(as.character(c11)," "),as.numeric)
list1<-list()
for(j in 1:length(c12)){
a<-c();b<-c();c<-c();
	for(i in 1:length(c12[[j]])){


		a<-c(a,rep(i,c12[[j]][i]))
print(a)
		c<-rep(0,c12[[j]][i])
print(c)
		if (length(c)==0) {
			if(i<wordlength){
#ISSUE: index derived from this is not 
#b[max(gregexpr("(?<!0) 0 ",paste0(b,collapse=" "),perl=T)[[1]])+1]<-(i+1)} else {break}} else {c[1]<-i+1}
					b[max(na.omit((seq_along(b)[b!=0]+1)[b[seq_along(b)[b!=0]+1]==0]))]<-(i+1)} else {break}} else {c[1]<-i+1}


print(c)

		b<-c(b,c)
print(b)

	}
		list1[[j]]<-cbind(a,b)
 }
list1
}

