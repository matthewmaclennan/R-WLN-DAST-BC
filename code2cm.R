#c11 is the output from alkenum()
#This code transforms alkenum() output into connectivity matrices in order to apply isomorphism test. 
code2cm<-function(c11,wordlength){
require(igraph);
c12<-lapply(strsplit(as.character(c11),""),as.numeric)
list1<-list()
for(j in 1:length(c12)){
a<-c();b<-c();c<-c();
	for(i in 1:length(c12[[j]])){
		a<-c(a,rep(i,c12[[j]][i]))
#print(a)
		c<-rep(0,c12[[j]][i])
#print(c)
		if (length(c)==0 && i<wordlength) {b[max(gregexpr("(?<!0)0",paste0(b,collapse=""),perl=T)[[1]])]<-(i+1)} else {if(i==wordlength) {next} else {c[1]<-i+1}}
#print(c)

		b<-c(b,c)
#print(b)
	}
		list1[[j]]<-cbind(a,b)
 }

list1
}
