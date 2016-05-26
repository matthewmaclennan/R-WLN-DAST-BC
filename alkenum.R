alkenum<-function(wordlength,maxedgevec){
digsum<-wordlength-1
#put the following vectors in terms of the input wordlength
veci<-seq(maxedgevec,2,-1)
vecsi<-seq(maxedgevec-1,0,-1)
vec<-seq(maxedgevec-1,0,-1)
vecsf<-c(1,0)
vecf<-0
list2<-c(list(veci,vecsi),rep(list(vec),wordlength-4),list(vecsf,vecf))
#list2
#c9<-as.numeric(apply(expand.grid(list2),1,function(x) paste0(x,collapse=" ")))
c9<-as.matrix(expand.grid(list2))
#keep digit sum = wordlength-1
#digitsum <- function(x) {
#					options(scipen=999)
#					sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)
#				}
#c10<-c9[sapply(c9,digitsum)==digsum]
c10<-c9[rowSums(c9)==digsum,]
#First digit is the largest digit and not appearing elsewhere after the first digit.
#c11<-c10[unlist(lapply(strsplit(as.character(c10)," "),function(x) x[1]==max(x) & !(x[1] %in% x[-1])))]
c11<-c10[c10[,2]!=0,]
c11<-c11[apply(c11,1,function(x) x[1]==max(x) & !(x[1] %in% x[-1])),]
#collapse to string
c11<-apply(apply(c11,1,function(x) paste(x,"")),2,function(y) paste0(y,collapse=""))
regex<-function(maxedgevec){
it<-c()
for (i in 2:(maxedgevec-1)){
	for(j in 3:maxedgevec){
#		it<-c(it,paste0("^.1+00+[^0]|^.1+0[^01]|^.1+0.+[^01]+|^.",i,"0{1,",(i-1),"}[^0]|^",j,i,"0{",i,"}0{1,",(j-1),"}[^0]"))
		it<-c(it,paste0("^. (1 )+0 (0 )+[^0]|^. (1 )+0 (. )*[^01]+|^. ",i," (0 ){1,",(i-1),"}[^0]|^",j," ",i," (0 ){",i,"}(0 ){1,",(j-1),"}[^0]"))
	}
}
it<-paste0(it,collapse="|")
}
c11<-c11[-grep(regex(maxedgevec),c11,perl=T)]
#c11<-c11[-grep("^. 0",c11,perl=T)]
if(maxedgevec==(wordlength-1)) {c11<-c(paste0(min(c(wordlength-1,maxedgevec))," ",paste0(rep("0 ",(wordlength-1)),collapse=""),collapse=""),c11)}
c11<-as.character(sort(c(c11[!grepl("^2 ",c11,perl=T)],c11[grepl(paste0("^2 (1 ){",digsum-2,"}"),c11,perl=T)]),decreasing=T))
}
