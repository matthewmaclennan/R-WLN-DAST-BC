alkenum<-function(wordlength,maxedgevec){
digsum<-wordlength-1
#put the following vectors in terms of the input wordlength
veci<-seq(maxedgevec,2,-1)
vecsi<-seq(maxedgevec-1,0,-1)
vec<-seq(maxedgevec-1,0,-1)
vecsf<-c(1,0)
vecf<-0
list2<-c(list(veci,vecsi),rep(list(vec),wordlength-4),list(vecsf,vecf))
c9<-as.numeric(apply(expand.grid(list2),1,function(x) paste0(x,collapse="")))
#keep digit sum = wordlength-1
digitsum <- function(x) {
					options(scipen=999)
					sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)
				}
c10<-c9[sapply(c9,digitsum)==digsum]
#First digit is the largest digit and not appearing elsewhere after the first digit.
c11<-c10[unlist(lapply(strsplit(as.character(c10),""),function(x) x[1]==max(x) & !(x[1] %in% x[-1])))]
#Ensure that the number zero does not appear second except if the first digit = digsum.
if(wordlength>6) c11<-c11[-grep("^.0",c11)]
#Ensure that the only entry beginning with 2 is the entry followed by wordlength-3 number 1s, or digsum-2 number 1s.
c11<-as.character(sort(c(c11[!grepl("^2",c11)],c11[grepl(paste0("^21{",digsum-2,"}"),c11)]),decreasing=T))
}