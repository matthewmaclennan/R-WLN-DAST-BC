a<-cbind(c(0,1,2,3),c(0,0,0,0))
rotmat2d<-function(r){
	matrix(c(cos(r*pi/180),-sin(r*pi/180),sin(r*pi/180),cos(r*pi/180)),ncol=2,byrow=T)

}
rn<-c(6,6,6,6)
ang<-lapply(360/rn,function(x) x*1:5)

loopy<-function(a,ang){
for(i in 1:length(ang)){
	for(j in 1:length(ang[[i]])){
list1[[j]]<-cbind(a[,1:(i-1)],(rotmat2d(ang[[i]][j])%*%(a-a[,(i-1)])[,i])+a[,(i-1)])

	}
list1
}
list1
}

list1r<-lapply(list1,function(x) round(x,3))
lapply(lapply(list1,function(x) round(x,3)),function(y) duplicated(t(y)))
