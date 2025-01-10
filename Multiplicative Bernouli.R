Multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","xj","Uj")
  for (j in 1:n) {
   xj[j,1]<-(a*z0)
   xj[j,2]<-xj[j,1]%%m
   xj[j,3]<-xj[j,2]/m
   z0<-xj[j,2]
  }  
  hist(xj[,3])
  View(xj)
}


Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for(z in 1:i) ifelse(X[z]<=p, Y[z]<-1, Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
View(Bernouli_1(n,p))

z0 = 21139
a = 45
m = 417
n = 150 
p = 0.83
