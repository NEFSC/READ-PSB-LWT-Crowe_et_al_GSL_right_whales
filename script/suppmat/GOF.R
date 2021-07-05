library(R2ucare) #Gimenez et al. 2018 

GOF_CJS<-function(x){
  
  n<-as.matrix(x[1])
  n[n==0]<-1
  
  overall_gof<-overall_CJS(as.matrix(x), n)
  trap<-test2ct(as.matrix(x), n) # test of trap dependence 
  trans<-test3sr(as.matrix(x), n) # test of transience
  # tests of over-dispersion
  od1<-test2cl(as.matrix(x), n)  
  od2<-test3sm(as.matrix(x), n) 
  
  list(overall_gof=overall_gof,trap=trap,trans=trans,od1=od1,od2=od2)
}

#MRAS 2017
GOF_CJS(ch.MRAS17)

#MRAS 2018
GOF_CJS(ch.MRAS18)

#MRAS 2019
GOF_CJS(ch.MRAS19)

#All data 2017
GOF_CJS(ch.alldata_s17)

#All data 2018
GOF_CJS(ch.alldata_s18)

#All data 2019
GOF_CJS(ch.alldata_s19)

#All years
GOF_CJS(ch.allyears)