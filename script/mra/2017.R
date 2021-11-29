
##MRAS 2017

#parameters for four occasions
npar_c4o<-function(x){
  x%>%
    mutate(npar = case_when( #correct parameter count
      model == "Phi(~1)p(~time)pent(~time)" ~ 8,
      model == "Phi(~time)p(~time)pent(~time)" ~ 9,
      model == "Phi(~time)p(~1)pent(~time)" ~ 8,
      model == "Phi(~1)p(~1)pent(~time)" ~ 6
      ))
}

JStable<-MRAS17.aic$model.table %>% mutate(Dataset = "MRAS")
JStable_npar_c<-npar_c4o(JStable)
m17_AICc<-AICtoAICc(JStable_npar_c,nrow(ch.MRAS17))

m1<-MRAS17.aic$Phi.dot.p.time.pent.time$results$reals
m2<-MRAS17.aic$Phi.time.p.time.pent.time$results$reals
m3<-MRAS17.aic$Phi.time.p.dot.pent.time$results$reals
m4<-MRAS17.aic$Phi.dot.p.dot.pent.time$results$reals

N_m1<-m1$N*m17_AICc$weight.c[1]
N_m2<-m2$N*m17_AICc$weight.c[2]
N_m3<-m3$N*m17_AICc$weight.c[3]
N_m4<-m4$N*m17_AICc$weight.c[4]
N_avg_m17<-N_m1%>%bind_rows(N_m2)%>%bind_rows(N_m3)%>%bind_rows(N_m4)%>%summarise_all(sum)

##all data 2017
JStable<-alldata_s17.aic$model.table %>% mutate(Dataset = "All data$_{s}$")
JStable_npar_c<-npar_c4o(JStable)
ad17_AICc<-AICtoAICc(JStable_npar_c,nrow(ch.alldata_s17))

ad1<-alldata_s17.aic$Phi.dot.p.time.pent.time$results$reals
ad2<-alldata_s17.aic$Phi.time.p.time.pent.time$results$reals
ad3<-alldata_s17.aic$Phi.time.p.dot.pent.time$results$reals
ad4<-alldata_s17.aic$Phi.dot.p.dot.pent.time$results$reals

N_ad1<-ad1$N*ad17_AICc$weight.c[1]
N_ad2<-ad2$N*ad17_AICc$weight.c[2]
N_ad3<-ad3$N*ad17_AICc$weight.c[3]
N_ad4<-ad4$N*ad17_AICc$weight.c[4]
N_avg_ad17<-N_ad1%>%bind_rows(N_ad2)%>%bind_rows(N_ad3)%>%bind_rows(N_ad4)%>%summarise_all(sum)
