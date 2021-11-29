
##MRAS 2019

#parameters for eight occasions
npar_c8o<-function(x){
  x%>%
    mutate(npar = case_when( #correct parameter count
      model == "Phi(~1)p(~time)pent(~time)" ~ 16,
      model == "Phi(~time)p(~time)pent(~time)" ~ 21,
      model == "Phi(~time)p(~1)pent(~time)" ~ 16,
      model == "Phi(~1)p(~1)pent(~time)" ~ 10
    ))
}

JStablem19<-MRAS19.aic$model.table %>% mutate(Dataset = "MRAS")
JStable_npar_c<-npar_c8o(JStablem19)
m19_AICc<-AICtoAICc(JStable_npar_c,nrow(ch.MRAS19))

m1<-MRAS19.aic$Phi.dot.p.time.pent.time$results$reals
m2<-MRAS19.aic$Phi.time.p.time.pent.time$results$reals
m3<-MRAS19.aic$Phi.dot.p.dot.pent.time$results$reals
m4<-MRAS19.aic$Phi.time.p.dot.pent.time$results$reals

N_m1<-m1$N*m19_AICc$weight.c[1]
N_m2<-m2$N*m19_AICc$weight.c[2]
N_m3<-m3$N*m19_AICc$weight.c[3]
N_m4<-m4$N*m19_AICc$weight.c[4]
N_avg_m19<-N_m1%>%bind_rows(N_m2)%>%bind_rows(N_m3)%>%bind_rows(N_m4)%>%summarise_all(sum)

#################
##all data 2019
JStable<-alldata_s19.aic$model.table%>%mutate(Dataset = "All data$_{s}$")
JStable_npar_c<-npar_c8o(JStable)
ad19_AICc<-AICtoAICc(JStable_npar_c,nrow(ch.alldata_s19))

ad1<-alldata_s19.aic$Phi.dot.p.time.pent.time$results$reals
ad2<-alldata_s19.aic$Phi.time.p.time.pent.time$results$reals
ad3<-alldata_s19.aic$Phi.time.p.dot.pent.time$results$reals
ad4<-alldata_s19.aic$Phi.dot.p.dot.pent.time$results$reals

N_ad1<-ad1$N*ad19_AICc$weight.c[1]
N_ad2<-ad2$N*ad19_AICc$weight.c[2]
N_ad3<-ad3$N*ad19_AICc$weight.c[3]
N_ad4<-ad4$N*ad19_AICc$weight.c[4]
N_avg_ad19<-N_ad1%>%bind_rows(N_ad2)%>%bind_rows(N_ad3)%>%bind_rows(N_ad4)%>%summarise_all(sum)
