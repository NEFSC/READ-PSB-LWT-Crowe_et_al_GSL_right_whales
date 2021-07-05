
## all years 2015–2019

#parameters for five occasions
npar_c5o<-function(x){
  x%>%
    mutate(npar = case_when( #correct parameter count
      model == "Phi(~1)p(~time)pent(~time)" ~ 10,
      model == "Phi(~time)p(~time)pent(~time)" ~ 12,
      model == "Phi(~time)p(~1)pent(~time)" ~ 10,
      model == "Phi(~1)p(~1)pent(~time)" ~ 7
    ))
}


JStable<-allyears.aic$model.table %>% mutate(Dataset = "All data")
JStable_npar_c<-npar_c5o(JStable)
all_AICc<-AICtoAICc(JStable_npar_c,nrow(ch.allyears))

all1<-allyears.aic$Phi.dot.p.time.pent.time$results$reals
all2<-allyears.aic$Phi.time.p.time.pent.time$results$reals
all3<-allyears.aic$Phi.time.p.dot.pent.time$results$reals
all4<-allyears.aic$Phi.dot.p.dot.pent.time$results$reals

N_all1<-all1$N*all_AICc$weight.c[1]
N_all2<-all2$N*all_AICc$weight.c[2]
N_all3<-all3$N*all_AICc$weight.c[3]
N_all4<-all4$N*all_AICc$weight.c[4]
N_avg_allyrs<-N_all1%>%bind_rows(N_all2)%>%bind_rows(N_all3)%>%bind_rows(N_all4)%>%summarise_all(sum)

##survival estimate
Phi_all1<-(all1$Phi*all_AICc$weight.c[1])
Phi_all1_2<-((all2$Phi%>%dplyr::select(-time,-occ))*all_AICc$weight.c[2])%>%mutate(occ = 1:4, e1 = Phi_all1$estimate, se1 = Phi_all1$se, l1 = Phi_all1$lcl, u1 = Phi_all1$ucl)
Phi_average<-Phi_all1_2%>%
                mutate(est_a = estimate + e1,
                       se_a = se + se1,
                       lcl_a = lcl + l1,
                       ucl_a = ucl + u1)%>%
                dplyr::select(occ, est_a,se_a,lcl_a,ucl_a)%>%
                mutate(Year = c("2015.5","2016.5","2017.5","2018.5"))
Phi_all2<-(all2$Phi%>%dplyr::select(-time,-occ))%>%
  mutate(Year = c("2015.5","2016.5","2017.5","2018.5"))  
names(Phi_average)<-c("occ","estimate","se","lcl","ucl","Year")
