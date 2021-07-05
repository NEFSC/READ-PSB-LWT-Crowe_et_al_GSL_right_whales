Phi_average$occ<-as.factor(Phi_average$occ)
Phi_average$Year<-as.numeric(Phi_average$Year)

phi<-ggplot()+
  geom_errorbar(Phi_average, mapping = aes(x = Year, y = estimate, ymin=lcl, ymax=ucl), width=.3)+
  geom_point(Phi_average, mapping = aes(x = Year, y = estimate), size = 2, alpha = 0.7)+
  theme_bw()+
  ylab("Survival probability")+
  labs(color = "Dataset")+
  ylim(c(0.5,1))+
  theme(panel.grid.minor.x = element_blank())+
  scale_x_continuous(name = "Year", breaks = c(2015,2016,2017,2018,2019), labels = c(2015,2016,2017,2018,2019), limits = c(2015,2019))

FigS2<-ggpubr::ggarrange(phi, C, ncol = 2, widths = c(3,1), labels = "B")

ggsave(filename = 'FigS2.svg',FigS2,device = 'svg', './figures', width = 81, height = 70, units = "mm", dpi = 320)

knitr::include_graphics("./Figures/FigS2.svg")