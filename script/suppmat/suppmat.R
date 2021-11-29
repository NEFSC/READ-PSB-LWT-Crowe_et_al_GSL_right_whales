efforttl<-read.csv('./data/effort_timeline.csv', header = T, stringsAsFactors = F)

efforttl<-efforttl%>%
  mutate(first_date = ymd(paste0(Year,'-',First,'-01')),
         last_date = ceiling_date(ymd(paste0(Year,'-',Last,'-30')), "month") - 1)%>%
  pivot_longer(cols = c(first_date, last_date),names_to = 'span',
               values_to = 'date')%>%
  mutate(jday = yday(date))%>%
  arrange(Type, date)

efforttl$Type<-factor(efforttl$Type, levels = c("MWSS","SDSS","SVAS","SLAS","MRAS","BSAS"))
myCol<-c("black","#7570b3","#d95f02","#e7298a","grey40","#1b9e77")
sup_map_fill = c("BSAS" = myCol[1],"SDSS" = myCol[4], "MRAS" = myCol[3], "MWSS" = myCol[2],"SVAS" = myCol[5],"SLAS" = myCol[6])

eff_tl<-ggplot(efforttl)+
  geom_line(efforttl%>%filter(!(Type == 'MRAS' & Last == 'September')), mapping = aes(x = jday, y = Type, group = Type, color = Type), size = 3, alpha = 0.8)+
  geom_line(efforttl%>%filter(Type == 'MRAS' & Last == 'September'), mapping = aes(x = jday, y = Type, group = Type), color = 'white', size = 3)+
  facet_grid(~Year)+
  scale_x_continuous(breaks = c(91,121,152,182,213,244,274,305,335,365), labels = c("Apr","May","June","July","Aug","Sep","Oct","Nov","Dec","Jan (next year)"))+
  theme_bw()+
  ylab('Survey Type')+
  xlab('')+
  theme(panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = sup_map_fill)+
  scale_color_manual(values = sup_map_fill)

FigS1B<-ggpubr::ggarrange(eff_tl,labels = "B")
ggsave(filename = 'FigS1B.svg',FigS1B,device = 'svg', './figures', width = 169, height = 60, units = "mm", dpi = 320)

######
#MRAS
suppmap_MRAS<-read.csv('./data/MRAS supp map.csv', header = T, stringsAsFactors = F)
suppmap_MRAS$DATETIME_ET<-ymd_hms(suppmap_MRAS$DATETIME_ET)

suppmap_MRAS<-suppmap_MRAS%>%
  mutate(date = as.factor(substr(DATETIME_ET, 1,10)),
         MONTH = as.numeric(month(DATETIME_ET)))%>%
  arrange(DATETIME_ET)

#####
#MWSS (NEA/CWI)

suppmap_MWSS15<-read.csv('./data/NEA2015.csv', header = T, stringsAsFactors = F)%>%mutate(date = ymd(paste0(YEAR,'-',MONTH,'-', DAY)))%>%dplyr::select(date, YEAR, EVENTNO, LATITUDE, LONGITUDE, PLATNAME, MONTH)
suppmap_MWSS16<-read.csv('./data/NEA2016.csv', header = T, stringsAsFactors = F)%>%mutate(date = ymd(paste0(YEAR,'-',MONTH,'-', DAY)))%>%dplyr::select(date, YEAR, EVENTNO, LATITUDE, LONGITUDE, PLATNAME, MONTH)
suppmap_MWSS17<-read.csv('./data/NEA2017.csv', header = T, stringsAsFactors = F)%>%mutate(date = ymd(paste0(YEAR,'-',MONTH,'-', DAY)))%>%dplyr::select(date, YEAR, EVENTNO, LATITUDE, LONGITUDE, PLATNAME, MONTH)
suppmap_MWSS18<-read.csv('./data/NEA2018.csv', header = T, stringsAsFactors = F)%>%mutate(date = ymd(paste0(YEAR,'-',MONTH,'-', DAY)))%>%dplyr::select(date, YEAR, EVENTNO, LATITUDE, LONGITUDE, PLATNAME, MONTH)
suppmap_MWSS19<-read.csv('./data/NEA2019.csv', header = T, stringsAsFactors = F)%>%mutate(date = ymd(paste0(YEAR,'-',MONTH,'-', DAY)))%>%dplyr::select(date, YEAR, EVENTNO, LATITUDE, LONGITUDE, PLATNAME, MONTH)

suppmap_MWSS<-suppmap_MWSS15%>%
  bind_rows(suppmap_MWSS16)%>%
  bind_rows(suppmap_MWSS17)%>%
  bind_rows(suppmap_MWSS18)%>%
  bind_rows(suppmap_MWSS19)%>%
  filter(!is.na(YEAR))%>%
  filter(LATITUDE > 46)%>%
  mutate(platdate = paste0(PLATNAME,date),
         MONTH = as.numeric(MONTH))
#####
#SDSS
suppmap_SDSS<-data.frame(LATITUDE = c(49.656, 50.432, 50.432, 49.834), 
                         LONGITUDE = c(-62.884, -62.884, -65.011, -65.011))
##

dynaship<-readOGR("./shapefiles", layer = "Dynamic_Shipping_Section", verbose = FALSE)
dynaship.tr<-spTransform(dynaship, CRS.new)
dynaship.sp<-spTransform(dynaship.tr,CRS.latlon)

to_string <- as_labeller(c(`4` = "April", `5` = "May", `6` = "June", `7` = "July", `8` = "August", `9` = "September", `10` = "October", `11` = "November"))

sup_map<-ggplot()+
  annotation_map(map_data("world"), fill = "antiquewhite", color = "grey50", size = 0.1)+
  annotation_map(map_data("state"), fill = "antiquewhite", color = "grey50", size = 0.1)+
  geom_path(fortify(iso200), mapping = aes(long,lat,group = group), color = "steelblue", alpha = 0.7, size = 0.25)+
  geom_path(data = fortify(ship.sp), aes(x = long, y = lat, group = group), color = "peachpuff4", size = 0.1)+
  geom_polygon(data = tss_polygons, aes(x = lon, y = lat), fill = "peachpuff4", color = "peachpuff4", size = 0.1, alpha = 0.5)+
  coord_sf(xlim = c(-70,-59), ylim = c(45.0,51.0), crs = 4269)+    
  theme_bw()+
  #MICS
  geom_polygon(suppmap_SDSS%>%mutate(MONTH = 6), mapping = aes(x = LONGITUDE, y = LATITUDE, fill = "SDSS"),  alpha = 0.5)+
  geom_polygon(suppmap_SDSS%>%mutate(MONTH = 7), mapping = aes(x = LONGITUDE, y = LATITUDE, fill = "SDSS"),  alpha = 0.5)+
  geom_polygon(suppmap_SDSS%>%mutate(MONTH = 8), mapping = aes(x = LONGITUDE, y = LATITUDE, fill = "SDSS"),  alpha = 0.5)+
  geom_polygon(suppmap_SDSS%>%mutate(MONTH = 9), mapping = aes(x = LONGITUDE, y = LATITUDE, fill = "SDSS"),  alpha = 0.5)+
  #Provinces
  geom_polygon(GSL_provinces, mapping = aes(long,lat,group = group), fill = "antiquewhite", color = "grey50", size = 0.25)+
  #Shipping lane 2018
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 4), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 5), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 6), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 7), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 8), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 9), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 10), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%filter(id != 4)%>%mutate(YEAR = 2018, MONTH = 11), aes(x = long, y = lat, group = group, fill = "SLAS"), size = 0.25, alpha = 0.5)+
  #Shipping lane 2019
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 5), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 6), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 7), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 8), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 9), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 10), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  geom_polygon(data = fortify(dynaship.sp)%>%mutate(YEAR = 2019, MONTH = 11), aes(x = long, y = lat, group = group, fill = "SLAS"),  size = 0.25, alpha = 0.5)+
  #TWIN OTTER
  geom_path(suppmap_MRAS, mapping = aes(x = LONGITUDE, y = LATITUDE, group = date, color = "MRAS"), size = 0.25)+
  #NEAQ/CWI
  geom_path(suppmap_MWSS, mapping = aes(x = LONGITUDE, y = LATITUDE, group = platdate, color = "MWSS"), size = 0.25)+
  facet_wrap(~MONTH, ncol = 3, labeller = to_string)+
  xlab("Longitude")+
  theme(legend.position = c(0.85,0.15),
        text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text=element_text(size=11),
        legend.key.size = unit(0.7, "line"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7))+
  coord_sf(xlim = c(-70,-57), ylim = c(45.5,51.5), crs = 4269)+
  scale_fill_manual(values = sup_map_fill)+
  scale_color_manual(values = sup_map_fill)

figS1A<-ggpubr::ggarrange(sup_map,labels = "A")  
ggsave(filename = 'FigS1A.svg',figS1A,device = 'svg', './figures', width = 169, height = 160, units = "mm", dpi = 320)