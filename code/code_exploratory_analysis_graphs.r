library(ggplot2)
#####

ggplot(forecast_housing, aes(x=HHTYPE, y=HH24new, fill= HHTYPE)) + geom_bar(stat = "identity")  +
  facet_grid( . ~ name ) + theme_bw()
ggsave("./Data/_output/Housing Projections/CITY_graph_housing_area24.pdf", width = 13, height = 7, dpi = 550)

ggplot(forecast_housing, aes(x=HHTYPE, y=HH34new, fill= HHTYPE)) + geom_bar(stat = "identity")  +
  facet_grid( . ~ name ) + theme_bw()

ggsave("./Data/_output/Housing Projections/CITY_graph_housing_area34.pdf", width = 13, height = 7, dpi = 550)

ggplot(forecast_housing, aes(x=HHTYPE, y=HH24new, fill= HHTYPE)) + geom_bar(stat = "identity")  +
  facet_grid( . ~ name ) + theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graph_housing

ggplot(forecast_housing, aes(x=HHTYPE, y=HH24new, fill= HHTYPE)) + geom_bar(stat = "identity")  +
  facet_grid( . ~ name ) + theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1))


forecast_housingL <- forecast_housing %>% reshape2::melt( id = c("czone","HHTYPE", "czonename", "name" ), value = "HHs", 3:4)
# names(IndIncForecastL) <- c("czone","Ind","variable","HH")
# IndIncForecastL$forecast <- "Ind Industry Forec


graph_housing <- ggplot(forecast_housingL, aes(x=HHTYPE, y=value, fill= variable)) + geom_bar(position="dodge", stat = "identity") 
graph_housing

ggsave("./Data/_output/Housing Projections/CA_housing_forecast_total_by_year.pdf")

graph_housing <- ggplot(forecast_housingL, aes(x=variable, y=value, fill= HHTYPE)) + geom_bar(position="dodge", stat = "identity") 
graph_housing
ggsave("./Data/_output/Housing Projections/CA_housing_forecast_total_by_year_split.pdf")

ggplot(forecast_housingL, aes(x=HHTYPE, y=value, fill= HHTYPE)) + geom_bar(position="dodge", stat = "identity",
                                                                           aes(fill=factor(variable))) + facet_grid( . ~ name) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("./Data/_output/Housing Projections/CA_housing_forecast_total_by_type_year.pdf")


graph_housing <- ggplot(forecast_housingL, aes(x=HHTYPE, y=value, fill= variable)) + geom_bar(position="dodge", stat = "identity", width = .85) 
graph_housing + theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( . ~ name) 
# ggsave("./Data/_output/CA/CA_housing_forecast_total_by_type_year.pdf")

# graph_housing <- ggplot(forecast_housingL, aes(x=HHTYPE, y=value, fill= variable)) + geom_bar(position="stack", stat = "identity") 
# graph_housing + theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( . ~ name) 


# facet_grid( czonename + HHTYPE ~ variable ) + theme_bw()
# facets = ppt~time

###############################




library(RColorBrewer)
# str(CzoneIndustry)
p <- filter(CzoneIndustry, forecast == "Ind Industry Forecast")  %>% ggplot(  aes(x = variable, y = HH, colour = IND, group = IND), log = x) + geom_line() + 
  geom_point()+ facet_wrap(~ czonename, ncol = 2 , scales = "free")  + theme_bw() #+ scale_colour_brewer( Set3)
p
ggsave(plot = p, filename= "./Data/_output/Industry_Projections/charts/HH_IndAgeforecast_for_areas.pdf" , width = 9, height = 12)

p <- filter(CzoneIndustry, forecast == "Ind Age Forecast")  %>% ggplot(  aes(x = variable, y = HH, colour = IND, group = IND), log = x) + geom_line() + 
  geom_point()+ facet_wrap(~ czonename, ncol = 2 , scales = "free")  + theme_bw() #+ scale_colour_brewer( Set3)
p
ggsave(plot = p, filename= "./Data/_output/Industry_Projections/charts/HH_IndIncomeforecast_for_areas.pdf" , dpi = 550, width = 9, height = 12)


p <- filter(CzoneIndustry, IND == "33") %>% ggplot(  aes(x = variable, y = HH,  group = forecast, color = forecast)) + geom_line() + 
  geom_point() +  facet_wrap(~ czonename, ncol = 2 , scales = "free") 
 p
ggsave(plot = p, filename= "./Data/_output/Industry_Projections/charts/HH_IndCompareFcast.pdf" , dpi = 550, width = 9, height = 12)

#  p <- plyr::ddply(CzoneIndustry, "IND", transform, 
#        p = ggplot(  aes(x = variable, y = HH,  group = forecast, color = forecast)) + geom_line() + 
#          geom_point() +  facet_wrap(~ czonename, ncol = 2 , scales = "free"))

p <- ggplot(CzoneIndustry,  aes(x = variable, y = HH,  group = forecast, color = forecast)) + geom_line() + 
  geom_point() +  facet_wrap(~ czonename, ncol = 2 , scales = "free") 
# p + theme(title = paste0("Industry ",pl[[i]][[1]][2,2]))

# pl = plyr::dlply(CzoneIndustry, "IND", "%+%", e1=last_plot()); 
pl = plyr::dlply(CzoneIndustry, "IND", "%+%", e1=last_plot()); 
for(i in 1:length(pl)){
  ggsave(  pl[[i]], filename= paste0("./Data/_output/Industry_Projections/charts/ind/IndCompareFcast",pl[[i]][[1]][2,2],".pdf"))
}

