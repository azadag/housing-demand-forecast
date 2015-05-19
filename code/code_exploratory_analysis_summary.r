library(plyr)
library(dplyr)
library(ggplot2)

ind_data_out
ind_data_czone

## industry demand change...

ind_data_czone %>% ungroup() %>% summarise_each( funs(sum),fcast_2014,fcast_2024,fcast_2034)

ZpctCH <- ind_data_czone %>% group_by(Ind_Code) %>% summarise_each( funs(sum),fcast_2014,fcast_2024,fcast_2034) %>%
     mutate(pctch = round((fcast_2024 - fcast_2014)/ fcast_2024, 2), pctch34 = round((fcast_2034 - fcast_2024)/ fcast_2034, 2)) %>%
     arrange(desc(pctch))
write.csv(ZpctCH, "./Data/_output/Industry_Projections/Industry Projections/EMPLOYMENT_forecast_changebyIndustry.csv")

# head(Zind_data_czone)
Zind_data_czone <- ind_data_czone %>% group_by(czone) %>% summarise_each( funs(sum),fcast_2014,fcast_2024,fcast_2034)
Zind_data_czone$czone <- as.character(Zind_data_czone$czone)
Zind_data_czone <- inner_join( Zind_data_czone, code_czone , by=c("czone"="czone"))
Zind_data_czone$name <- strtrim(Zind_data_czone$czonename, 15 )
Zind_data_czone$czonename <- NULL
write.csv( Zind_data_czone, "./Data/_output/CA/Employment/EMPLOYMENT_forecast_byArea.csv")

Z2ind_data_czone <- ind_data_czone #%>% group_by(czone) %>% summarise_each( funs(sum),fcast_2014,fcast_2024,fcast_2034)
Z2ind_data_czone$czone <- as.character(Z2ind_data_czone$czone)
Z2ind_data_czone <- left_join( Z2ind_data_czone, code_czone , by=c("czone"="czone"))
Z2ind_data_czone$name <- strtrim(Z2ind_data_czone$czonename, 15 )
Z2ind_data_czone$czonename <- NULL
Z2ind_data_czone$cty_pop <- NULL
Z2ind_data_czone$cty_labf <- NULL
Z2ind_data_czone$join <- NULL
write.csv( Z2ind_data_czone, "./Data/_output/CA/Employment/EMPLOYMENT_forecast_byIndustryArea.csv")

p <- filter(Z2ind_data_czone)  %>% ggplot(  aes(x = Ind_Code, y = EmpCh14_24, colour = Ind_Code, group = Ind_Code)) + geom_bar(stat = "identity") +
  facet_wrap(~ name, ncol = 2 , scales = "free")  + theme_bw() #+ scale_colour_brewer( Set3)

Z2ind_data_czoneL <- Z2ind_data_czone %>% reshape2::melt( id = c("czone","Ind_Code", "name" ), value = "Employment", 6:7)

p <- filter(Z2ind_data_czoneL)  %>% ggplot(  aes(x = Ind_Code , y = value, fill = variable, colour = variable, group = variable)) + geom_bar(position = "dodge", width = .5, stat = "identity") +
  facet_wrap(~ name, ncol = 2 , scales = "free")  + theme_bw() #+ scale_colour_brewer( Set3)
ggsave("./Data/_output/CA/Employment/EMPLOYMENT_ChangeIndustryByArea.pdf",  height = 14, width = 12)

p <- filter(Z2ind_data_czone)  %>% group_by(Ind_Code) %>% summarise_each( funs(sum),EmpCh14_24,EmpCh24_34 ) %>% reshape2::melt( id = c("Ind_Code" ), value = "Employment", 2:3) %>%
  ggplot(  aes(x = Ind_Code , y = value/1e04, fill = variable, colour = variable, group = variable)) + geom_bar(position = "dodge", width = .5, stat = "identity") + theme_bw() 
p
  # facet_wrap(~ name, ncol = 2 , scales = "free")  + theme_bw() #+ scale_colour_brewer( Set3)
ggsave("./Data/_output/CA/Employment/EMPLOYMENT_ChangeByIndustryCAL.pdf",  height = 10, width = 12)



#######################

Zdata_acs_11hA <- data_acs_hhinc1 %>% filter( UNITSSTR != 0, UNITSSTR != 1, UNITSSTR != 2 , OWNERSHP != 0 ) %>%
  filter(PERNUM == min(PERNUM)) %>% group_by(czone, HHTYPE) %>% #group_by(czone, HHTYPE, HH, hhincsum) %>% 
  summarize( num  = sum(HHWT) ) 

Zdata_acs_11hA$join <- with(Zdata_acs_11hA, paste0(czone, HHTYPE) )
Zdata_acs_11hA$HHTYPE <- NULL
Zdata_acs_11hA$czone <- NULL

forecast_housing$join <- with(forecast_housing, paste0(czone, HHTYPE) )

Zdata_acs_11hA <- inner_join(Zdata_acs_11hA, forecast_housing, by = "join")
Zdata_acs_11hA <- rename(Zdata_acs_11hA, Housing14Total = num  )
Zdata_acs_11hA$Housing24Total <- Zdata_acs_11hA$Housing14Total + Zdata_acs_11hA$HH24new
Zdata_acs_11hA$Housing34Total <- Zdata_acs_11hA$Housing24Total + Zdata_acs_11hA$HH34new
Zdata_acs_11hA$czone <- as.character(Zdata_acs_11hA$czone)

# pctch <- function(a,b)  { z <- (Housing24Total - Housing14Total) / Housing14Total }
# ch <- function(a,b) {z <- Housing24Total - Housing14Total}
Zdata_acs_11hCITY <- Zdata_acs_11hA %>% group_by(czone) %>% summarise_each(funs(sum), Housing14Total, Housing24Total, Housing34Total) %>% 
  mutate(HousingCh_1424 = Housing24Total - Housing14Total, HousingCh_2434 = Housing34Total - Housing24Total, 
         HousingChPct_1424 = round((Housing24Total - Housing14Total) / Housing24Total, 3), 
         HousingChPct_2434 = round((Housing34Total - Housing24Total)/Housing34Total, 3)  ) %>%
  inner_join( code_czone, by=c("czone"))

write.csv( Zdata_acs_11hCITY, "./Data/_output/Housing Projections/HOUSING_CityTotal.csv")

################

Zdata_acs_11hCITYTYPE <- Zdata_acs_11hA %>% group_by(czone, HHTYPE) %>% summarise_each(funs(sum), Housing14Total, Housing24Total, Housing34Total) %>% 
  mutate(HousingCh_1424 = Housing24Total - Housing14Total, HousingCh_2434 = Housing34Total - Housing24Total, 
         HousingChPct_1424 = round((Housing24Total - Housing14Total) / Housing24Total, 3), 
         HousingChPct_2434 = round((Housing34Total - Housing24Total)/Housing34Total, 3)  ) %>%
  inner_join( code_czone, by=c("czone"))

write.csv( Zdata_acs_11hCITYTYPE, "./Data/_output/Housing Projections/HOUSING_CityTypeTotal.csv")
################


Zdata_acs_11hTYPE <- Zdata_acs_11hA %>% group_by( HHTYPE) %>% summarise_each(funs(sum), Housing14Total, Housing24Total, Housing34Total) %>% 
  mutate(HousingCh_1424 = Housing24Total - Housing14Total, HousingCh_2434 = Housing34Total - Housing24Total, 
         HousingChPct_1424 = round((Housing24Total - Housing14Total) / Housing24Total, 3), 
         HousingChPct_2434 = round((Housing34Total - Housing24Total)/Housing34Total, 3)  ) #%>%
  #inner_join( code_czone, by=c("czone"))

write.csv( Zdata_acs_11hTYPE, "./Data/_output/Housing Projections/HOUSING_TypeTotal.csv")



# Z2ind_data_czone$czone <- as.character(Z2ind_data_czone$czone)

