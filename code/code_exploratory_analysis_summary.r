library(plyr)
library(dplyr)
library(ggplot2)

ind_data_out
ind_data_czone

########################   SUMMARIZE / OUTPUT of TWO Different Household Forecasts  ##########
######### OUTPUT total HH income present and forecasts ###########
### Graphs and csv data pushed to Household Projections Folder 

### TO DO rename NewHousingCZinc to something about HouseHolds Instead
HH14 <- data_acs_age2 %>% group_by(czone) %>% summarize(netHH14 = sum(AGE1, na.rm = TRUE))
HH14 <- rename(HH14,  czone.x = czone)

NewHousingCZinc <- data_acs_hhinc_change %>% group_by( czone.x) %>%
  summarize( netHH24 = sum(HHsbyInc2024,na.rm=TRUE), 
             netHH34 = sum(HHsbyInc2034, na.rm=TRUE)) 

NewHousingCZinc <- inner_join(NewHousingCZinc, HH14, by = c("czone.x" = "czone.x"))
NewHousingCZinc$TotalHH14 <- NewHousingCZinc$netHH14 
NewHousingCZinc$TotalHH24 <- NewHousingCZinc$netHH14 + NewHousingCZinc$netHH24 
NewHousingCZinc$TotalHH34 <- NewHousingCZinc$TotalHH24 + NewHousingCZinc$netHH24 
NewHousingCZinc$netHH24 <- NULL
NewHousingCZinc$netHH34 <- NULL
NewHousingCZinc$netHH14 <- NULL
# NewHousingCZinc$czone.x <- as.character( NewHousingCZinc$czone.x ) 
NewHousingCZinc <-  inner_join( code_czone, NewHousingCZinc, by=c("czone"="czone.x"))
write.csv( NewHousingCZinc, "./Data/_output/Household Projections/HH_forecast_from_income.csv")

# NewHousingCZincL <-  inner_join( code_czone, NewHousingCZinc, by=c("czone"="czone.x"))
NewHousingCZincL <-  tidyr::gather(NewHousingCZinc, czone, "HH", 3:5)
names(NewHousingCZincL) <- c("czone","czonename","variable","HH")
plyr::mapvalues(NewHousingCZincL$variable, from = c("TotalHH14", "TotalHH24", "TotalHH34"), 
                to = c("2014", "2024", "2034"))
NewHousingCZincL$forecast <- "Income Forecast"

############# output age / industry forecast
NewHousingCZ1 <- data_acs_agehhch %>% group_by( czone.x) %>%
  summarize(netHH24 = round(sum(NetWorkers24,na.rm=TRUE),0), 
            netHH34 = round(sum(NetWorkers34, na.rm=TRUE),0)) 

NewHousingCZ1 <- inner_join(NewHousingCZ1, HH14, by = c("czone.x" = "czone.x"))
NewHousingCZ1$TotalHH14 <- NewHousingCZ1$netHH14 
NewHousingCZ1$TotalHH24 <- NewHousingCZ1$netHH14 + NewHousingCZ1$netHH24 
NewHousingCZ1$TotalHH34 <- NewHousingCZ1$TotalHH24 + NewHousingCZ1$netHH24 
NewHousingCZ1$netHH24 <- NULL
NewHousingCZ1$netHH34 <- NULL
NewHousingCZ1$netHH14 <- NULL
# NewHousingCZ1$czone.x <- as.character( NewHousingCZ1$czone.x ) 
NewHousingCZ1 <-  inner_join( code_czone, NewHousingCZ1, by=c("czone"="czone.x"))

write.csv( NewHousingCZ1, "./Data/_output/Household Projections/HH_forecast_from_age&industry.csv")

########### Re Do  for industry income forecast ####

NewHousingCZ1L <- tidyr::gather(NewHousingCZ1, czone, "HH", 3:5)
names(NewHousingCZ1L) <- c("czone","czonename","variable","HH")
# plyr::mapvalues(NewHousingCZ1L$variable, from = c("TotalHH14", "TotalHH24", "TotalHH34"), to = c("2014", "2024", "2034"))

# NewHousingCZ1L$variable[NewHousingCZ1L$variable == "TotalHH14" ] <- "2014"
# NewHousingCZ1L$variable[NewHousingCZ1L$variable == "TotalHH24" ] <- "2024"
# NewHousingCZ1L$variable[NewHousingCZ1L$variable == "TotalHH34" ] <- "2034"
NewHousingCZ1L$forecast <- "Age Industry Forecast"

CaForecast <- rbind(NewHousingCZincL, NewHousingCZ1L)
CaForecast$forecast <- as.factor(CaForecast$forecast)
CaForecast$variable <- plyr::mapvalues(CaForecast$variable, from = c("TotalHH14", "TotalHH24", "TotalHH34"), to = c("2014", "2024", "2034"))

p <- ggplot( CaForecast, aes(x = variable, y = log(HH),  group = czonename, color = czonename), log = y) + geom_line() + 
  geom_point() +  facet_wrap(~ forecast, ncol = 2 , scales = "free") 
p
ggsave(plot = p, filename= "./Data/_output/Household Projections/HH_forecast_compare_type.pdf", height = 8, width = 8 )

p <- ggplot( CaForecast, aes(x = variable, y = HH,  group = forecast, color = forecast)) + geom_line() + 
  geom_point() +  facet_wrap(~ czonename, ncol = 2 , scales = "free") 
p
ggsave(plot = p, filename= "./Data/_output/Household Projections/HH_forecast_compare_area.pdf", height = 12, width = 9 )


write.csv( NewHousingCZ1L, "./Data/_output/Household Projections/HH_forecast_from_income&industry.csv")

######## Industry area  forecasts 
HHind14 <- data_acs_age2 %>% group_by( czone, IND1) %>% summarize(netHH14 = sum(AGE1, na.rm = TRUE))

## need to paste for the industry join...
data_acs_agehhINCchS$join <- paste0(data_acs_agehhINCchS$IND1, data_acs_agehhINCchS$czone.x)
HHind14$join <- paste0(HHind14$IND1, HHind14$czone)
IndIncForecast <- inner_join( data_acs_agehhINCchS, HHind14, by = "join")

IndIncForecast$TotalHH14 <- IndIncForecast$netHH14 
IndIncForecast$TotalHH24 <- IndIncForecast$netHH14 + IndIncForecast$netHH24 
IndIncForecast$TotalHH34 <- IndIncForecast$TotalHH24 + IndIncForecast$netHH24 
IndIncForecast$netHH24 <- NULL
IndIncForecast$netHH34 <- NULL
IndIncForecast$netHH14 <- NULL
IndIncForecast$czone <- NULL
IndIncForecast$IND1.y <- NULL
IndIncForecast$join <- NULL
names(IndIncForecast) <- c("IND", "czone", "TotalHH14", "TotalHH24", "TotalHH34")
# IndIncForecastL <- tidyr::gather(IndIncForecast, czone, "HH", 3:5)
IndIncForecastL <- IndIncForecast %>% reshape2::melt( id = c("czone","IND"), value = "HH", 3:5)
names(IndIncForecastL) <- c("czone","Ind","variable","HH")
IndIncForecastL$forecast <- "Ind Industry Forecast"

##  ## age forecast
data_acs_agehhchS$join <- paste0(data_acs_agehhchS$IND1, data_acs_agehhchS$czone.x)
IndAgeForecast <- inner_join(data_acs_agehhchS, HHind14, by = c("join" = "join"))

IndAgeForecast$TotalHH14 <- IndAgeForecast$netHH14 
IndAgeForecast$TotalHH24 <- IndAgeForecast$netHH14 + IndAgeForecast$netHH24 
IndAgeForecast$TotalHH34 <- IndAgeForecast$TotalHH24 + IndAgeForecast$netHH24 
IndAgeForecast$netHH24 <- NULL
IndAgeForecast$netHH34 <- NULL
IndAgeForecast$netHH14 <- NULL
IndAgeForecast$czone <- NULL
IndAgeForecast$IND1.y <- NULL
IndAgeForecast$join <- NULL
names(IndAgeForecast) <- c("IND", "czone", "TotalHH14", "TotalHH24", "TotalHH34")


IndAgeForecastL <- IndAgeForecast %>% reshape2::melt( id = c("czone","IND"), value = "HH", 3:5)
# IndAgeForecastL <- tidyr::gather(IndIncForecast, key = c("czone","IND"), value = "HH", 3:5)
names(IndAgeForecastL) <- c("czone","Ind","variable","HH")
IndAgeForecastL$forecast <- "Ind Age Forecast"

CzoneIndustry <- rbind(IndIncForecastL, IndAgeForecastL)
# names(IndAgeForecast) <- c("czone", "IND", "variable", "HH", "forecast")

code_czone$czone <- as.character(code_czone$czone)
CzoneIndustry$czone <- as.character(CzoneIndustry$czone)

CzoneIndustry <- inner_join( CzoneIndustry, code_czone, by="czone")
names(CzoneIndustry) <- c("czone", "IND", "variable", "HH", "forecast", "czonename")

CzoneIndustry$variable <- plyr::mapvalues(CzoneIndustry$variable, from = c("TotalHH14", "TotalHH24", "TotalHH34"), to = c("2014", "2024", "2034"))
CzoneIndustry$forecast <- as.factor(CzoneIndustry$forecast)

write.csv( CzoneIndustry, "./Data/_output/Household Projections/HH_forecast_from_area&industry.csv")




#############################################################################
############################ industry demand change... ######################

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

