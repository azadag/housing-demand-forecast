library(plyr)
library(dplyr)
library(reshape2)

# data_acs_1

# data_acs_1$STATEFIP <-  sprintf( "%02d", as.numeric(data_acs_1$STATEFIP ))
# # data_acs_1$STPUMA <- paste0( data_acs_1$STATEFIP, data_acs_1$PUMA1)
# data_acs_1$cty_fips <- paste0(  data_acs_1$STATEFIP, sprintf( "%03d", (data_acs_1$COUNTY/10) ))
# 
# data_acs_11 <- filter(data_acs_1, COUNTY != "0")
# data_acs_11 <- data_acs_11 %>%  inner_join(cwcty, by = "cty_fips")



## this splits the data apart like in tables A5
data_acs_11hwideSPLIT <- plyr::ddply(data_acs_11h, .(czone, hhincsum), 
                                     function(x) dcast(x,  HHTYPE ~ HH, value.var = "HHpct1" ) ) 

## arrange the data correctly and replace NA's with 0.
data_acs_11hwideSPLIT[is.na(data_acs_11hwideSPLIT)] <- 0

data_acs_11hwideSPLITa <- data_acs_11hwideSPLIT %>% group_by( czone, HHTYPE ) %>% arrange( hhincsum  ) %>% 
  ungroup() %>% group_by(czone) %>% arrange( czone )
# View(data_acs_11hwideSPLITa)


#### calculate forecast of housing Section 5 of Sturtevant & Chapman
data_acs_hhinc1$HHTYPE <- with(data_acs_hhinc1,
                               ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 1, "sf_owner",
                                       ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 2, "sf_renter",
                                               ifelse( (UNITSSTR >= 5 & OWNERSHP == 1), "mf_owner" ,
                                                       ifelse( (UNITSSTR >= 5 & OWNERSHP == 2), "mf_renter" , "other")))))

data_acs_11h <- data_acs_hhinc1 %>% filter(  UNITSSTR != 0,  UNITSSTR != 1,  UNITSSTR !=  2 ,  OWNERSHP != 0 ) %>%
  filter(PERNUM == min(PERNUM)) %>% group_by(czone, HHTYPE, HH, hhincsum) %>% 
  summarize( num  = sum(HHWT) )  %>% ungroup() %>% group_by( czone, hhincsum, HH) %>%
  mutate( HHpct1 = ( num / sum(num))) 

## can apply to the two forecast variables but just use HHs here... can use workers and back into HH again but... ##
# data_acs_hhinc_change, HHsbyInc2024, HHsbyInc2034  (aggregate by age group, ageg)
# data_acs_agehhch, NetWorkers24, Networker34 (aggregate by ageg)

data_forecastinc   <- data_acs_hhinc_change %>% select(IND1, czone.x, ageg, HH, hhincsum, HHsbyInc2024, HHsbyInc2034 ) %>% 
      group_by(czone.x, HH, hhincsum) %>% dplyr::summarize( HH24 = sum(HHsbyInc2024), HH34 = sum(HHsbyInc2034)) 

data_forecastinc[is.na(data_forecastinc)] <- 0
# data_acs_age1 %>% group_by(czone) %>% summarize(netHH14 = sum(AGE1))
# head(data_forecastinc)

data_acs_11h$join <- with(data_acs_11h, paste0(czone, HH, hhincsum))
data_forecastinc$join <- with(data_forecastinc, paste0(czone.x, HH, hhincsum))
data_forecastinc$czone.x <- NULL
data_forecastinc$HH <- NULL
data_forecastinc$hhincsum <- NULL

forecast_housing <- data_acs_11h %>% inner_join(data_forecastinc, by = "join") %>% 
  mutate(HH24new = HH24 * HHpct1, HH34new = HH34 * HHpct1 ) %>%
  group_by(czone, HHTYPE) %>% summarize(HH24new = sum(HH24new), HH34new = sum(HH34new))

forecast_housing$czone <- as.character( forecast_housing$czone )
forecast_housing <- inner_join( forecast_housing, code_czone, by="czone")

View(forecast_housing %>% arrange(czone))




HHinc14 <- data_acs_age %>% filter(PERNUM == max(PERNUM) ) %>% group_by(czone, HH, hhincsum) %>%
  dplyr::summarise( HH14 = sum( HHWT))

# HHinc14 <- data_acs_age %>% 
  # dplyr::summarise( HH14 = sum( PERWT))

View(data_forecastinc %>% arrange(czone.x, HH, hhincsum))
View(HHinc14 %>% arrange(czone, HH, hhincsum))
# data_forecastHHage <- data_acs_agehhch %>% select(IND1, czone.x, ageg, HH, NetWorkers24, NetWorkers34) %>% 
  # group_by(czone.x, HH, hhincsum) %>% dplyr::summarize( HH24 = sum(NetWorkers24), HH34 = sum(NetWorkers34))










# data_acs_11h$num <- NULL

# data_acs_11hmelt <- reshape2:: melt(data_acs_11h, id = c("hhincsum","HHTYPE", "HH", "czone"), measure=c("HHpct1") )
# data_acs_11hmelt %>% group_by(czone, hhincome) %>% spread()


# data_acs_11hwide <- reshape2:: dcast(data_acs_11hmelt, HHTYPE ~ HH)


# Aggregate using this newid and toss in the id so we don't lose it
# out <- dcast(tmp, id + newid ~ cat, value.var = "val")

# df <- ddply(df2, .(cat), function(x){ x$id2 = 1:nrow(x); x})
# data_acs_11hmelt$tmpid <- with(data_acs_11hmelt, paste0( czone, HH ))
# tmp <- ddply(data_acs_11hmelt, .(hhincsum, HH, czone), transform, newid = paste(czone, seq_along(HH), seq_along(hhincsum) ))

# m<-match(c(5,1,7
# 
# ,3),data2$V1)





test <-  (data_acs_11hwideSPLIT %>% filter(czone == 38300 & hhincsum == "b50_75" ))
head(test)


test <-  (data_acs_11h %>% filter(czone == 38300 & hhincsum == "Less50k" ))

testm <- reshape2:: melt(test, id = c("HHTYPE", "HH"), measure=c("HHpct1") )
widem <- reshape2:: dcast(testm, HHTYPE ~ HH)
widem

sum(widem[,2])
sum(widem[,3])
sum(widem[,4])


reshape2::dcast( testm, HH ~ hhincsum , value.var= "num" )
                 

value.var = HHpct1)



      mutate(HHtype = as.factor(ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 1, "sf_owner",
                                ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 2, "sf_renter",
                                ifelse( (UNITSSTR >= 5 & OWNERSHP == 1), "mf_owner" ,
                                ifelse( (UNITSSTR >= 4 & OWNERSHP == 2), "mf_renter" , "other")))))) %>%
      filter(PERNUM == min(PERNUM)) %>% group_by(czone, HHtype, HH, hhincsum) %>% 
      summarize( num  = sum(HHWT) )  %>% group_by( czone, HHtype, hhincsum) %>%
  mutate( HHpct1 = ( num / sum(num))) 

data_acs_11ha <- data_acs_11h
data_acs_11ha$num <- NULL

View(data_acs_11h %>% filter(czone == 38300 & hhincsum == "Less50k" )  )

tabular( HHtype ~ hhincsum, data=(data_acs_11h %>% filter(czone == 38300) ) )


data_acs_11h2 <- data_acs_11h %>% group_by( czone, HHtype, hhincsum) %>%
      mutate( HHpct1 = ( num / sum(num))) 

View(data_acs_11h)


data_acs_11$housetype <- 
  mutate(HHtype = as.factor(ifelse(maxP == 1 & hhchild ==0, 1,
                         ifelse( (maxP == 1 & hhchild == 1 ), 2,
                                 ifelse( (maxP == 2 & hhchild == 0 ), 3 ,
OWNERSHP 

data_acs_1housing <- data_acs_11 %>% group_by(czone, HHWT) %>% 
  summarize( INCOME1 = weighted.mean(HHINCOME1, HHWT) )  %>% 
  
  
 
