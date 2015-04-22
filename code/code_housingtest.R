


data_acs_1

data_acs_1$STATEFIP <-  sprintf( "%02d", as.numeric(data_acs_1$STATEFIP ))
# data_acs_1$STPUMA <- paste0( data_acs_1$STATEFIP, data_acs_1$PUMA1)
data_acs_1$cty_fips <- paste0(  data_acs_1$STATEFIP, sprintf( "%03d", (data_acs_1$COUNTY/10) ))

data_acs_11 <- filter(data_acs_1, COUNTY != "0")
data_acs_11 <- data_acs_11 %>%  inner_join(cwcty, by = "cty_fips")


data_acs_hhinc1

data_acs_11h <- data_acs_hhinc1 %>% filter( ( UNITSSTR != "0" & UNITSSTR != "1" & UNITSSTR !=  "2" ) & OWNERSHP != "0" )%>%
      mutate(HHtype = as.factor(ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 1, "sf_owner",
                                ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 2, "sf_renter",
                                ifelse( (UNITSSTR > 4 & OWNERSHP == 1), "mf_owner" ,
                                ifelse( (UNITSSTR > 4 & OWNERSHP == 2), "mf_renter" , "other")))))) %>%
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
