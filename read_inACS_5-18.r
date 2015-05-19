# install.packages("devtools")
# devtools::install_github("hadley/haven")
library(data.table)
# library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

library("sas7bdat")

##### read in employment forecast data (Moody's), and codes' ####
file <- paste0(getwd())
Ind_Emp2 <- read.csv("./Data/_input/1_emp_data.csv") %>% gather(Code)
names(Ind_Emp2) <- c("Date", "Code", "Empvalue")

code_ind    <- read.csv("./Data/_input/2_code_indemp.csv", nrows=16)
code_county <- read.csv("./Data/_input/3_code_county.csv", stringsAsFactors = FALSE, colClasses = c("character", "character")) #, nrows=58)
code_czone <- read.csv("./Data/_input/9_czonename.csv", stringsAsFactors = FALSE, nrows=14)
# code_czone$czone <- as.character(code_czone$czone)

#### Read and clean employment data ####
split <- stringr::str_split(Ind_Emp2[,2], pattern="_")
split2 <- as.data.frame( t(as.data.frame(split)) )
row.names(split2) <- NULL
names(split2) <- c("Ind_code", "county_code_moodies")
Ind_Emp3 <- cbind(Ind_Emp2, split2)
split <- NULL

Ind_Emp3$Year <- as.numeric(stringr::str_sub(Ind_Emp3$Date, start = 5, end = 8))
Ind_Emp3$county_code_moodies <- as.character(Ind_Emp3$county_code_moodies)
Ind_Emp3$Date <-NULL
Ind_Emp3$Code <-NULL

Ind_Emp14_34w <- Ind_Emp3 %>% 
  filter(Year == 2014 | Year == 2024 | Year == 2034 ) %>% 
  spread(Year, Empvalue)

Ind_Emp14_34wl <- Ind_Emp14_34w %>% 
      left_join(code_ind, by = c( "Ind_code" = "Mnemonic")) %>% 
      left_join(code_county, by = "county_code_moodies")
tbl_df(Ind_Emp14_34wl)

########## check area codes for BLS merge ###########
#################### checks out ##################### 

tbl = list.files(path=paste0(file,"/Data/BLS/2013/"), pattern="*.csv")
list_of_data1 = lapply(paste0(file,"/Data/BLS/2013/",tbl), read.csv, stringsAsFactors = FALSE)

empdata <- data.table::rbindlist(list_of_data1)
FilterEmpData <- empdata[empdata[[1]] %in% code_county[[1]],]
SumEmpData <- FilterEmpData %>% 
  dplyr::group_by( area_fips, industry_code) %>% 
  dplyr::summarize(Emp2013 = sum(annual_avg_emplvl, na.rm = TRUE) )

SumEmpData$ind_code_2 <- SumEmpData$industry_code

SumEmpData$ind_code_2[SumEmpData$industry_code == "52" ] <- "1023"
SumEmpData$ind_code_2[SumEmpData$industry_code == "53" ] <- "1023"

SumEmpData$ind_code_2[SumEmpData$industry_code == "54" ] <- "1024"
SumEmpData$ind_code_2[SumEmpData$industry_code == "55" ] <- "1024"
SumEmpData$ind_code_2[SumEmpData$industry_code == "56" ] <- "1024"

SumEmpData$ind_code_2[SumEmpData$industry_code == "61" ] <- "1025"
SumEmpData$ind_code_2[SumEmpData$industry_code == "62" ] <- "1025"

SumEmpData$ind_code_2[SumEmpData$industry_code == "71" ] <- "1026"
SumEmpData$ind_code_2[SumEmpData$industry_code == "72" ] <- "1026"

SumEmpData$industry_code <- as.factor( SumEmpData$industry_code )
SumEmpData$ind_code_2 <- as.factor( SumEmpData$ind_code_2 )

SumEmpData <- SumEmpData %>% arrange(area_fips, industry_code)

SumEmpData2 <- ( SumEmpData %>% group_by ( area_fips, ind_code_2) 
          %>%  mutate(Emp2013avg = Emp2013 / sum(Emp2013) ) )

SumEmpData2$Emp2013avg[is.nan(SumEmpData2$Emp2013avg)] = 0

## split recast modified data base into two : Ind_Emp14_34wl ###### mod is th
## efile of codes that need to be split mod 2 is to be retained
## NEED TO HARMONIZE CODES HERE VS THE ACS....

Ind_Emp14_34wlmod <- Ind_Emp14_34wl %>% 
    filter(Ind_Code %in% c("52", "54", "61", "71"))

Ind_Emp14_34wlmod2 <- Ind_Emp14_34wl %>% 
    filter(Ind_Code %in% c("21","22","23","33","42",
          "44","48","51","81","91","92","93"))

Ind_Emp14_34wlmod$join <- paste0(Ind_Emp14_34wlmod$county_code_moodies, Ind_Emp14_34wlmod$Ind_Code)
SumEmpData2$join <- paste0(SumEmpData2$area_fips, SumEmpData2$industry_code)

require(data.table)
a <- data.table(Ind_Emp14_34wlmod, key= "join")
b <- data.table(SumEmpData2, key = "join")
c <- a[b, roll = TRUE ]

c$Ind_code <- NULL
c$Description <- NULL
c$county <- NULL

c$fcast_2014 <- round( c[[3]] * c$Emp2013avg * 1000, digits = 0)
c$fcast_2034 <- round( c[[5]] * c$Emp2013avg * 1000, digits = 0)
c$fcast_2024 <- round( c[[4]] * c$Emp2013avg * 1000, digits = 0)

Ind_Emp14_34wlmod2$fcast_2014 <- Ind_Emp14_34wlmod2$`2014` * 1000
Ind_Emp14_34wlmod2$fcast_2024 <- Ind_Emp14_34wlmod2$`2024` * 1000
Ind_Emp14_34wlmod2$fcast_2034 <- Ind_Emp14_34wlmod2$`2034` * 1000

Ind_Emp14_34wlmod2 <- rename(Ind_Emp14_34wlmod2, industry_code = Ind_Code )
Ind_Emp14_34wlmod2$industry_code <- as.character(Ind_Emp14_34wlmod2$industry_code)

ind_data_out <- bind_rows(c, Ind_Emp14_34wlmod2 )
summary(as.factor(ind_data_out$industry_code))

ind_data_out$join <- NULL
ind_data_out$area_fips <- NULL
ind_data_out$Ind_Code <- NULL
ind_data_out$ind_code_2 <- NULL
ind_data_out$Emp2013avg <- NULL
ind_data_out$Ind_code <- NULL
ind_data_out$Description <- NULL
ind_data_out$county <- NULL

## recode local, national, state gov employment into one type
ind_data_out <- rename(ind_data_out, Ind_Code = industry_code )
ind_data_out <- rename(ind_data_out, cty_fips = county_code_moodies)

ind_data_out$Ind_Code[ind_data_out$Ind_Code == 91] <- 92
ind_data_out$Ind_Code[ind_data_out$Ind_Code == 93] <- 92

cwcty <- read.csv(".\\Data\\_input\\6_cs_cty_czone.csv")
cwcty$cty_fips <-  sprintf("%05d",cwcty$cty_fips)   ## add leading zeros to cross walk fips

##### roll industries up to commuter zones #####
ind_data_czone <- ind_data_out %>%  inner_join(cwcty, by = "cty_fips") %>% group_by(czone, Ind_Code) %>% 
  summarise_each(funs(sum), 
                 fcast_2014,fcast_2024,fcast_2034,cty_pop,cty_labf)

ind_data_czone$EmpCh14_24 <-  ind_data_czone$fcast_2024 - ind_data_czone$fcast_2014 
ind_data_czone$EmpCh24_34 <- ind_data_czone$fcast_2034 - ind_data_czone$fcast_2024  

#### clean out intermediary data frames  #####
a <- NULL
b <- NULL
c <- NULL

empdata <- NULL
FilterEmpData <- NULL
Ind_Emp <- NULL
Ind_Emp2 <- NULL
Ind_Emp3 <- NULL
Ind_Emp14_34w <- NULL
Ind_Emp14_34 <- NULL
Ind_Emp14_34a <- NULL
Ind_Emp14_34wl <- NULL
Ind_Emp14_34wl2 <- NULL
Ind_Emp14_34wlmod <- NULL
Ind_Emp14_34wlmod2 <- NULL
SumEmpData <- NULL
SumEmpData2 <- NULL
Ind_Splits <- NULL
split1 <- NULL
split2 <- NULL
list_of_data1 <- NULL 

#### read in acs data... #####
data_acs <- SAScii::read.SAScii ( 
  paste0(file,"/Data/ACS_input/usa_00032.dat") , 
  paste0(file,"/Data/ACS_input/usa_00032.sas") , 
  zipped = F )

####                    HH Type Determination              #####
#### for each HH determine which household type it belongs to ###

## count up the number of working adults per household linked to a household SERIAL number,
## then join back into, eventually replace LABFORCE with empstat

data_acs_wkrs <-  data_acs %>% group_by(SERIAL) %>% filter(AGE > 18 , (LABFORCE == 2 | (LABFORCE == 1 & OCC1990 == 999))) %>%
  summarize ( workers  = n() ) 

data_acs_1 <- inner_join(data_acs, data_acs_wkrs, by ="SERIAL") 

#####  explanation of new variables #### 
# HH is household types, maxP is number of of adults in hh,
# hhchild is # of children in household...
##### create household types, rejoin into ACS table ####

hh_nums <-  data_acs %>% group_by(SERIAL) %>% filter(AGE > 18) %>%
  summarize(maxP = n(), hhchild = ( sum(NCHILD))) %>% ungroup() %>%
  mutate(HH = as.factor(ifelse(maxP == 1 & hhchild ==0, 1,
                         ifelse( (maxP == 1 & hhchild == 1 ), 2,
                         ifelse( (maxP == 2 & hhchild == 0 ), 3,
                         ifelse( (maxP == 1 & hhchild == 2 ), 4,
                         ifelse( (maxP == 2 & hhchild == 1 ), 5,
                         ifelse( (maxP == 3 & hhchild == 0 ), 6,
                         ifelse( (maxP == 1 & hhchild >= 3 ), 7,
                         ifelse( (maxP == 2 & hhchild >= 2 ), 8,
                         ifelse( (maxP == 3 & hhchild >= 1 ), 9,
                         ifelse( (maxP >= 4 & hhchild >= 1 ) , 10,
                         ifelse( (maxP >= 4 &  hhchild == 0) , 11, 12  
                                                       )))))))))))))

data_acs_1 <- inner_join(data_acs_1, hh_nums, by = "SERIAL") 

######### employment filters  ##########
#### check  employment data levels #####
# levels(as.factor(data_acs$INDNAICS))
# levels(as.factor(ind_data_czone$Ind_Code))

## take out unemployed, and military occupations
data_acs_emp <- data_acs_1 %>% filter( OCC1990 != "999")
data_acs_emp <- data_acs_emp %>% filter( INDNAICS != "928110P1"  &
                                  INDNAICS != "928110P2" & 
                                  INDNAICS != "928110P3" &
                                  INDNAICS != "928110P4" &
                                  INDNAICS != "928110P5" &
                                  INDNAICS != "928110P6" &
                                  INDNAICS != "928110P7")

## Recode Industry Variables to Match 
data_acs_emp$IND1 <- substr(data_acs_emp$INDNAICS,1,2)
data_acs_emp <- data_acs_emp %>% filter( IND1 != 11 ) ## remove farmers 
data_acs_emp$IND1[data_acs_emp$IND1 == 31 ]<- 33
data_acs_emp$IND1[data_acs_emp$IND1 == 32 ]<- 33
data_acs_emp$IND1[data_acs_emp$IND1 == "3M" ]<- 33
data_acs_emp$IND1[data_acs_emp$IND1 == 45 ]<- 44
data_acs_emp$IND1[data_acs_emp$IND1 == "4M" ]<- 44 
data_acs_emp$IND1[data_acs_emp$IND1 == 49 ]<- 48 
data_acs_emp$IND1[data_acs_emp$IND1 == 56 ]<- 54

##### create puma state name variable with leading zero prior to state fips concatentation ####
data_acs_emp$STATEFIP <-  sprintf( "%02d", as.numeric(data_acs_emp$STATEFIP ))
# data_acs_emp$STPUMA <- paste0( data_acs_emp$STATEFIP, data_acs_emp$PUMA1)
data_acs_emp$cty_fips <- paste0(  data_acs_emp$STATEFIP, sprintf( "%03d", (data_acs_emp$COUNTY/10) ))
data_acs_emp1 <- filter(data_acs_emp, COUNTY != "0")
data_acs_emp1 <- data_acs_emp1 %>%  inner_join(cwcty, by = "cty_fips")

#### create income groups determination #####
levels <- c(-Inf, 50000, 74999, 99999, 124999, 149999, 199999, 299999, 399999, 599999, Inf)
labels <- c("Less50k", "b50_75", "b75_100", "b100_125", "b125_149", "b150_200", "b200_300",
            "b300_400", "b400_600", "More600")
data_acs_emp1$HHINCOME1 <- with( data_acs_emp1, ifelse( HHINCOME == 9999999, 0, HHINCOME))
data_acs_hhinc <- data_acs_emp1 %>% group_by(SERIAL) %>% filter(PERNUM == max(PERNUM))# %>% 
#  select out only 1 person value per household, create the household 
data_acs_hhinc1 <- data_acs_hhinc %>% #group_by(IND1, czone, HH) %>%
  dplyr::mutate(hhincsum = cut(HHINCOME1, levels, labels = labels))

data_acs_hhincJ <- data_acs_hhinc1 %>% select(SERIAL, hhincsum)

## this is the average industry / area wage
df_median_wage_sector <- data_acs_hhinc1 %>% group_by(IND1, czone, HHWT) %>% ####### XXXXXXXXXXXX
  summarize( INCOME1 = weighted.mean(HHINCOME1, HHWT) )  %>% 
  summarize(
    avg_inc= round(  weighted.mean( INCOME1, HHWT),0) , 
    med_inc=round(median(rep(INCOME1,times= HHWT)),0) ,
    Q1 = Hmisc::wtd.quantile( INCOME1, weights = HHWT, probs = .25),
    Q2 = Hmisc::wtd.quantile( INCOME1, weights = HHWT, probs = .5),
    Q3 = Hmisc::wtd.quantile( INCOME1, weights = HHWT, probs = .75),
    Q4 = Hmisc::wtd.quantile( INCOME1, weights = HHWT, probs = .9)
  )

df_median_wage_sector$join <- paste0(df_median_wage_sector$czone, df_median_wage_sector$IND1)
df_median_wage_sector$czone <- NULL
df_median_wage_sector$IND1 <- NULL

# rejoin in
data_acs_emp1J <- left_join(data_acs_emp1, data_acs_hhincJ, by="SERIAL")

#### CREATE AGE DISTRBUTION // a3
levels <- c(-Inf, 30, 44, 64, Inf)
labels <- c("Less30", "b30_44", "b45_64", "More65")
 
data_acs_age <- data_acs_emp1J %>% 
  dplyr::group_by(IND1, czone) %>%
  # dplyr::tally  %>% 
  dplyr::mutate(ageg = cut(AGE, levels, labels = labels))


####
data_acs_age2 <- data_acs_age %>% dplyr::group_by(IND1, czone, ageg, HH) %>%
  dplyr::summarise( AGE1 = sum( HHWT, na.rm = TRUE)) %>% ungroup() %>% 
  dplyr::group_by(IND1, czone, ageg) %>% 
  dplyr::mutate( AGE1pct = AGE1 / sum(AGE1, na.rm = TRUE)) %>% ungroup() %>%
  dplyr::group_by(IND1, czone) %>% 
  dplyr::mutate( PctOfINDZONE = AGE1 / sum(AGE1, na.rm = TRUE)) %>% ungroup() %>%
  dplyr::group_by(IND1, czone, HH) %>%
  dplyr::mutate(AGE2 = AGE1 / sum(AGE1, na.rm = TRUE)) 
  
# AGE1pct :: the age group's percent of industry employment for the czone 
# AGE2    :: has HH's pct of industry  for czone...
# ageg    :: Age Groups


#### create distribution of hh types  #### net new forecasts from industry / ## 
##age distributions
data_acs_age1hh <- data_acs_age %>% dplyr::group_by(IND1, czone, ageg, HH) %>%
          dplyr::summarise( AGE1 = sum( HHWT, na.rm = TRUE), avghhworkers = mean(workers, na.rm = TRUE) )
          
data_acs_age3 <- as.data.frame(cbind(data_acs_age2, data_acs_age1hh$avghhworkers))
names(data_acs_age3)[9] <- "avghhworkers"

##### join industry forecasts to household #####
ind_data_czone$join <- paste0(ind_data_czone$czone, ind_data_czone$Ind_Code)
data_acs_age3$join  <- paste0(data_acs_age3$czone, data_acs_age3$IND1)

data_acs_age4 <- left_join(data_acs_age3, ind_data_czone, by="join") %>% arrange(czone.x,IND1)
data_acs_age4$czone.y  <- NULL
data_acs_age4$Ind_Code <- NULL

data_acs_agehhch <- data_acs_age4 %>% group_by(IND1, czone.x, ageg, HH) %>% 
  mutate( NetWorkers24 = sum( (AGE2 * AGE1pct * EmpCh14_24)/ avghhworkers , na.rm = TRUE), 
          NetWorkers34 = sum( (AGE2 * AGE1pct * EmpCh24_34)/ avghhworkers) , na.rm = TRUE) 
##age2 - is the households

data_acs_agehhchS <- data_acs_agehhch %>% group_by(IND1, czone.x) %>%
         summarize(netHH24 = sum(NetWorkers24, na.rm = TRUE), 
                   netHH34 = sum(NetWorkers34, na.rm = TRUE)) %>%
        arrange(desc(czone.x))

## hh by income group calcs, this redoes the analysis above using age / household income 
## groups... #####

data_acs_age1I <- data_acs_age %>% group_by(IND1, czone, ageg, HH, hhincsum) %>%
  summarise( AGEI1 = sum( PERWT))# %>% mutate(AGE1pct = AGE1 / sum( PERWT))

data_acs_age1It <- data_acs_age1I %>% dplyr::group_by(IND1, czone, ageg, hhincsum) %>% 
  mutate( AGE1Ipct = AGEI1 / sum(AGEI1, na.rm = TRUE)) %>% ungroup() %>%
  group_by(IND1, czone, HH) %>%
  mutate(AGE2 = AGEI1 / sum(AGEI1, na.rm = TRUE)) %>% ungroup() %>%
  group_by(IND1, czone, HH) %>%
  dplyr::mutate(AGE2 = AGEI1 / sum(AGEI1 , na.rm = TRUE)) %>% ungroup() %>%
  group_by(IND1, czone) %>%
  dplyr::mutate(PCTofIndZone = AGEI1 / sum(AGEI1 , na.rm = TRUE)) %>% ungroup()

#### create distribution of hh types  ####
data_acs_age1Ihh <- data_acs_age %>% #  group_by(SERIAL) %>% filter(PERNUM == min(PERNUM)) %>%
  dplyr::group_by(IND1, czone, ageg, HH, hhincsum) %>%
  dplyr::summarise( AGE1 = sum( PERWT, na.rm = TRUE ), 
                    avghhworkers = mean(workers, na.rm = TRUE) )# %>% arrange(czone, HH, ageg, IND1 )

data_acs_ageI3 <- as.data.frame(cbind(data_acs_age1It, data_acs_age1Ihh$avghhworkers))
names(data_acs_ageI3)[10] <- "avghhworkers"

#### upto here gives us methodology section 4. ####
## we can now create a count of number of households by household type 
## and household income associatged with new job growth

##### income population groups #####
# this summarises number of households in each income group by industry 
# and pct of households in each industry 
data_acs_hhinc_groups <- data_acs_hhinc1 %>%  dplyr::group_by(IND1, czone, hhincsum ) %>%
  dplyr::summarise( HHs = sum( PERWT, na.rm = TRUE),  avghhworkers = mean(workers)) %>% mutate(HHpct = HHs / sum( HHs, na.rm = TRUE))

##### forecasts of hh incomes, paralleling forecasts of hh ind / age types
## used data_acs_ageI3 instead of data_acs_hhinc_groups
data_acs_ageI3$join <- paste0(data_acs_ageI3$czone, data_acs_ageI3$IND1)
data_acs_hhinc_groups_a <- left_join(data_acs_ageI3, ind_data_czone, by="join") 

data_acs_hhinc_groups_a$czone.y <- NULL
data_acs_hhinc_groups_a$Ind_Code <- NULL

data_acs_hhinc_change <- data_acs_hhinc_groups_a %>% group_by(IND1, czone.x, hhincsum, HH) %>% 
  mutate( HHsbyInc2024 = round(  (PCTofIndZone * fcast_2014 * ((fcast_2024 - fcast_2014)/ fcast_2014 )),digits=0),   #/  avghhworkers 
          HHsbyInc2034 = round( (PCTofIndZone * fcast_2014 * ((fcast_2034 - fcast_2024)/ fcast_2024 )) ,digits=0)
  ) #%>% mutate( HHsChbyInc1424 = round( HHsbyInc2024 - HHs) , HHsChbyInc2434 = round( HHsbyInc2034 - HHsbyInc2024) )

data_acs_agehhINCchS <- data_acs_hhinc_change %>% group_by(IND1, czone.x) %>%
  summarize( netHH24 = sum(HHsbyInc2024, na.rm = TRUE), netHH34 = sum(HHsbyInc2034, na.rm = TRUE)) 

########################  DATA SUMMARY                  ##########
######### OUTPUT total HH income present and forecasts ###########
HH14 <- data_acs_age1 %>% group_by(czone) %>% summarize(netHH14 = sum(AGE1, na.rm = TRUE))
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
NewHousingCZinc$czone.x <- as.character( NewHousingCZinc$czone.x ) 
NewHousingCZinc <-  inner_join( code_czone, NewHousingCZinc, by=c("czone"="czone.x"))
write.csv( NewHousingCZinc, "./Data/_output/Industry_Projections/HH_forecast_from_income.csv")

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
NewHousingCZ1$czone.x <- as.character( NewHousingCZ1$czone.x ) 
NewHousingCZ1 <-  inner_join( code_czone, NewHousingCZ1, by=c("czone"="czone.x"))

write.csv( NewHousingCZ1, "./Data/_output/CA/HH_forecast_from_age&industry.csv")



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
ggsave(plot = p, filename= "./Data/_output/CA/HHforecast_compare.pdf", height = 8, width = 8 )

p <- ggplot( CaForecast, aes(x = variable, y = HH,  group = forecast, color = forecast)) + geom_line() + 
  geom_point() +  facet_wrap(~ czone, ncol = 2 , scales = "free") 
p
ggsave(plot = p, filename= "./Data/_output/CA/HHforecast_compare_area.pdf", height = 12, width = 9 )

write.csv( NewHousingCZ1L, "./Data/_output/CA/HH_forecast_from_income&industry.csv")

######## Industry area  forecasts 
HHind14 <- data_acs_age1 %>% group_by( czone, IND1) %>% summarize(netHH14 = sum(AGE1, na.rm = TRUE))

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

write.csv( CzoneIndustry, "./Data/_output/CA/HH_forecast_from_area&industry.csv")

### FORECAST HOUSING NEEDS
#### calculate forecast of housing Section 5 of Sturtevant & Chapman
data_acs_hhinc1$HHTYPE <- with(data_acs_hhinc1,
                              ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 1, "sf_owner",
                              ifelse( (UNITSSTR == 3 | UNITSSTR == 4) & OWNERSHP == 2, "sf_renter",
                              ifelse( (UNITSSTR >= 5 & OWNERSHP == 1), "mf_owner" ,
                              ifelse( (UNITSSTR >= 5 & OWNERSHP == 2), "mf_renter" , "other")))))

data_acs_11h <- data_acs_hhinc1 %>% filter( UNITSSTR != 0, UNITSSTR != 1, UNITSSTR != 2 , OWNERSHP != 0 ) %>%
  filter(PERNUM == min(PERNUM)) %>% group_by(czone, HHTYPE, HH, hhincsum) %>% 
  summarize( num  = sum(HHWT, na.rm = TRUE) ) %>% ungroup() %>% group_by( czone, hhincsum, HH) %>%
  mutate( HHpct1 = ( num / sum(num, na.rm = TRUE))) 

## can apply to the two forecast variables but just use HHs here... can use workers and back into HH again but... ##
# data_acs_hhinc_change, HHsbyInc2024, HHsbyInc2034  (aggregate by age group, ageg)
# data_acs_agehhch, NetWorkers24, Networker34 (aggregate by ageg)

data_forecastinc   <- data_acs_hhinc_change %>% select(IND1, czone.x, ageg, HH, hhincsum, HHsbyInc2024, HHsbyInc2034 ) %>% 
  group_by(czone.x, HH, hhincsum) %>% dplyr::summarize( HH24 = sum(HHsbyInc2024, na.rm = TRUE), 
                                                        HH34 = sum(HHsbyInc2034, na.rm = TRUE)) 

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
  group_by(czone, HHTYPE) %>% summarize(HH24new = sum(HH24new, na.rm = TRUE), 
                                        HH34new = sum(HH34new, na.rm = TRUE))

forecast_housing$czone <- as.character( forecast_housing$czone )
forecast_housing <- inner_join( forecast_housing, code_czone, by="czone")
forecast_housing$name <- strtrim(forecast_housing$czonename, 15 )

x_la1 <- forecast_housing %>% group_by(czone) %>% summarise(NewHousing24 = sum(HH24new, na.rm = TRUE), 
                                                            NewHousing34 = sum(HH34new, na.rm = TRUE))


write.csv( forecast_housing, "./Data/_output/CA/HOUSING_forecast.csv")


