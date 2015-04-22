* NOTE: You need to edit the `cd` command to specify the path to the directory
* where the data file is located. For example: "C:\ipums_directory".
* .

cd ".".

data list file = "usa_00027.dat" /
  YEAR       1-4
  DATANUM    5-6
  SERIAL     7-14
  NUMPREC    15-16
  HHWT       17-26 (2)
  HHTYPE     27-27
  STATEFIP   28-29
  PUMA       30-34
  GQ         35-35
  GQTYPE     36-36
  GQTYPED    37-39
  OWNERSHP   40-40
  OWNERSHPD  41-42
  MORTGAGE   43-43
  MORTGAG2   44-44
  CONDOFEE   45-48
  HHINCOME   49-55
  VALUEH     56-62
  UNITSSTR   63-64
  NFAMS      65-66
  NMOTHERS   67-67
  NFATHERS   68-68
  PERNUM     69-72
  PERWT      73-82 (2)
  FAMSIZE    83-84
  NCHILD     85-85
  AGE        86-88
  LABFORCE   89-89
  IND        90-93
  INDNAICS   94-101 (a)
  FTOTINC    102-108
  MIGPLAC1   109-111
  MIGPUMA1   112-116
  PWPUMA00   117-121
  TRANWORK   122-123
.

variable labels
  YEAR         "Census year"
  DATANUM      "Data set number"
  SERIAL       "Household serial number"
  NUMPREC      "Number of person records following"
  HHWT         "Household weight"
  HHTYPE       "Household Type"
  STATEFIP     "State (FIPS code)"
  PUMA         "Public Use Microdata Area"
  GQ           "Group quarters status"
  GQTYPE       "Group quarters type [general version]"
  GQTYPED      "Group quarters type [detailed version]"
  OWNERSHP     "Ownership of dwelling (tenure) [general version]"
  OWNERSHPD    "Ownership of dwelling (tenure) [detailed version]"
  MORTGAGE     "Mortgage status"
  MORTGAG2     "Second mortgage status"
  CONDOFEE     "Monthly condominium fee"
  HHINCOME     "Total household income"
  VALUEH       "House value"
  UNITSSTR     "Units in structure"
  NFAMS        "Number of families in household"
  NMOTHERS     "Number of mothers in household"
  NFATHERS     "Number of fathers in household"
  PERNUM       "Person number in sample unit"
  PERWT        "Person weight"
  FAMSIZE      "Number of own family members in household"
  NCHILD       "Number of own children in the household"
  AGE          "Age"
  LABFORCE     "Labor force status"
  IND          "Industry"
  INDNAICS     "Industry, NAICS classification"
  FTOTINC      "Total family income"
  MIGPLAC1     "State or country of residence 1 year ago"
  MIGPUMA1     "PUMA of residence 1 year ago"
  PWPUMA00     "Place of work: PUMA, 2000 onward"
  TRANWORK     "Means of transportation to work"
.

value labels
  /YEAR
    1850   "1850"
    1860   "1860"
    1870   "1870"
    1880   "1880"
    1900   "1900"
    1910   "1910"
    1920   "1920"
    1930   "1930"
    1940   "1940"
    1950   "1950"
    1960   "1960"
    1970   "1970"
    1980   "1980"
    1990   "1990"
    2000   "2000"
    2001   "2001"
    2002   "2002"
    2003   "2003"
    2004   "2004"
    2005   "2005"
    2006   "2006"
    2007   "2007"
    2008   "2008"
    2009   "2009"
    2010   "2010"
    2011   "2011"
    2012   "2012"
    2013   "2013"
  /NUMPREC
    00   "Vacant household"
    01   "1 person record"
    02   "2"
    03   "3"
    04   "4"
    05   "5"
    06   "6"
    07   "7"
    08   "8"
    09   "9"
    10   "10"
    11   "11"
    12   "12"
    13   "13"
    14   "14"
    15   "15"
    16   "16"
    17   "17"
    18   "18"
    19   "19"
    20   "20"
    21   "21"
    22   "22"
    23   "23"
    24   "24"
    25   "25"
    26   "26"
    27   "27"
    28   "28"
    29   "29"
    30   "30"
  /HHTYPE
    0   "N/A"
    1   "Married-couple family household"
    2   "Male householder, no wife present"
    3   "Female householder, no husband present"
    4   "Male householder, living alone"
    5   "Male householder, not living alone"
    6   "Female householder, living alone"
    7   "Female householder, not living alone"
    9   "HHTYPE could not be determined"
  /STATEFIP
    01   "Alabama"
    02   "Alaska"
    04   "Arizona"
    05   "Arkansas"
    06   "California"
    08   "Colorado"
    09   "Connecticut"
    10   "Delaware"
    11   "District of Columbia"
    12   "Florida"
    13   "Georgia"
    15   "Hawaii"
    16   "Idaho"
    17   "Illinois"
    18   "Indiana"
    19   "Iowa"
    20   "Kansas"
    21   "Kentucky"
    22   "Louisiana"
    23   "Maine"
    24   "Maryland"
    25   "Massachusetts"
    26   "Michigan"
    27   "Minnesota"
    28   "Mississippi"
    29   "Missouri"
    30   "Montana"
    31   "Nebraska"
    32   "Nevada"
    33   "New Hampshire"
    34   "New Jersey"
    35   "New Mexico"
    36   "New York"
    37   "North Carolina"
    38   "North Dakota"
    39   "Ohio"
    40   "Oklahoma"
    41   "Oregon"
    42   "Pennsylvania"
    44   "Rhode Island"
    45   "South Carolina"
    46   "South Dakota"
    47   "Tennessee"
    48   "Texas"
    49   "Utah"
    50   "Vermont"
    51   "Virginia"
    53   "Washington"
    54   "West Virginia"
    55   "Wisconsin"
    56   "Wyoming"
    61   "Maine-New Hampshire-Vermont"
    62   "Massachusetts-Rhode Island"
    63   "Minnesota-Iowa-Missouri-Kansas-Nebraska-S.Dakota-N.Dakota"
    64   "Maryland-Delaware"
    65   "Montana-Idaho-Wyoming"
    66   "Utah-Nevada"
    67   "Arizona-New Mexico"
    68   "Alaska-Hawaii"
    72   "Puerto Rico"
    97   "Military/Mil. Reservation"
    99   "State not identified"
  /GQ
    0   "Vacant unit"
    1   "Households under 1970 definition"
    2   "Additional households under 1990 definition"
    3   "Group quarters--Institutions"
    4   "Other group quarters"
    5   "Additional households under 2000 definition"
    6   "Fragment"
  /GQTYPE
    0   "NA (non-group quarters households)"
    1   "Institution (1990, 2000, ACS/PRCS)"
    2   "Correctional institutions"
    3   "Mental institutions"
    4   "Institutions for the elderly, handicapped, and poor"
    5   "Non-institutional GQ"
    6   "Military"
    7   "College dormitory"
    8   "Rooming house"
    9   "Other non-institutional GQ and unknown"
  /GQTYPED
    000   "NA (non-group quarters households)"
    010   "Family group, someone related to head"
    020   "Unrelated individuals, no one related to head"
    100   "Institution (1990, 2000, ACS/PRCS)"
    200   "Correctional institution"
    210   "Federal/state correctional"
    211   "Prison"
    212   "Penitentiary"
    213   "Military prison"
    220   "Local correctional"
    221   "Jail"
    230   "School juvenile delinquents"
    240   "Reformatory"
    250   "Camp or chaingang"
    260   "House of correction"
    300   "Mental institutions"
    400   "Institutions for the elderly, handicapped, and poor"
    410   "Homes for elderly"
    411   "Aged, dependent home"
    412   "Nursing/convalescent home"
    413   "Old soldiers home"
    420   "Other Instits (Not Aged)"
    421   "Other Institution nec"
    430   "Homes neglected/depend children"
    431   "Orphan school"
    432   "Orphans home, asylum"
    440   "Other instits for children"
    441   "Childrens home, asylum"
    450   "Homes physically handicapped"
    451   "Deaf, blind school"
    452   "Deaf, blind, epilepsy"
    460   "Mentally handicapped home"
    461   "School for feeblemind"
    470   "TB and chronic disease hospital"
    471   "Chronic hospitals"
    472   "Sanataria"
    480   "Poor houses and farms"
    481   "Poor house, almshouse"
    482   "Poor farm, workhouse"
    491   "Maternity homes for unmarried mothers"
    492   "Homes for widows, single, fallen women"
    493   "Detention homes"
    494   "Misc asylums"
    495   "Home, other dependent"
    496   "Instit combo or unknown"
    499   "499"
    500   "Non-institutional group quarters"
    501   "Family formerly in institutional group quarters"
    502   "Unrelated individual residing with family formerly in institutional group quarters"
    600   "Military"
    601   "U.S. army installation"
    602   "Navy, marine intallation"
    603   "Navy ships"
    604   "Air service"
    700   "College dormitory"
    701   "Military service academies"
    800   "Rooming house"
    801   "Hotel"
    802   "House, lodging apartments"
    803   "YMCA, YWCA"
    804   "Club"
    810   "810"
    900   "Other Non-Instit GQ"
    901   "Other Non-Instit GQ"
    910   "Schools"
    911   "Boarding schools"
    912   "Academy, institute"
    913   "Industrial training"
    914   "Indian school"
    920   "Hospitals"
    921   "Hospital, charity"
    922   "Infirmary"
    923   "Maternity hospital"
    924   "Childrens hospital"
    931   "Church, Abbey"
    932   "Convent"
    933   "Monastery"
    934   "Mission"
    935   "Seminary"
    936   "Religious commune"
    937   "Other religious"
    940   "Work sites"
    941   "Construction, except rr"
    942   "Lumber"
    943   "Mining"
    944   "Railroad"
    945   "Farms, ranches"
    946   "Ships, boats"
    947   "Other industrial"
    948   "Other worksites"
    950   "Nurses home, dorm"
    955   "Passenger ships"
    960   "Other group quarters"
    999   "Fragment (boarders and lodgers, 1900)"
  /OWNERSHP
    0   "N/A"
    1   "Owned or being bought (loan)"
    2   "Rented"
  /OWNERSHPD
    00   "N/A"
    10   "Owned or being bought"
    11   "Check mark (owns?)"
    12   "Owned free and clear"
    13   "Owned with mortgage or loan"
    20   "Rented"
    21   "No cash rent"
    22   "With cash rent"
  /MORTGAGE
    0   "N/A"
    1   "No, owned free and clear"
    2   "Check mark on manuscript (probably yes)"
    3   "Yes, mortgaged/ deed of trust or similar debt"
    4   "Yes, contract to purchase"
  /MORTGAG2
    0   "N/A "
    1   "No"
    2   "Yes"
    3   "Yes, 2nd mortgage"
    4   "Yes, home equity loan"
    5   "Yes, 2nd mortgage and home equity loan"
  /CONDOFEE
    0000   "0000"
  /HHINCOME
    9999999   "9999999"
  /UNITSSTR
    00   "N/A"
    01   "Mobile home or trailer"
    02   "Boat, tent, van, other"
    03   "1-family house, detached"
    04   "1-family house, attached"
    05   "2-family building"
    06   "3-4 family building"
    07   "5-9 family building"
    08   "10-19 family building"
    09   "20-49 family building"
    10   "50+ family building"
  /NFAMS
    00   "0 families (vacant unit)"
    01   "1 family or N/A"
    02   "2 families"
    03   "3"
    04   "4"
    05   "5"
    06   "6"
    07   "7"
    08   "8"
    09   "9"
    10   "10"
    11   "11"
    12   "12"
    13   "13"
    14   "14"
    15   "15"
    16   "16"
    17   "17"
    18   "18"
    19   "19"
    20   "20"
    21   "21"
    22   "22"
    23   "23"
    24   "24"
    25   "25"
    26   "26"
    27   "27"
    28   "28"
    29   "29"
    30   "30"
  /NMOTHERS
    0   "0 mothers or N/A"
    1   "1"
    2   "2"
    3   "3"
    4   "4"
    5   "5"
    6   "6"
    7   "7"
    8   "8"
  /NFATHERS
    0   "0 fathers or N/A"
    1   "1"
    2   "2"
    3   "3"
    4   "4"
    5   "5"
    6   "6"
  /FAMSIZE
    01   "1 family member present"
    02   "2 family members present"
    03   "3"
    04   "4"
    05   "5"
    06   "6"
    07   "7"
    08   "8"
    09   "9"
    10   "10"
    11   "11"
    12   "12"
    13   "13"
    14   "14"
    15   "15"
    16   "16"
    17   "17"
    18   "18"
    19   "19"
    20   "20"
    21   "21"
    22   "22"
    23   "23"
    24   "24"
    25   "25"
    26   "26"
    27   "27"
    28   "28"
    29   "29"
  /NCHILD
    0   "0 children present"
    1   "1 child present"
    2   "2"
    3   "3"
    4   "4"
    5   "5"
    6   "6"
    7   "7"
    8   "8"
    9   "9+"
  /AGE
    000   "Less than 1 year old"
    001   "1"
    002   "2"
    003   "3"
    004   "4"
    005   "5"
    006   "6"
    007   "7"
    008   "8"
    009   "9"
    010   "10"
    011   "11"
    012   "12"
    013   "13"
    014   "14"
    015   "15"
    016   "16"
    017   "17"
    018   "18"
    019   "19"
    020   "20"
    021   "21"
    022   "22"
    023   "23"
    024   "24"
    025   "25"
    026   "26"
    027   "27"
    028   "28"
    029   "29"
    030   "30"
    031   "31"
    032   "32"
    033   "33"
    034   "34"
    035   "35"
    036   "36"
    037   "37"
    038   "38"
    039   "39"
    040   "40"
    041   "41"
    042   "42"
    043   "43"
    044   "44"
    045   "45"
    046   "46"
    047   "47"
    048   "48"
    049   "49"
    050   "50"
    051   "51"
    052   "52"
    053   "53"
    054   "54"
    055   "55"
    056   "56"
    057   "57"
    058   "58"
    059   "59"
    060   "60"
    061   "61"
    062   "62"
    063   "63"
    064   "64"
    065   "65"
    066   "66"
    067   "67"
    068   "68"
    069   "69"
    070   "70"
    071   "71"
    072   "72"
    073   "73"
    074   "74"
    075   "75"
    076   "76"
    077   "77"
    078   "78"
    079   "79"
    080   "80"
    081   "81"
    082   "82"
    083   "83"
    084   "84"
    085   "85"
    086   "86"
    087   "87"
    088   "88"
    089   "89"
    090   "90 (90+ in 1980 and 1990)"
    091   "91"
    092   "92"
    093   "93"
    094   "94"
    095   "95"
    096   "96"
    097   "97"
    098   "98"
    099   "99"
    100   "100 (100+ in 1960-1970)"
    101   "101"
    102   "102"
    103   "103"
    104   "104"
    105   "105"
    106   "106"
    107   "107"
    108   "108"
    109   "109"
    110   "110"
    111   "111"
    112   "112 (112+ in the 1980 internal data)"
    113   "113"
    114   "114"
    115   "115 (115+ in the 1990 internal data)"
    116   "116"
    117   "117"
    118   "118"
    119   "119"
    120   "120"
    121   "121"
    122   "122"
    123   "123"
    124   "124"
    125   "125"
    126   "126"
    129   "129"
    130   "130"
    135   "135"
  /LABFORCE
    0   "N/A"
    1   "No, not in the labor force"
    2   "Yes, in the labor force"
  /INDNAICS
    "00000000"   "00000000"
  /MIGPLAC1
    000   "N/A"
    001   "Alabama"
    002   "Alaska"
    004   "Arizona"
    005   "Arkansas"
    006   "California"
    008   "Colorado"
    009   "Connecticut"
    010   "Delaware"
    011   "District of Columbia"
    012   "Florida"
    013   "Georgia"
    015   "Hawaii"
    016   "Idaho"
    017   "Illinois"
    018   "Indiana"
    019   "Iowa"
    020   "Kansas"
    021   "Kentucky"
    022   "Louisiana"
    023   "Maine"
    024   "Maryland"
    025   "Massachusetts"
    026   "Michigan"
    027   "Minnesota"
    028   "Mississippi"
    029   "Missouri"
    030   "Montana"
    031   "Nebraska"
    032   "Nevada"
    033   "New Hampshire"
    034   "New Jersey"
    035   "New Mexico"
    036   "New York"
    037   "North Carolina"
    038   "North Dakota"
    039   "Ohio"
    040   "Oklahoma"
    041   "Oregon"
    042   "Pennsylvania"
    044   "Rhode Island"
    045   "South Carolina"
    046   "South Dakota"
    047   "Tennessee"
    048   "Texas"
    049   "Utah"
    050   "Vermont"
    051   "Virginia"
    053   "Washington"
    054   "West Virginia"
    055   "Wisconsin"
    056   "Wyoming"
    099   "United States, ns"
    100   "Samoa, 1950"
    105   "Guam"
    110   "Puerto Rico"
    115   "Virgin Islands"
    120   "Other US Possessions"
    150   "Canada"
    151   "English Canada"
    152   "French Canada"
    160   "Atlantic Islands"
    200   "Mexico"
    211   "Belize/British Honduras"
    212   "Costa Rica"
    213   "El Salvador"
    214   "Guatemala"
    215   "Honduras"
    216   "Nicaragua"
    217   "Panama"
    218   "Canal Zone"
    219   "Central America, nec"
    250   "Cuba"
    261   "Dominican Republic"
    262   "Haita"
    263   "Jamaica"
    264   "British West Indies"
    267   "Other West Indies"
    290   "Other Caribbean and North America"
    305   "Argentina"
    310   "Bolivia"
    315   "Brazil"
    320   "Chile"
    325   "Colombia"
    330   "Ecuador"
    345   "Paraguay"
    350   "Peru"
    360   "Uruguay"
    365   "Venezuela"
    390   "South America, nec"
    400   "Denmark"
    401   "Finland"
    402   "Iceland"
    404   "Norway"
    405   "Sweden"
    410   "England"
    411   "Scotland"
    412   "Wales"
    413   "United Kingdom (excluding England: 2005ACS)"
    414   "Ireland"
    415   "Northern Ireland"
    419   "Other Northern Europe"
    420   "Belgium"
    421   "France"
    422   "Luxembourg"
    425   "Netherlands"
    426   "Switzerland"
    429   "Other Western Europe"
    430   "Albania"
    433   "Greece"
    434   "Dodecanese Islands"
    435   "Italy"
    436   "Portugal"
    437   "Azores"
    438   "Spain"
    450   "Austria"
    451   "Bulgaria"
    452   "Czechoslovakia"
    453   "Germany"
    454   "Hungary"
    455   "Poland"
    456   "Romania"
    457   "Yugoslavia"
    458   "Bosnia and Herzegovinia"
    459   "Other Eastern Europe"
    460   "Estonia"
    461   "Latvia"
    462   "Lithuania"
    463   "Other Northern or Eastern Europe"
    465   "USSR"
    498   "Ukraine"
    499   "Europe, ns"
    500   "China"
    501   "Japan"
    502   "Korea"
    503   "Taiwan"
    515   "Philippines"
    517   "Thailand"
    518   "Vietnam"
    519   "Other South East Asia"
    520   "Nepal"
    521   "India"
    522   "Iran"
    523   "Iraq"
    525   "Pakistan"
    534   "Israel/Palestine"
    535   "Jordan"
    537   "Lebanon"
    540   "Saudi Arabia"
    541   "Syria"
    542   "Turkey"
    543   "Afghanistan"
    551   "Other Western Asia"
    599   "Asia, nec"
    600   "Africa"
    610   "Northern Africa"
    611   "Egypt"
    619   "Nigeria"
    620   "Western Africa"
    621   "Eastern Africa"
    694   "South Africa (Union of)"
    699   "Africa, nec"
    701   "Australia"
    702   "New Zealand"
    710   "Pacific Islands (Australia and New Zealand Subregions, not specified, Oceania and at Sea: ACS)"
    900   "Abroad (unknown) or at sea"
    997   "Unknown value"
    999   "Missing"
  /MIGPUMA1
    00000   "00000"
  /PWPUMA00
    00000   "00000"
  /TRANWORK
    00   "N/A "
    10   "Auto, truck, or van"
    11   "Auto"
    12   "Driver"
    13   "Passenger"
    14   "Truck"
    15   "Van"
    20   "Motorcycle"
    30   "Bus or streetcar"
    31   "Bus or trolley bus"
    32   "Streetcar or trolley car"
    33   "Subway or elevated"
    34   "Railroad"
    35   "Taxicab"
    36   "Ferryboat"
    40   "Bicycle"
    50   "Walked only"
    60   "Other"
    70   "Worked at home"
.

execute.

