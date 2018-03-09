## Script containing all the required lists for plotting correct orders and aesthetics (colours etc.)
# Avoids bulking out general scripts when this info is required across multiple scripts for the same purposes
# MA 07/08/2017

#Need to update so have anon hosps and RC names
Hospital_Anon_geo_order <- c(
"South-WestD"  ,          "South-WestA"  ,          "South-WestC"      ,      "South-WestB"    ,        "SouthCentralA"  ,       
"South-EastA"  ,          "South-EastB"    ,        "South-EastC" ,           "South-EastD",            "East-1D"  ,             
"East-1A"    ,            "East-1C" ,               "East-1F"    ,            "East 2A"      ,          "East 2B"     ,          
"East 2G"      ,          "East 2D"      ,          "Central-East MidlandsA" ,"Central-East MidlandsB", "West-MidlandsC",        
"West-MidlandsA"  ,       "West-MidlandsB"    ,     "North-West 1A"   ,       "North-West 2A" ,         "North-East 1A"  ,       
"North-East 2A" ,         "North-CentralA"    ,     "North-CentralB"      ,   "ScotlandF"      ,        "ScotlandD"   ,          
"ScotlandC"  ,            "ScotlandH"    ,          "ScotlandA"  ,            "ScotlandG" ,             "ScotlandE"  ,           
"ScotlandB"  ,            "Northern IrelandD"  ,    "Northern IrelandC" ,     "Northern IrelandA"  ,    "Northern IrelandB"    , 
"IrelandC"  ,             "IrelandB"  ,             "IrelandA"  ,             "WalesA"  ,               "WalesC" ,               
"WalesB"       
)

RC_Anon_geo_order <- c(
"South West"   ,         "South West" ,           "South West"     ,       "South West"    ,        "South Central"       ,  "South East"      ,     
"South East" ,           "South East"     ,       "South East"     ,       "East 1"    ,            "East 1"      ,          "East 1"    ,           
 "East 1"   ,             "East 2"        ,        "East 2"        ,        "East 2"   ,             "East 2"      ,          "Central-East Midlands",
 "Central-East Midlands" ,"West Midlands"   ,      "West Midlands"    ,     "West Midlands"    ,     "North West 1"  ,        "North West 2"     ,    
 "North East 1"    ,      "North East 2"     ,     "North Central"   ,      "North Central"  ,       "Scotland"     ,         "Scotland"    ,         
 "Scotland"     ,         "Scotland"         ,     "Scotland"        ,      "Scotland"        ,      "Scotland"    ,          "Scotland"  ,           
 "Northern Ireland" ,     "Northern Ireland"  ,    "Northern Ireland"  ,    "Northern Ireland" ,     "Ireland"     ,          "Ireland"   ,           
"Ireland"    ,           "Wales"             ,    "Wales"       ,          "Wales"  
)

RC_anon_breaks <- c(Hospital_Anon_geo_order[2], Hospital_Anon_geo_order[5], Hospital_Anon_geo_order[7], 
                    Hospital_Anon_geo_order[11], Hospital_Anon_geo_order[15], Hospital_Anon_geo_order[18], 
                    Hospital_Anon_geo_order[21], Hospital_Anon_geo_order[23], Hospital_Anon_geo_order[24],
                    Hospital_Anon_geo_order[25], Hospital_Anon_geo_order[26], Hospital_Anon_geo_order[27], 
                    Hospital_Anon_geo_order[32], Hospital_Anon_geo_order[38], Hospital_Anon_geo_order[42], 
                    Hospital_Anon_geo_order[45])

##Anonymous Hospital-RC association named list
HospitalRCs_anon <- RC_Anon_geo_order
names(HospitalRCs_anon) <- Hospital_Anon_geo_order


#### First, lists required for aesthetics and ordering taken from JS "Geo Order of 2001 to 2010 Hosps" ####
#RC Order, with various levels.
RCOrder<-c("RC1","RC2","RC3","RC4","RC5","RC6","RC7","RC8","RC9","RC10","RC11","RC12","RC13","RC14","RC15","RC16")

#colours for RCs ordered
RCcolsHospGeoOrder<-c(
  rep("#df65b0",4),
  rep("#fe9929",1),
  rep("#683200",4),
  rep("#88419d",4),
  rep("#3690c0",4),
  rep("#045a8d",2),
  rep("#ae017e",3),
  rep("#e74e8a",1),
  rep("#fedc29",1),
  rep("#023858",1),
  rep("#848482",1),
  rep("#d7301f",2),
  rep("#7f0000",8),
  rep("#00441b",4),
  rep("#238b45",3),
  rep("#66c2a4",3))

MonthOrder<-c(
  "Jan-01","Feb-01","Mar-01","Apr-01","May-01","Jun-01","Jul-01","Aug-01","Sep-01","Oct-01","Nov-01","Dec-01",
  "Jan-02","Feb-02","Mar-02","Apr-02","May-02","Jun-02","Jul-02","Aug-02","Sep-02","Oct-02","Nov-02","Dec-02",
  "Jan-03","Feb-03","Mar-03","Apr-03","May-03","Jun-03","Jul-03","Aug-03","Sep-03","Oct-03","Nov-03","Dec-03",
  "Jan-04","Feb-04","Mar-04","Apr-04","May-04","Jun-04","Jul-04","Aug-04","Sep-04","Oct-04","Nov-04","Dec-04",
  "Jan-05","Feb-05","Mar-05","Apr-05","May-05","Jun-05","Jul-05","Aug-05","Sep-05","Oct-05","Nov-05","Dec-05",
  "Jan-06","Feb-06","Mar-06","Apr-06","May-06","Jun-06","Jul-06","Aug-06","Sep-06","Oct-06","Nov-06","Dec-06",
  "Jan-07","Feb-07","Mar-07","Apr-07","May-07","Jun-07","Jul-07","Aug-07","Sep-07","Oct-07","Nov-07","Dec-07",
  "Jan-08","Feb-08","Mar-08","Apr-08","May-08","Jun-08","Jul-08","Aug-08","Sep-08","Oct-08","Nov-08","Dec-08",
  "Jan-09","Feb-09","Mar-09","Apr-09","May-09","Jun-09","Jul-09","Aug-09","Sep-09","Oct-09","Nov-09","Dec-09",
  "Jan-10","Feb-10","Mar-10","Apr-10","May-10","Jun-10","Jul-10","Aug-10","Sep-10","Oct-10","Nov-10","Dec-10")
Months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

QuarterOrder<-unique(StrainQuarter2)
Quarters<-c("First","Second","Third","Fourth")

#### Second, lists created by MA to apply these orders to ggplot, and extras for nice ggplot layouts (themes) ####

#set-up formatting for plotting in loop 
#general ggplot theme - COULD MAKE THIS THEME ENCOMPASS MORE OF THE PLOTTING AESTHETICS WHICH ARE REPEATED (& name it sthg else more specific).
my_theme <- theme(panel.border = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black"),
                  legend.position = "right",
                  legend.background = element_rect(fill = "white"), #changes bg of legend itself
                  legend.key = element_rect(fill="white"))

#boxplot theme
boxplot_theme <- theme(panel.border = element_blank(),
                       panel.background = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.position = "right",
                       legend.background = element_rect(fill = "white"), #changes bg of legend itself
                       legend.key = element_rect(fill="white"))

#Creating named colour vector for use in ggplot - hosp names anon version
#RC colours repeated correct num times in correct order according to hosp order
ggplot_cols_anon <- ggplot_colours <- RCcolsHospGeoOrder 
#Naming the RC colours according to hosp, so ggplot calls right RC colour by hosp
names(ggplot_colours) <- HospGeoOrder2 
#combining into appropriately named colour vector
names(ggplot_cols_anon) <- Hospital_Anon_geo_order

#need RC list order to be same order as plotting order to make legend label correctly
RCHospsOrder <- c("RC2", "RC5", "RC1", "RC4", "RC8", "RC9", "RC6", "RC7", "RC12", "RC11", "RC3", "RC10", "RC15", "RC14", "RC13", "RC16")