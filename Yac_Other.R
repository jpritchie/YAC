#SET WORKING DIRECTORY
setwd("~/Dropbox/YAC/Data")

#DEFINE PACKAGES
require(readxl)
require(dplyr)
require(tidyr)

#IMPORT DATA
  YAC_MASTER<-read_excel("Yac_data.xlsx",sheet=1, skip=1)

#GFR TRAJECTORY DATA
#replace >90 with 90
  YAC_MASTER$`First Seen NEPHYAC eGFR`[YAC_MASTER$`First Seen NEPHYAC eGFR`==">90"]<-"90"
  YAC_MASTER$`First Seen NEPHYAC eGFR`<-as.numeric(as.character(YAC_MASTER$`First Seen NEPHYAC eGFR`))

  YAC_MASTER$`12 month eGFR`[YAC_MASTER$`12 month eGFR`==">90"]<-"90"
  YAC_MASTER$`12 month eGFR`<-as.numeric(as.character(YAC_MASTER$`12 month eGFR`))


  YAC_MASTER$`24 month eGFR`[YAC_MASTER$`24 month eGFR`==">90"]<-"90"
  YAC_MASTER$`24 month eGFR`<-as.numeric(as.character(YAC_MASTER$`24 month eGFR`))


  YAC_MASTER$`36 month eGFR`[YAC_MASTER$`36 month eGFR`==">90"]<-"90"
  YAC_MASTER$`36 month eGFR`<-as.numeric(as.character(YAC_MASTER$`36 month eGFR`))
  
#create a single file and reshape to allow calculation of individual regression slopes
  YAC_Slope <- YAC_MASTER[, c(1,30,36,50,64)]
  
  #simplify column names for regression / reshaping
  colnames(YAC_Slope) <- c("Number", "0", "1", "2", "3")
  
  YAC_Slope_tidy <- gather(YAC_Slope, Year, GFR, -Number)
  YAC_Slope_tidy<-arrange(YAC_Slope_tidy, Number, Year)
  YAC_Slope_tidy$Year<-as.numeric(as.character(YAC_Slope_tidy$Year))
  
  #remove na values
  YAC_Slope_tidy<-subset(YAC_Slope_tidy, YAC_Slope_tidy$GFR >1)
  
  #THIS IS NOT ADDING THE SLOPE COLUMN TO THE DF - NEEDS ADDRESSING
  YAC_Slope_tidy %>%
    group_by(Number) %>% 
    do(mod = lm(GFR ~ Year, data = .)) %>%
    mutate(Slope = summary(mod)$coeff[2]) %>%
    select(-mod)
  

  