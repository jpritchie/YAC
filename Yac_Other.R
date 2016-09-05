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
  
  #REMOVE EGFR DATA OBTAINED AFTER START OF RRT
  #baseline
  YAC_MASTER$GFR_0<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`First Seen NEPHYAC eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`First Seen NEPHYAC eGFRDate`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`First Seen NEPHYAC eGFR`))
  
  YAC_MASTER$GFR_0<-as.numeric(YAC_MASTER$GFR_0)
  
  #year 1
  YAC_MASTER$GFR_1<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`12 month eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`12 month eGFR Date`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`12 month eGFR`)) 
  
  YAC_MASTER$GFR_1<-as.numeric(YAC_MASTER$GFR_1)
  
  #year 2
  YAC_MASTER$GFR_2<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`24 month eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`24 month eGFR Date`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`24 month eGFR`))
  
  YAC_MASTER$GFR_2<-as.numeric(YAC_MASTER$GFR_2)
  
  #year 3
  YAC_MASTER$GFR_3<-ifelse(is.na(YAC_MASTER$`Start of Dialysis`), YAC_MASTER$`36 month eGFR`, 
                           ifelse(YAC_MASTER$`Start of Dialysis`>0 & YAC_MASTER$`36 month eGFR Date`>YAC_MASTER$`Start of Dialysis`,  
                                  "", YAC_MASTER$`36 month eGFR`)) 
  
  YAC_MASTER$GFR_3<-as.numeric(YAC_MASTER$GFR_3)
  
#create a single file and reshape to allow calculation of individual regression slopes
  YAC_Slope <- YAC_MASTER[, c(1,77,78,79,80)]
  
  #simplify column names for regression / reshaping
  colnames(YAC_Slope) <- c("Number", "0", "1", "2", "3")
  
  YAC_Slope_tidy <- gather(YAC_Slope, Year, GFR, -Number)
  YAC_Slope_tidy<-arrange(YAC_Slope_tidy, Number, Year)
  YAC_Slope_tidy$Year<-as.numeric(as.character(YAC_Slope_tidy$Year))
  
  #remove na values
  YAC_Slope_tidy<-subset(YAC_Slope_tidy, YAC_Slope_tidy$GFR >1)
  
  #add a slope column to the data frame
  Slope<-YAC_Slope_tidy %>%
    group_by(Number) %>% 
    do(mod = lm(GFR ~ Year, data = .)) %>%
    mutate(Slope = summary(mod)$coeff[2]) %>%
    select(-mod)
  
  YAC_Slope_tidy <-left_join(YAC_Slope_tidy, Slope)
  #convert into easy to digest integer values
  YAC_Slope_tidy$Slope<-round(YAC_Slope_tidy$Slope, digits=0)
  
  #sort to remove duplication and then NA values
  YAC_Slope_tidy_nodup<-YAC_Slope_tidy[!duplicated(YAC_Slope_tidy$Number),]
  
  Slope_list<-is.na(YAC_Slope_tidy_nodup$Slope)
  YAC_Slope_tidy_nodup<-subset(YAC_Slope_tidy_nodup, Slope_list==F)
  
#PSYCHOLOGY DATA
  #subset to look just at those who have had evidence of psychology input
  Psych_list<-is.na(YAC_MASTER$PsychologyDocument)
  Psychology<-subset(YAC_MASTER, Psych_list==F)
  
  #remove patients without a first date for YAC
  Psych_list<-is.na(Psychology$`First Attended NEPHYAC Appt`)
  Psychology<-subset(Psychology, Psych_list==F)
  
  #calculate time from first YAC to first psychology appointment
  Psychology$Psych_time<-Psychology$`First Psychology Document Since Referral To Renal`-Psychology$`First Attended NEPHYAC Appt`
  
  #remove patients seen by pschology team prior to YAC
  Psychology<-subset(Psychology, Psychology$Psych_time>0)
  