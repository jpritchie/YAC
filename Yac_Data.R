#SET WORKING DIRECTORY
setwd("~/Dropbox/YAC/Data")

#DEFINE PACKAGES
require(readxl)
require(dplyr)
require(ggplot2)

#IMPORT DATA
YAC_MASTER<-read_excel("Yac_data.xlsx",sheet=1)