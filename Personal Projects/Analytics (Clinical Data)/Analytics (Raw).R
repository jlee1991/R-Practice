#Import/Install Libraries
install.packages("dplyr")
library(dplyr)

#Set working directory - using the online R Studio Cloud
setwd("/cloud/project/")

#Import files from csv (exported from original Word Exercise)
prescription <- read.csv("data/prescription.csv")
outpatient <- read.csv("data/outpatient.csv")
inpatient <- read.csv("data/inpatient.csv")
consultation <- read.csv("data/consultation.csv")

#These are small sets, so displayed individually below
prescription
outpatient 
inpatient
consultation

#create a new dataframe table
all_merged <- full_join(consultation, inpatient, by = c("PATID", "CONSID")) %>% 
  full_join(.,outpatient, by = c("PATID", "CONSID")) %>%
  full_join(.,prescription, by = c("PATID", "CONSID"))

#Just take in the dates (and reword because I find it a little confusing)
all_dates <- all_merged[c("PATID","CONSID","EVENTDATE.x","EVENTDATE.y","EVENTDATE.x.x", "EVENTDATE.y.y")]
colnames(all_dates) <- c("PATID","CONSID","C_EVENTDATE","I_EVENTDATE","O_EVENTDATE","P_EVENTDATE")
all_dates

#Reformat the dataframe date types 
all_dates$C_EVENTDATE <- as.Date(all_dates$C_EVENTDATE, "%m/%d/%Y")
all_dates$I_EVENTDATE <- as.Date(all_dates$I_EVENTDATE, "%m/%d/%Y")
all_dates$O_EVENTDATE <- as.Date(all_dates$O_EVENTDATE, "%m/%d/%Y")
all_dates$P_EVENTDATE <- as.Date(all_dates$P_EVENTDATE, "%m/%d/%Y")
all_dates

#Apply event conditions from the assignment:
# 1) For all PATID/CONSID pairs in the Consultation table, EVENTDATE from Consultation will be used as the date when EVENTDATE exists
# 2) Otherwise, the minimum EVENTDATE from all associated nested observations will be used 
# 3) If EVENTDATE is missing in Consultation and all nested tables, then drop the observation

#Applies conditions 1 and 2
new_consultation <- all_dates %>%
  transmute(
    PATID,
    CONSID,
    EVENTDATE = coalesce(C_EVENTDATE, pmin(I_EVENTDATE,O_EVENTDATE,P_EVENTDATE, na.rm = TRUE))
    )

#Applies condition 3
new_consultation <- na.omit(new_consultation)

#Export the file (if neccessary)
write.csv(new_consultation, file = "new_consultation.csv")
