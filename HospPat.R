#Load files
a <- read.csv("patient-discharge-data-by-admission-source.csv")
b<- read.csv("patient-discharge-data-by-admission-type.csv")
c<- read.csv("patient-discharge-data-by-age-groups.csv")
d<- read.csv("patient-discharge-data-by-disposition.csv")
e<- read.csv("patient-discharge-data-by-expected-payer.csv")
f<- read.csv("patient-discharge-data-by-gender.csv")
g<- read.csv("patient-discharge-data-by-principal-cause-of-injury.csv")
h<- read.csv("patient-discharge-data-by-principal-diagnosis.csv")
i<- read.csv("patient-discharge-data-by-principal-procedure.csv")
j<- read.csv("patient-discharge-data-by-race-groups.csv")
k<- read.csv("patient-discharge-data-by-type-of-care.csv")
l <- read.csv("patient-discharge-data-by-admission-source.csv")
#Load libraries
library(tidyverse)
library(reshape);library(reshape2)
#Transform Data
a1 <- melt(a,id=c("Oshpd_ID","Year","Facility_Name","Type_Of_Control","County_Name","Count"))
b1 <- melt(b,id=c("Oshpd_ID","Year","Facility_Name","Type_Of_Control","County_Name","Count"))
#Merge datasets
a1 <- data.frame(a1)
b1 <- data.frame(b1)
# Where I left off
rbind(a1,b1,by=c("Oshpd_ID","Year","Facility_Name","Type_Of_Control","County_Name","Count"))
#Transform Data (Attempt 1)
a <- a %>% 
  dcast(Oshpd_ID~Admission_Source_Site)
b <- b %>% 
  dcast(Oshpd_ID~Admission_Type)
c <- c %>% 
  dcast(OSHPD.Facility.Number~Age.Groups)
d <- d %>% 
  dcast(OSHPD.ID~Disposition)
e <- e %>% 
  dcast(OSHPD.Facility.Number~Expected.Payer)
f <- f %>% 
  dcast(OSHPD.Facility.Number~Gender)
g <- g %>% 
  dcast(OSHPD.ID~Principal.Injury.Group)
h <- h %>% 
  dcast(OSHPD.ID~Principal.Diagnosis.Group)
i <- i %>% 
  dcast(Oshpd_ID~Principal_Procedure_Group)
j <- j %>% 
  dcast(OSHPD.Facility.Number~Race.Group)
k <- k %>% 
  dcast(OSHPD.Facility.Number~Type.Of.Care)
l <- l %>% 
  select(-c(Admission_Source_Site,Count,Year)) 
l <- l %>% 
  distinct()
#Joined datasets
data <- left_join(a,b,by='Oshpd_ID') %>% 
  left_join(.,c,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
  left_join(.,d,by=c('Oshpd_ID'='OSHPD.ID')) %>% 
  left_join(.,e,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
  left_join(.,f,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
  left_join(.,g,by=c('Oshpd_ID'='OSHPD.ID')) %>% 
  left_join(.,h,by=c('Oshpd_ID'='OSHPD.ID')) %>% 
  left_join(.,i,by=c('Oshpd_ID'='Oshpd_ID')) %>% 
  left_join(.,j,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
  left_join(.,k,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
  left_join(.,l,by=c('Oshpd_ID'='Oshpd_ID')) %>% 
  na.omit()
data <- data.frame(data)
#Look at data
dim(data)
# 638  133 before omitting n/a's
# 578 133 after omitting n/a's
# 90.59% retained
hist(data$Acute.Inpatient.Hospital.Care,data$County_Name)
+Ambulatory.Surgery+Home+Newborn+Other.x+Other.Inpatient.Hospital.Care+
       Prison.Jail.x+Residential.Care.Facility.x+Skilled.Nursing.Intermediate.Care.x+Unknown.x                          
     , data=data)
########################################### Column Index of variables ############################################
# Admission Source: 2-11
# Admission Type: 12-15
# Age Groups: 16-26
# Disposition: 27-51
# Expected Payer: 52-62
# Gender: 63-66
# Principal Cause of Injury: 67-81
# Principal Diagnosis: 82-100
# Principal Procedure: 101-117
# Race Groups: 118-124
# Type of Care: 125-130

names(data[,2:11])





















