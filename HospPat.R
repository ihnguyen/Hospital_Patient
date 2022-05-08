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
library(tidyverse);library(dplyr)
library(reshape);library(reshape2)
#Transform Data
a1 <- melt(a,id=c("Oshpd_ID","Year","Facility_Name","Type_Of_Control","County_Name","Count"))
b1 <- melt(b,id=c("Oshpd_ID","Year","Facility_Name","Type_Of_Control","County_Name","Count"))
c1 <- melt(c,id=c("OSHPD.Facility.Number","Year","Facility.Name","Type.Of.Control","County.Name","Count"))
d1 <- melt(d,id=c("OSHPD.ID","Year","Facility.Name","Type.of.Control","County.Name","Count"))
e1 <- melt(e,id=c("OSHPD.Facility.Number","Year","Facility.Name","Type.Of.Control","County.Name","Count"))
f1 <- melt(f,id=c("OSHPD.Facility.Number","Year","Facility.Name","Type.Of.Control","County.Name","Count"))
g1 <- melt(g,id=c("OSHPD.ID","Year","Facility.Name","Type.of.Control","County.Name","Count"))
h1 <- melt(h,id=c("OSHPD.ID","Year","Facility.Name","Type.of.Control","County.Name","Count"))
i1 <- melt(i,id=c("Oshpd_ID","Year","Facility_Name","Type_Of_Control","County_Name","Count"))
j1 <- melt(j,id=c("OSHPD.Facility.Number","Year","Facility.Name","Type.Of.Control","County.Name","Count"))
k1 <- melt(k,id=c("OSHPD.Facility.Number","Year","Facility.Name","Type.Of.Control","County.Name","Count"))
# Set as dataframe
a1 <- data.frame(a1)
b1 <- data.frame(b1)
c1 <- data.frame(c1)
d1 <- data.frame(d1)
e1 <- data.frame(e1)
f1 <- data.frame(f1)
g1 <- data.frame(g1)
h1 <- data.frame(h1)
i1 <- data.frame(i1)
j1 <- data.frame(j1)
k1 <- data.frame(k1)
# Change column names to the same names
a1 <- a1 %>%
  dplyr::rename(OSHPD.ID = Oshpd_ID,
  Facility.Name = Facility_Name,
  Type.Of.Control = Type_Of_Control,
  County.Name = County_Name)
b1 <- b1 %>%
  dplyr::rename(OSHPD.ID = Oshpd_ID,
                Facility.Name = Facility_Name,
                Type.Of.Control = Type_Of_Control,
                County.Name = County_Name)
c1 <- c1 %>%
  dplyr::rename(OSHPD.ID = OSHPD.Facility.Number)
d1 <- d1 %>%
  dplyr::rename(Type.Of.Control = Type.of.Control)
e1 <- e1 %>%
  dplyr::rename(OSHPD.ID = OSHPD.Facility.Number)
f1 <- f1 %>%
  dplyr::rename(OSHPD.ID = OSHPD.Facility.Number)
g1 <- g1 %>%
  dplyr::rename(Type.Of.Control = Type.of.Control)
h1 <- h1 %>%
  dplyr::rename(Type.Of.Control = Type.of.Control)
i1 <- i1 %>%
  dplyr::rename(OSHPD.ID = Oshpd_ID,
                Facility.Name = Facility_Name,
                Type.Of.Control = Type_Of_Control,
                County.Name = County_Name)
j1 <- j1 %>%
  dplyr::rename(OSHPD.ID = OSHPD.Facility.Number)
k1 <- k1 %>%
  dplyr::rename(OSHPD.ID = OSHPD.Facility.Number)
# Combine dataframes to one
dt0 <- do.call("rbind", list(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1))
# Summarise
dt <- dt0 %>% group_by(OSHPD.ID,Type.Of.Control,Facility.Name,Year,County.Name,variable,value) %>% 
  summarise(Count = sum(Count)) %>% na.omit()
# 32,418 observations before na.omit
# 29,378 observations after na.omit
# Set as factor
dt$variable <- as.factor(dt$variable)
dt$value <- as.numeric(dt$value)
dt$County.Name <- as.factor(dt$County.Name)
dt$Type.Of.Control <- as.factor(dt$Type.Of.Control)
dt$Facility.Name <- as.factor(dt$Facility.Name)
# View data
hist(dt$value)
hist(dt$Year)
plot(dt$Type.Of.Control)
plot(dt$County.Name)
plot(dt$variable)
ggplot(dt, aes(Count,variable)) + geom_boxplot()
# #Transform Data (Attempt 1)
# a <- a %>% 
#   dcast(Oshpd_ID~Admission_Source_Site)
# b <- b %>% 
#   dcast(Oshpd_ID~Admission_Type)
# c <- c %>% 
#   dcast(OSHPD.Facility.Number~Age.Groups)
# d <- d %>% 
#   dcast(OSHPD.ID~Disposition)
# e <- e %>% 
#   dcast(OSHPD.Facility.Number~Expected.Payer)
# f <- f %>% 
#   dcast(OSHPD.Facility.Number~Gender)
# g <- g %>% 
#   dcast(OSHPD.ID~Principal.Injury.Group)
# h <- h %>% 
#   dcast(OSHPD.ID~Principal.Diagnosis.Group)
# i <- i %>% 
#   dcast(Oshpd_ID~Principal_Procedure_Group)
# j <- j %>% 
#   dcast(OSHPD.Facility.Number~Race.Group)
# k <- k %>% 
#   dcast(OSHPD.Facility.Number~Type.Of.Care)
# l <- l %>% 
#   select(-c(Admission_Source_Site,Count,Year)) 
# l <- l %>% 
#   distinct()
# #Joined datasets
# data <- left_join(a,b,by='Oshpd_ID') %>% 
#   left_join(.,c,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
#   left_join(.,d,by=c('Oshpd_ID'='OSHPD.ID')) %>% 
#   left_join(.,e,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
#   left_join(.,f,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
#   left_join(.,g,by=c('Oshpd_ID'='OSHPD.ID')) %>% 
#   left_join(.,h,by=c('Oshpd_ID'='OSHPD.ID')) %>% 
#   left_join(.,i,by=c('Oshpd_ID'='Oshpd_ID')) %>% 
#   left_join(.,j,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
#   left_join(.,k,by=c('Oshpd_ID'='OSHPD.Facility.Number')) %>% 
#   left_join(.,l,by=c('Oshpd_ID'='Oshpd_ID')) %>% 
#   na.omit()
# data <- data.frame(data)
#Look at data
dt <- dt %>% na.omit()
dim(dt)
# 253,563 observations 8 variables before omitting n/a's
# 243,811 observations 8 variables after omitting n/a's
# 96.15% retained
# Plots
dt <- data.frame(dt)
stripchart(Count~value+variable, vertical=T,pch=1,data=dt)
# Santa Clara County
sc <- subset(dt, County.Name == "SANTA CLARA")
dim(sc)
ggplot(sc,aes(Count,value)) + geom_boxplot()














