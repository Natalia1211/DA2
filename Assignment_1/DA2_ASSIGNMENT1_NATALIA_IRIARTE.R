
## Clean workspace ##

rm(list=ls())


## Prepare Packages ##
library(tidyverse)
library(modelsummary)


## Load Database ##

Database <- read_csv( 'https://osf.io/4ay9x/download' ) 

## Filter Data ##

DB <- Database %>% filter(age>=30, age<=70,
                          occ2012 == 0010,
                          lfsr94 == 'Employed-At Work',
                          earnwke >0,
                          grade92>=43)

## Create Variables ##

DB$FEM <- as.numeric(DB$sex == 2)
DB$BARC <- as.numeric(DB$grade92 == 43)
DB$MAST <- as.numeric(DB$grade92 == 44)
DB$PROF <- as.numeric(DB$grade92 == 45)
DB$PHD <- as.numeric(DB$grade92 == 46)
DB$LOGWG <- log((DB$earnwke/DB$uhours))


## Simple Regression ##

reg1 <- lm(DB$LOGWG ~ DB$FEM, data = DB)
summary(reg1)

## Multiple Regression #


reg2 <- lm( DB$LOGWG ~ DB$FEM + DB$PROF, data = DB)
summary(reg2)

reg3 <- lm( DB$LOGWG ~ DB$FEM + DB$PROF +DB$MAST, data = DB)
summary(reg3)

reg4 <- lm( DB$LOGWG ~ DB$FEM + DB$PROF +DB$PHD, data = DB)
summary(reg4)

reg5 <- lm( DB$LOGWG ~ DB$FEM + DB$PROF + DB$MAST + DB$MAST*DB$FEM, data = DB)
summary(reg5)


## GRAPH ##

DB$ED_LEVEL <- as.character(ifelse(DB$grade92 == 43, "BARCHELOR", 
                                   ifelse(DB$grade92 == 44, "MASTER", 
                                          ifelse(DB$grade92 == 45, "PROFESSIONAL", 
                                                 ifelse(DB$grade92 == 46, "PhD", "N/A")))))



DB$GENDER <- as.character(ifelse(DB$sex == 1, "MALE",'FEMALE'))

ggplot(DB, aes(x = DB$GENDER, y = mean(DB$earnwke/DB$uhours))) +
  geom_col( ) + facet_wrap(~DB$ED_LEVEL) +
  labs( x = 'gender' , y = 'Average wage') +theme_test()

