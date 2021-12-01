library(tidyverse)
library(skimr)
library(janitor)
library(here)

load("original_data/37166-0007-Data.rda")


df1 <- da37166.0007 %>% dplyr::select(COHORT,W1DUDIT_I,W2DUDIT_I,W1GENDER,
                     W1AGE,W1SEX,W1SEX_GENDER,W2SEXMINID,W1Q166,W1SEXMINID,
                     GURBAN_I,W1Q32,W1Q169,
                     GEDUC1,GEDUC2,GEMPLOYMENT2010,W1PINC,W1POVERTY,
                     W1Q03,W2Q100,W1Q180,
                     W1ACE_EMO_I,W1ACE_INC_I,W1ACE_IPV_I,W1ACE_PHY_I,
                     W1ACE_SEX_I,W1ACE_SUB_I,W1Q162,SCREEN_RACE,
                     W1FELTSTIGMA,W1INTERNALIZED,W1WEIGHT_ORIG)

skim(df1)
df1 %>% tabyl(W1SEX)
df1 %>% tabyl(W1GENDER)
df1 %>% tabyl(W1FELTSTIGMA)
#Create new categorical DUDIT Variable based on the ranges
#w/ different range for DUDIT based on males and GNC/Females
DUDIT <- factor(c("no_use","moderate","drug_abuse","extreme_abuse"))
female <- df1 %>% filter(W1SEX == "(1) Female") %>% 
  mutate(DUDIT = { case_when(W1DUDIT_I == 0 ~ 1,
                                 W1DUDIT_I == 1 ~ 2,
                                 W1DUDIT_I >= 2 & W1DUDIT_I <=24 ~ 3,
                                 W1DUDIT_I >= 25 ~ 4) 
  })

DUDIT <- factor(c("no_use","moderate","drug_abuse","extreme_abuse"))

class(W1SEX)
male <- df1 %>% filter(W1SEX == "(2) Male") %>% 
  mutate(DUDIT = { case_when(W1DUDIT_I == 0 ~ 1,
                             W1DUDIT_I >= 1 & W1DUDIT_I<= 5 ~ 2,
                             W1DUDIT_I >= 6 & W1DUDIT_I <=24 ~ 3,
                             W1DUDIT_I >= 25 ~ 4)
  })

female %>% tabyl(DUDIT)
male %>% tabyl(DUDIT)

df1 <- rbind(female,male)
df1$DUDIT = as_factor(df1$DUDIT)
class(df1$DUDIT)
levels(df1$DUDIT) <- list(no_use = 1,
                          moderate = 2,
                          drug_abuse = 3,
                          extreme_abuse = 4)

df1 %>% tabyl(DUDIT) %>% adorn_totals()

#Rename Variables
ls(df1)
df1 <- df1 %>% rename(urban = "GURBAN_I",emotional_abuse = "W1ACE_EMO_I",
               IPV_abuse = "W1ACE_IPV_I", physical_abuse = "W1ACE_PHY_I",
               sexual_abuse="W1ACE_SEX_I",incaceration_parent = "W1ACE_INC_I",
               substance_abuse_parent="W1ACE_SUB_I", 
               poverty = "W1POVERTY",happiness = "W1Q03", 
               household_income = "W1PINC", bullying= "W1Q162", 
               US_born = "W1Q166", have_children = "W1Q169", 
               child_religion = "W1Q180",relationship_status ="W1Q32",
               suicide="W2Q100",gender = "W1GENDER",sex_gender = "W1SEX_GENDER",
               sexual_orientation = "W1SEXMINID",race = "SCREEN_RACE",
               wave2_sexual_orientation = "W2SEXMINID",felt_stigma= "W1FELTSTIGMA",
               internal_homophobia = "W1INTERNALIZED",survey_weights="W1WEIGHT_ORIG")
skim(df1)
#Next - revise variable names and factor names, relevel, select, etc
df1 %>% sapply(levels)
df1$COHORT = fct_recode(df1$COHORT,young = "(1) Younger",middle="(2) Middle",
                   older= "(3) Older")
df1$gender <- fct_recode(df1$gender,woman = "(1) Woman",man="(2) Man",non_binary= "(5) Non-binary/GQ")

skim(df1)

#Re-catogorize race dummy variables
df1 = df1 %>% mutate(white = ifelse(race == "(1) White",1,0),
               black = ifelse(race == "(2) Black/African American",1,0),
               latino = ifelse(race == "(3) Latino/Hispanic",1,0))
tabyl(df1$race) %>% adorn_totals()
tabyl(df1$latino)
df1$race <- fct_recode(df1$race,white = "(1) White",black="(2) Black/African American",latino= "(3) Latino/Hispanic")
df1$race = fct_drop(df1$race)
levels(df1$race)

df1$white <- as_factor(df1$white)
df1$black <- as_factor(df1$black)
df1$latino <- as_factor(df1$latino)

ls(df1)
levels(df1$incaceration_parent)
skim(df1)
df1$emotional_abuse <- fct_recode(df1$emotional_abuse,no = "(0) (No, event never occurred)",
                       yes="(1) (Yes, event occurred at least once)")
df1$IPV_abuse <- fct_recode(df1$IPV_abuse,no = "(0) (No, event never occurred)",
           yes="(1) (Yes, event occurred at least once)")
df1$incaceration_parent <- fct_recode(df1$incaceration_parent,no = "(0) (No, event never occurred)",
           yes="(1) (Yes, event occurred at least once)")
df1$sexual_abuse <- fct_recode(df1$sexual_abuse,no = "(0) (No, event never occurred)",
           yes="(1) (Yes, event occurred at least once)")
df1$substance_abuse_parent <- fct_recode(df1$substance_abuse_parent,no = "(0) (No, event never occurred)",
           yes="(1) (Yes, event occurred at least once)")


skim(df1)

save(df1,file="data.rda",)
skim(df1)


library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(MASS)
library(brant)

load("data.Rda")
skim(df1)
df1$DUDIT = ordered(df1$DUDIT,levels=c("no_use","moderate","drug_abuse","extreme_abuse"))

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(survey)

df2 = df1[,!(names(df1) %in% c("wave2_sexual_orientation","W2DUDIT_I","suicide"))]
ls(df2)
dim(df2)
df2 = drop_na(df2)
skim(df2)

save(df2,file="mydata.rda")


#export citations to .bib for R packages used
library(bibtex)
library(RefManageR)
library(knitr)

references <- c("tidyverse","knitr","survey","ggplot2",
                "skimr","janitor","foreign","MASS",
                "Hmisc","reshape2","patchwork","stargazer",
                "here")
knitr::write_bib(references, file = "references.bib")


