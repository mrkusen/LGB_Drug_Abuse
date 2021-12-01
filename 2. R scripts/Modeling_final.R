library(tidyverse)
library(here)
library(stargazer)
options(scipen = 999)

load('mydata.rda')

#binary variable - little to no use vs. dependent and extreme dependence
df5 <- df2 %>% mutate(DUDIT_binary = ifelse(DUDIT=="no_use"|DUDIT=="moderate",0,1))
df5 %>% tabyl(DUDIT_binary)
df5$DUDIT_binary <- as_factor(df5$DUDIT_binary)
levels(df5$DUDIT_binary)
df5$DUDIT_binary <- fct_recode(df5$DUDIT_binary,no = "0",yes="1")
class(df5$DUDIT_binary)

#review some key variables
df2 %>% tabyl(gender,sexual_orientation)
df5 %>% tabyl(substance_abuse_parent) %>% knitr::kable()
df5 %>% tabyl(W1SEX,gender)


#Model without Survey Weights
model4 <- glm(DUDIT_binary ~ COHORT+gender+sexual_orientation+
                race+poverty+GEDUC2+happiness+bullying+
                sexual_abuse+incaceration_parent+
                substance_abuse_parent+IPV_abuse+
                sexual_orientation*race,
              family=binomial,data = df5)
summary(model4)
exp(model4$coefficients)

save(df5,file="data5.rda")

# stargazer(skim_df6,type="html")
#EDA
# library(summarytools)
# summarytools::descr(df6)


#Using survey weights
survey_design <- svydesign(ids=~1,weights=~survey_weights,
                           data=df5)
survey_model <- svyglm(DUDIT_binary ~ COHORT+gender+sexual_orientation+
                         race+poverty+GEDUC2+happiness+bullying+
                         sexual_abuse+incaceration_parent+
                         substance_abuse_parent+IPV_abuse+
                         sexual_orientation*race,
                       data=df5, family=stats::binomial(link=logit),design=survey_design,
                       subset=NULL,start=NULL, rescale=TRUE, 
                       deff=FALSE,influence=FALSE)
summary(survey_model)
exp(survey_model$coefficients)
df5 %>% tabyl(DUDIT_binary,bullying)

summary(model4)

#export logistic regression and odds ratios to word document for use in research paper
stargazer(survey_model, t.auto=F, p.auto=F, report = "vcs*",
          type="html", out="log_model.doc")
stargazer(survey_model, apply.coef=exp, t.auto=F, p.auto=F, report = "vcs*",
          type="html", out="log_model_OR.doc")
