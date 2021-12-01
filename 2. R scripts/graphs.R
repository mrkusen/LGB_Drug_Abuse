library(tidyverse)
library(patchwork)
library(janitor)

load("data5.Rda")

#Y variable
df5 %>% tabyl(DUDIT_binary) %>% adorn_totals()

ls(df5)
#X variables
df5 %>% tabyl(W1SEX,gender)

df5 %>% tabyl(gender)
df5 %>% tabyl(COHORT)
df5 %>% tabyl(sexual_orientation)
df5 %>% tabyl(race)

df5 %>% tabyl(poverty)
df5 %>% tabyl(GEDUC2)

df5 %>% tabyl(happiness)
df5 %>% tabyl(bullying)

df5 %>% tabyl(sexual_abuse)
df5 %>% tabyl(incaceration_parent)
df5 %>% tabyl(substance_abuse_parent)
df5 %>% tabyl(IPV_abuse)


df5 %>% tabyl(sexual_orientation,race)


dudit <- ggplot(df5) +
  aes(x = DUDIT_binary, fill = DUDIT_binary) +
  geom_bar() +
  theme_classic()+
  theme(legend.position = "none")
dudit

class(df5$DUDIT_binary)

a <- ggplot(df5) +
  aes(x = sexual_abuse, fill = sexual_abuse) +
  geom_bar() +
  theme_classic()+
  theme(legend.position = "none")
a
class(df5$sexual_abuse)
b <- ggplot(df5) +
  aes(x = IPV_abuse, fill = IPV_abuse) +
  geom_bar() +
  theme_classic()+
  theme(legend.position = "none")
c <- ggplot(df5) +
  aes(x = incaceration_parent, fill = incaceration_parent) +
  geom_bar() +
  theme_classic()+
  theme(legend.position = "none")

d <- ggplot(df5) +
  aes(x = substance_abuse_parent, fill = substance_abuse_parent) +
  geom_bar() +
  theme_classic()+
  theme(legend.position = "none")

a+b+c+d+
  plot_annotation(title = 'Adverse Childhood Experiences',
    subtitle = 'Generations Study: Wave 1 - 2016-2017')

ggplot(df5) +
  aes(x = W1DUDIT_I, fill = DUDIT) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(title = "DUDIT original scores") +
  theme_minimal()

dudit_1 <- ggplot(df5) +
  aes(x = DUDIT, fill = DUDIT) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
dudit_1

dudit_2 <- ggplot(df5) +
  aes(x = DUDIT_binary, fill = DUDIT_binary) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_classic()+ labs(title="Drug Dependency of Lesian, Gay and Bisexual persons",
                        caption="Generations Study, Wave 1 (2016-17)",
                        fill="Drug Abuse")
dudit_2

ggplot(df5) +
 aes(x = W1DUDIT_I, fill = DUDIT_binary) +
 geom_histogram(bins = 25L) +
 scale_fill_manual(values = list(no = "#B6AD74", yes = "#859BFA")) +
  labs(title="Original DUDIT score against transformation
       of drug dependency variable",
       x="Original DUDIT score",fill="drug dependency")+
 theme_classic()




#Bivariate
df5 %>% tabyl(gender,DUDIT_binary) %>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(COHORT,DUDIT_binary) %>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(sexual_orientation,DUDIT_binary) %>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(race,DUDIT_binary) %>% adorn_totals() %>% adorn_percentages()

df5 %>% tabyl(poverty,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(GEDUC2,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(happiness,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()

df5 %>% tabyl(bullying,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()

df5 %>% tabyl(incaceration_parent,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(IPV_abuse,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(sexual_abuse,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()
df5 %>% tabyl(substance_abuse_parent,DUDIT_binary)%>% adorn_totals() %>% adorn_percentages()

#bivariate graphs

ggplot(df5) +
 aes(x = bullying, fill = DUDIT_binary) +
 geom_bar() +
 scale_fill_manual(values = list(no = "#C83A6D", 
 yes = "#6BCBF1")) +
 labs(title = "Drug Dependency by Childhood Experiences of Bullying", fill = "Drug Dependent") +
 theme_minimal()

ggplot(df5) +
  aes(x = sexual_abuse, fill = DUDIT_binary) +
  geom_bar() +
  scale_fill_manual(values = list(no = "#C83A6D", 
                                  yes = "#6BCBF1")) +
  labs(title = "Drug Dependency by Childhood Experiences of sexual abuse", fill = "Drug Dependent") +
  theme_minimal()

ggplot(df5) +
  aes(x = COHORT, fill = DUDIT_binary) +
  geom_bar() +
  scale_fill_manual(values = list(no = "#C83A6D", 
                                  yes = "#6BCBF1")) +
  labs(title = "Drug Dependency by Cohort", fill = "Drug Dependent") +
  theme_minimal()

