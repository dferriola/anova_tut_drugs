library(tidyr)
library(faraway)
library(ggplot2)
library(ggthemes)
pain_minutes=c(14,24,12,25,20,14,17,18,22,29,36,20)
drug=c(rep("A",4), rep("B",4), rep("C",4))
df=data.frame(pain_minutes,drug)
attach(df)
str(df)
summary(df)
plot(pain_minutes~drug, data=df)
pl=ggplot(df, aes (x=drug, y=pain_minutes))+
	geom_boxplot(fill="grey90", colour="steelblue")
pl+theme_fivethirtyeight()	
#aov() to fit ANOVA models
results=aov(pain_minutes~drug, data=df)
summary(results)
#ANOVA F-test helps to see if there are differences in the K means. Pairwise t test
#computes the pair wise compare between groups and corrects 
pairwise.t.test(pain_minutes,drug, p.adjust="bonferroni")
#    Pairwise comparisons using t tests with pooled SD 

#data:  pain_minutes and drug 

#  A    B   
#B 1.00 -   
#C 0.26 0.15

#P value adjustment method: bonferroni

##Not significantly different between drugs A and B (p-value=1.00) but
##different for C because p-values are 0.26 and 0.15 respectively

##Tukey Method creates a confidence interval
TukeyHSD(results, conf.level=0.95)
#  Tukey multiple comparisons of means
#    95% family-wise confidence level

#Fit: aov(formula = pain_minutes ~ drug, data = df)

#$drug
#    diff        lwr     upr     p adj
#B-A -1.5 -13.128702 10.1287 0.9315326
#C-A  8.0  -3.628702 19.6287 0.1883467
#C-B  9.5  -2.128702 21.1287 0.1102074

##From this we can see that C-A and C-B are significant (p-value 0.1883 and p-value 0.1102)
##and B-A is not significant (p-value 0.9315) and confirms the Bonferroni correction
