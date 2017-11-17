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
#use lm function to look at parameter estimates
drug.mod1=lm(pain_minutes~drug, data=df)	
summary(drug.mod1)

#Call:
#lm(formula = pain_minutes ~ drug, data = df)

#Residuals:
#   Min     1Q Median     3Q    Max 
#-6.750 -4.750  0.250  3.375  9.250 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   18.750      2.945   6.367  0.00013 ***
#drugB         -1.500      4.165  -0.360  0.72705    
#drugC          8.000      4.165   1.921  0.08695 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.89 on 9 degrees of freedom
#Multiple R-squared:  0.4006,    Adjusted R-squared:  0.2674 
#F-statistic: 3.007 on 2 and 9 DF,  p-value: 0.09995

##The model output shows some evidence of a difference in drug c. 
##an analysis of variance table can be created for this model

anova(drug.mod1)

Analysis of Variance Table

#Response: pain_minutes
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#drug       2 208.67 104.333  3.0072 0.09995 .
#Residuals  9 312.25  34.694                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##this shows there are differences between the groups
##confint can calculate the confidence intervals with default 95%

confint(drug.mod1)

#                 2.5 %    97.5 %
#(Intercept)  12.087722 25.412278
#drugB       -10.921884  7.921884
#drugC        -1.421884 17.421884

##This model can be plotted against fitted values to investigate model assumptions

drug.mod = data.frame(Fitted = fitted(drug.mod1),
  Residuals = resid(drug.mod1), Treatment = df$drug)

##create the plot

ggplot(drug.mod, aes(Fitted, Residuals, colour = drug)) + geom_point()

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

