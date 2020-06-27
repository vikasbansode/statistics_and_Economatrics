# INFERENTIAL STATISTICS
# Hypothesis Testing - parametric Test*

# Z-test one-sided
# H0 : Male smoker is greater than female smoker.
# CI : 95 %

t <- table(Gender,Smoke);t

# Make a Barplot to examine the distribution of data

barplot(t,beside = TRUE)

prop.test(t,correct = FALSE,alternative = 'greater')

# Z-test two-sided

# H0 : female smoker is equal to male smoker
# CI : 95%

prop.test(t,correct = FALSE)

# T-test one-side
# H0 : mu < 8
# one sided 95% cI for mu

t.test(LungCap,mu=8,alternative = "less",conf.level = 0.95)

# t-test two-side - is parametric methods appropriate for examining the difference in means for 2 population.
boxplot(LungCap ~ Smoke)

# Ho : mean lung cap of smokers = of non-smokers
# assume non-equal variances

t.test(LungCap ~ Smoke,mu=0,alt='two.side',conf=0.95,var.eq=F,paired =F)

# Chi-square - Appropriate for testing independence between two categorical variables.

# For chi-square test produce a contigency table

TAB <- table(Gender,Smoke)

# produce a barplot to check the distribution.

barplot(TAB,beside = T,legend=T)

CHI <- chisq.test(TAB,correct = T)
CHI

# ANOVA - appropriate for comparing the means for 2 or more independent populations.
# produce a box plot to check the distribution of Lung cap variable and gender variable.

boxplot(LungCap ~ Gender)

# H0 : Mean Lungcap  is the same for all Genders

ANOVA1 <-  aov(LungCap ~ Gender)
summary(ANOVA1)

TukeyHSD(ANOVA1)

plot(TukeyHSD(ANOVA1),las=1)

# Hypothesis Testing - Non-Parametric Test

# Wilcoxon Signed Rank Test- Appropriate for examining the median Difference in observations for 2 populations.



# Mann-Whitney U Test A.K.A Wilcoxon Rank sum test - appropriate for examining the difference in Medians for 2 independent populations

# Ho : Median Lung Capacity of Smokers  = that of non smokers
# two sided test

wilcox.test(LungCap ~ Smoke,mu=0,alt='two.sided',conf.int=T,conf.level = 0.95,paired=F,exact=F,correct =T)

# Kruskal Wallis Test - equivalent to one-way Analysis of Variance

kruskal.test(LungCap ~ Gender)

# Fisher's Exact test - alternative to the chi-square test, it is used when the assumptions of chi-square test not met.we may consider using Fisher's Exact Test

fisher.test(TAB,conf.int = T,conf.level = 0.99)

