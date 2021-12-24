library(car)
library(asbio)

data = hollywoodmovies
study = hollywoodmovies
### Question 1
# Q1a Built-in Function Version
t.test(study$Budget,conf.level = 0.95)
t.test(study$DomesticGross,conf.level = 0.95)
t.test(study$OpeningWeekend,conf.level = 0.95)
# Median
ci.median(study$Budget, conf = 0.95)
ci.median(study$DomesticGross, conf = 0.95)
ci.median(study$OpeningWeekend, conf = 0.95)

#Q1a Manual Calculation Version
n = length(study$Budget)
Z = qnorm(0.975)

meanBudget = mean(study$Budget)
meBudget = Z*(sd(study$Budget)/sqrt(n))
UpperBudget = mean(study$Budget) + meBudget
LowerBudget = mean(study$Budget) - meBudget
meanBudget
UpperBudget
LowerBudget

meanDG = mean(study$DomesticGross)
meDG = Z*(sd(study$DomesticGross)/sqrt(n))
UpperDG = mean(study$DomesticGross) + meDG
LowerDG = mean(study$DomesticGross) - meDG
meanDG
UpperDG
LowerDG

meanOW = mean(study$OpeningWeekend)
meOW = Z*(sd(study$OpeningWeekend)/sqrt(n))
UpperOW = mean(study$OpeningWeekend) + meOW
LowerOW = mean(study$OpeningWeekend) - meOW
meanOW
UpperOW
LowerOW

#### Question 2 
c = subset(study,Genre=="Comedy")
d = subset(study,Genre=="Drama")
Q2 = rbind(c,d)

##########Q2-Budget
dobsBudget = mean(Q2$Budget[Q2$Genre=="Comedy"])-mean(Q2$Budget[Q2$Genre=="Drama"])
d=c()
p=c()
for(i in 1:5000){
  permut=sample(Q2$Budget)
  d[i]=mean(permut[Q2$Genre=="Comedy"])-mean(permut[Q2$Genre=="Drama"])
  p[i]=(d[i]>=dobsBudget)+0
}
pvalueBudget=sum(p)/5000
pvalueBudget

t.test(Q2$Budget[Q2$Genre=='Comedy'], Q2$Budget[Q2$Genre=='Drama'], alternative = 'greater')
wilcox.test(Q2$Budget[Q2$Genre=="Comedy"], Q2$Budget[Q2$Genre=="Drama"], alternative = 'greater')

##########Q2-Domestic Gross
dobsDG = mean(Q2$DomesticGross[Q2$Genre=="Comedy"])-mean(Q2$DomesticGross[Q2$Genre=="Drama"])
d=c()
p=c()
for(i in 1:5000){
  permut=sample(Q2$DomesticGross)
  d[i]=mean(permut[Q2$Genre=="Comedy"])-mean(permut[Q2$Genre=="Drama"])
  p[i]=(d[i]>=dobsDG)+0
}
pvalueDG=sum(p)/5000
pvalueDG

t.test(Q2$DomesticGross[Q2$Genre=='Comedy'], Q2$DomesticGross[Q2$Genre=='Drama'],alternative = 'greater')
wilcox.test(Q2$DomesticGross[Q2$Genre=="Comedy"], Q2$DomesticGross[Q2$Genre=="Drama"], alternative = 'greater')
?wilcox.test
##########Q2-Opening Weekend
dobsOW = mean(Q2$OpeningWeekend[Q2$Genre=="Comedy"])-mean(Q2$OpeningWeekend[Q2$Genre=="Drama"])
d=c()
p=c()
for(i in 1:5000){
  permut=sample(Q2$OpeningWeekend)
  d[i]=mean(permut[Q2$Genre=="Comedy"])-mean(permut[Q2$Genre=="Drama"])
  p[i]=(d[i]>=dobsOW)+0
}
pvalueOW=sum(p)/5000
pvalueOW

t.test(Q2$OpeningWeekend[Q2$Genre=='Comedy'], Q2$OpeningWeekend[Q2$Genre=='Drama'], alternative = 'greater')
wilcox.test(Q2$OpeningWeekend[Q2$Genre=="Comedy"], Q2$OpeningWeekend[Q2$Genre=="Drama"], alternative = 'greater')

#Non parametric tests are used when your data isn't normal. Therefore the key is to 
#figure out if you have normally distributed data. For example, you could look at the 
#distribution of your data. If your data is approximately normal, then you can use parametric 
#statistical tests.
library(car)
par(mfrow=c(2,3))
qqPlot(Q2$Budget[Q2$Genre=="Comedy"],xlab = 'Normal Quantiles', ylab = 'Comedy Budget')
qqPlot(Q2$Budget[Q2$Genre=="Drama"],xlab = 'Normal Quantiles', ylab = 'Drama Budget')
qqPlot(Q2$DomesticGross[Q2$Genre=='Comedy'],xlab = 'Normal Quantiles', ylab = 'Comedy Domestic Gross')
qqPlot(Q2$DomesticGross[Q2$Genre=='Drama'],xlab = 'Normal Quantiles', ylab = 'Drama Domestic Gross')
qqPlot(Q2$OpeningWeekend[Q2$Genre=='Comedy'],xlab = 'Normal Quantiles', ylab = 'Comedy Opening Weekend')
qqPlot(Q2$OpeningWeekend[Q2$Genre=='Drama'],xlab = 'Normal Quantiles', ylab = 'Drama Opening Weekend')

# Normality test using Shapiro-Wilk normality test
shapiro.test(Q2$Budget[Q2$Genre=="Comedy"])
shapiro.test(Q2$Budget[Q2$Genre=="Drama"])
shapiro.test(Q2$DomesticGross[Q2$Genre=='Comedy']) #Not normal
shapiro.test(Q2$DomesticGross[Q2$Genre=='Drama']) #Not normal
shapiro.test(Q2$OpeningWeekend[Q2$Genre=='Comedy']) #Not normal
shapiro.test(Q2$OpeningWeekend[Q2$Genre=='Drama'])
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different 
# from the normal distribution. In other words, we can assume the normality.

# Question 3
# Budget
HT = c(data$Budget[data$Genre=='Horror'], data$Budget[data$Genre=='Thriller'])
AFR = c(data$Budget[data$Genre=='Animation'], data$Budget[data$Genre=='Fantasy'], data$Budget[data$Genre=='Romance'])
AA = c(data$Budget[data$Genre=='Action'],data$Budget[data$Genre=='Adventure'])

# Domestic Gross
HT = c(data$DomesticGross[data$Genre=='Horror'], data$DomesticGross[data$Genre=='Thriller'])
AFR = c(data$DomesticGross[data$Genre=='Animation'], data$DomesticGross[data$Genre=='Fantasy'], data$DomesticGross[data$Genre=='Romance'])
AA = c(data$DomesticGross[data$Genre=='Action'],data$DomesticGross[data$Genre=='Adventure'])

# Opening Weekend
HT = c(data$OpeningWeekend[data$Genre=='Horror'], data$OpeningWeekend[data$Genre=='Thriller'])
AFR = c(data$OpeningWeekend[data$Genre=='Animation'], data$OpeningWeekend[data$Genre=='Fantasy'], data$OpeningWeekend[data$Genre=='Romance'])
AA = c(data$OpeningWeekend[data$Genre=='Action'],data$OpeningWeekend[data$Genre=='Adventure'])

# HT vs. AFR
data3 = data.frame(
  y = c(HT,AFR),
  x = factor(rep(c('HT','AFR'), times = c(length(HT), length(AFR))))
)
fit = lm(y~x, data = data3)
anova(fit)


# HT vs. AA
data3 = data.frame(
  y = c(HT,AA),
  x = factor(rep(c('HT','AA'), times = c(length(HT), length(AA))))
)
fit = lm(y~x, data = data3)
anova(fit)

# AFR vs. AA
data3 = data.frame(
  y = c(AFR,AA),
  x = factor(rep(c('AFR','AA'), times = c(length(AFR), length(AA))))
)
fit = lm(y~x, data = data3)
anova(fit)

### Question 4
#Correlation
Act = subset(study,Genre=="Action")
Act
plot(Act$OpeningWeekend, Act$DomesticGross, main = 'Action Movies \nDomestic Gross vs. Opening Weekend',
     xlab = 'Opening Weekend',
     ylab = 'Domestic Gross')
fit = lm(Act$DomesticGross ~ Act$OpeningWeekend)
fit
abline(fit)
Pearson = cor(Act$OpeningWeekend,Act$DomesticGross)
Pearson
Spearman = cor(Act$OpeningWeekend,Act$DomesticGross, method = "spearman")
Spearman


#Regression
plot(study$Budget,study$DomesticGross, main = 'Budget vs. Domestic Gross',
     xlab = 'Budget',
     ylab = 'Domestic Gross')
fit = lm(study$DomesticGross ~ study$Budget)
abline(fit)


#### Question 5 
domestic = data$DomesticGross
foreign = data$ForeignGross

# Test for normality
qqPlot(domestic)
shapiro.test(domestic) # p-value = 4.203e-12
legend(-2.5,350, legend = c('p.value = 4.203 e-12'))

qqPlot(foreign)
shapiro.test(foreign) # p-value = 4.249e-16
legend(-2.5,850, legend = c('p.value = 4.249 e-16'))

# Since the data is not normal, we should not use parametric teset; instead, use non-parametric
# H0: domestic = Foreign, Ha = Domestic <= Foreign

# Parametric Test
t.test(domestic, foreign, alternative = 'greater')


# Non-parametric Test
# Independent 2 groups Mann-Whitnesy U test
wilcox.test(domestic, foreign, alternative = 'greater')

# Kruskal Wallis Test One Way ANOVA by ranks
kruskal.test(domestic,foreign, alternative = 'greater')


