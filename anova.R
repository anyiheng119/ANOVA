library(tidyverse)

setwd('C:/Users/any/Documents/WFU MSBA Courses/Process and Supply Analytics 2023 Spring/ANOVA')

# data
df_blood = read.csv('Blood Pressure Data.csv')
df_dimension = read.csv('Dimension Data.csv')
df_motor = read.csv('Motor Life.csv')

# have a glimpes
df_blood %>% glimpse() 
df_dimension %>% glimpse()
df_motor %>% glimpse()

# Pre-model EDA: sample sizes for each group (level)
df_blood %>%
  group_by(Drug) %>%
  summarise(count = sum(!is.na(BP.Change))) %>%
  ggplot(aes(x = (Drug), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  ylab('Sample size')


# Pre-model EDA
df_blood %>% 
  group_by(Drug) %>% 
  summarize(mean = mean(BP.Change), var = var(BP.Change), median = median(BP.Change))

ggplot(df_blood, aes(x = Drug, y = BP.Change)) + 
  geom_boxplot()

# anova
blood_aov <- aov(BP.Change~Drug, data = df_blood)
summary(blood_aov)

# see results
par(mfrow=c(2, 2))
plot(blood_aov)
# The Q-Q plot: check if the residuals are fairly normal
# The Residuals vs Fitted values plot: check if the variances are homogeneous (i.e. with no patterns)


# test of homogeneity of variances
## H0: homogeneous variances
## H1: not homogeneous variances
## If we reject H0, ANOVA might not be the best choice for this experiment
bartlett.test(BP.Change ~ Drug, data = df_blood)



# If the assumptions of ANOVA do not hold true...
# one non-parametric alternative to ANOVA is the Kruskal-Wallis rank sum test
# The low p-value indicates that based on this test, we can be confident across this experiment A varies by B
kruskal.test(BP.Change ~ Drug, data = df_blood)





library(tidyverse)
df_dimension = read.csv('Dimension Data.csv')

# Transpose data frame
df_dimension_stack <- stack(df_dimension)
names(df_dimension_stack)=c("Values","Machine")
df_dimension_stack %>% glimpse()

# Pre-model EDA: sample sizes for each group (level)
# Note: Equal sample sizes is NOT one of the assumptions made in an ANOVA
df_dimension_stack %>%
group_by(Machine) %>%
  summarise(count = sum(!is.na(Values))) %>%
  ggplot(aes(x = (Machine), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  ylab('Sample size')
       

# Pre-model EDA
ggplot(df_dimension_stack, aes(x = Machine, y = Values)) + 
  geom_boxplot()

# anova
dimension_aov <- aov(Values~Machine, data = df_dimension_stack)
summary(dimension_aov)

# see results
par(mfrow=c(2, 2))
plot(dimension_aov)


# test of homogeneity of variances
## H0: homogeneous variances
## H1: not homogeneous variances
## If we reject H0, ANOVA might not be the best choice for this experiment
bartlett.test(Values~Machine, data = df_dimension_stack)




# Transpose data frame
df_motor_stack <- stack(df_motor)
names(df_motor_stack)=c("Values","Brand")
df_motor_stack %>% glimpse()


# Pre-model EDA: Box plots
ggplot(data = df_motor_stack, aes(x = Brand, y = Values)) +
  geom_boxplot() +
  xlab("Brand")

# Pre-model EDA: sample sizes for each group (level)
# Note: Equal sample sizes is NOT one of the assumptions made in an ANOVA
df_motor_stack %>%
  group_by(Brand) %>%
  summarise(count = sum(!is.na(Values))) %>%
  ggplot(aes(x = (Brand), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  ylab('Sample size')


# anova
motor_aov <- aov(Values~Brand, data = df_motor_stack)
summary(motor_aov)


# see results
par(mfrow=c(2, 2))
plot(motor_aov)


# test of homogeneity of variances
## H0: homogeneous variances
## H1: not homogeneous variances
## If we reject H0, ANOVA might not be the best choice for this experiment
bartlett.test(Values~Brand, data = df_motor_stack) # cannot reject H0






