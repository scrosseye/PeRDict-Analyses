## This document performs a linear model to predict lexical decision times for the
#10,000 must frequent words in English as reported in the ELP database based on 
#PeRDict rhyme counts and phonological neighborhood effects

################################################################################
# Load Libraries
################################################################################

# If you don't have these libraries installed, you will have to install them
#library(caret)#machine learning
library(dplyr)#data analytics
library(psych)#descriptives
library(tidyverse)#data analytics
#library(leaps)#stepwise
library(car)#vif
library(relaimpo)#variable importance
library(Hmisc)#correlations

################################################################################
# clear previous data and max printing
################################################################################


rm(list=ls(all=TRUE)) #clear memory
options(max.print=1000000)


################################################################################
# Prepare Data
################################################################################

# read in file that contains all variables with high non-zero counts
data <- read.csv(file = 'rhymes_elp_all_pruned_freq_final.csv')


# examine data
str(data)


######################This is for 10000 most frequent words###################

#select data you want
data_sel <- data[, c(27, 17, 2:14)] #select only columns that are worth it
str(data_sel)

data_10000 <- data_sel %>% 
  filter(freq_10000 == "yes")

data_10000 <- data_10000[, c(2:15)]
str(data_10000)

#describe the data
psych::describe(data_10000) #seems okay


# scale linguistic data 
data_10000 <- data_10000 %>%
  mutate_at(c(1:14), funs(c(scale(.))))

# ensure data is mean centered
psych::describe(data_10000)



################################################################################
#break into training and test
################################################################################

#select only the variables that are not multicollinear including the DV. 
#this is from a previous analysis that pruned variables based on multicollinearity
str(data_10000)
data_source <- data_10000[, c(1, 3:5, 7:8, 11:12)] #select only column 2-9
str(data_source)

#break data into training and test sets

set.seed(1234)

dt = sort(sample(nrow(data_source), nrow(data_source)*.7))
train<-data_source[dt,]
test<-data_source[-dt,]


################################################################################
# Correlation analysis on training set
################################################################################

#correlateion
cor(train)

#correlation matrix with p values (requires Hmisc)
correl2 <- rcorr(as.matrix(train)) 
correl2

# Extract the correlation coefficients
corr_r <- correl2$r

#round them to 3 decimals
corr_r_round <- round(corr_r, 3)
corr_r_round

#write to csv
write.csv(write.csv(corr_r_round,"corr_matrix_all_varaibles_LD_train_freq_10000.csv"))

################################################################################
# LM Modeling with train and test set
################################################################################
#conducted a final LM model to get all the stuff using the training set only.

final <- lm(formula = I_Zscore ~ ., data = train)

#summary of model with additional statistics
summary (final)

#include only those that are significant predictors and no suppresion

final2 <- lm(formula = I_Zscore ~
             num_rhymes_1000_coca
             +num_rhymes_2500_coca
             +num_rhymes_5000_coca
             +Ortho_N
             +OLD,
             data = train)

summary (final2)

#remove those that are no significant now or have suppression
final3 <- lm(formula = I_Zscore ~
              num_rhymes_1000_coca
             +num_rhymes_2500_coca
             +OLD,
             data = train)
               
               
summary (final3)

#this is the r value
.573*.573

#calculate variable importance metrics. This will likely require a high-performance machine
metrics_w_types_all <- calc.relimp(final3)#, type = c("lmg", "pmvd", "first", "last", "betasq", "pratt"))
metrics_w_types_all

calc.relimp(final3,type=c("lmg","last","first","pratt"),rela=TRUE)
#this reports percentage of importance by variable that adds up to 100

car::vif(final3) #VIF values for the regression to ensure no problems with multi-collinearity


#normality. Can't run. Sample size too big!
shapiro.test(residuals(final3)) #residual plot to check for normality

#Homoscedasticity check
plot(final3, which = 1) #residual plot


########predict on training set####################

pred_model <- predict(final3, newdata = test)

#get correlation and r squared

r_test <- cor(test$I_Zscore, pred_model)
r_test
r_test^2 #### 0.3186974
