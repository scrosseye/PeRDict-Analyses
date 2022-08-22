## This document performs a linear model to predict lexical decision times for the
#all words in English as reported in the ELP database based on  PeRDict rhyme counts 
#and phonological neighborhood effects

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

#this is for tokens, not lemmas
################################################################################

# read in file that contains all variables with high non-zero counts
data <- read.csv(file = 'rhymes_elp_all_pruned_final.csv')


# examine data
str(data)

#select data you want
data_sel <- data[, c(17, 2:14)] #select only columns that are worth it
str(data_sel)

#describe the data
psych::describe(data_sel) #seems okay


# scale linguistic data 
data_sel1 <- data_sel %>%
  mutate_at(c(1:14), funs(c(scale(.))))

# ensure data is mean centered
psych::describe(data_sel1)



################################################################################
#break into training and test
################################################################################

#select only the variables that are not multicollinear including the DV. 
#this is from a previous analysis that pruned variables based on multicollinearity
#this is for the entire dataset
str(data_sel1)
data_source <- data_sel1[, c(1:7, 9, 12:14)] #select only column 2-9
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
write.csv(write.csv(corr_r_round,"corr_matrix_all_varaibles_LD_train.csv"))

################################################################################
# LM Modeling with train and test set
################################################################################
#conducted a final LM model to get all the stuff using the training set only.

final <- lm(formula = I_Zscore ~ ., data = train)

#summary of model with additional statistics
summary (final)

#include only those that are significant predictors and no suppresion

final2 <- lm(formula = I_Zscore ~ num_rhymes_full_elp
             +num_rhymes_1000_coca
             +num_rhymes_2500_coca
             +num_rhymes_5000_coca
             +num_rhymes_10000_coca
             +Ortho_N
             +OLDF
             +PLD,
             data = train)

summary (final2)

#remove those that are no significant now or have suppression
final3 <- lm(formula = I_Zscore ~ num_rhymes_full_elp
             +num_rhymes_2500_coca
             +num_rhymes_5000_coca
             +num_rhymes_10000_coca
             +OLDF
             +PLD,
             data = train)

summary (final3)

#this is the r value
.613*.613 

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
r_test^2 #### 0.3548191
