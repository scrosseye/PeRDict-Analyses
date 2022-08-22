## This document performs a linear model to predict readability using output from 
# the PeRDICT database as well neighborhood features reported by the Engligh Lexicon
# Project

################################################################################
# Load Libraries
################################################################################

# If you don't have these libraries installed, you will have to install them
library(caret)#machine learning
library(dplyr)#data analytics
library(psych)#descriptives
library(tidyverse)#data analytics
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
data <- read.csv(file = 'num_perfect_rhymes_lemma_token_w_nlp.csv')


# examine data
str(data)

#select data you want
data_sel <- data[, c(19, 29:38, 47:60)] #select only columns that are needed
str(data_sel)

#describe the data
psych::describe(data_sel) #seems okay


# scale linguistic data 
data_sel1 <- data_sel %>%
  mutate_at(c(1:25), funs(c(scale(.))))

# ensure data is mean centered
psych::describe(data_sel1)


################################################################################
#break into training and test
################################################################################

#select only the variables that are not multicollinear including the DV. 
#this is from a previous analysis that pruned variables based on multicollinearity

data_source <- data_sel1[, c(1, 3, 5:11, 17, 19, 22)] #select only column 2-9
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
write.csv(write.csv(corr_r_round,"corr_matrix_all_varaibles_bt_ease_train.csv"))

################################################################################
# LM Modeling with train and test set
################################################################################
#conducted a final LM model to get all the stuff using the training set only.

final <- lm(formula = BT_easiness ~ ., data = train)

#summary of model with additional statistics
summary (final)

#include only those that are significant predictors

final2 <- lm(formula = BT_easiness ~ rhymes_1000_coca_final_lemma+rhymes_5000_coca_final_lemma 
             +rhymes_10000_coca_final_tokens+Freq_N_PH_CW+OLD_CW, 
             data = train)

summary (final2)



#this is the final model with variables that are predictive and not suppressed
final3 <- lm(formula = BT_easiness ~ rhymes_1000_coca_final_lemma+
               Freq_N_PH_CW+OLD_CW, data = train)

summary (final3)

#reverse engineer the R value
.509*.509

#calculate variable importance metrics. This will likely require a high-performance machine
metrics_w_types_all <- calc.relimp(final3)#, type = c("lmg", "pmvd", "first", "last", "betasq", "pratt"))
metrics_w_types_all

calc.relimp(final3,type=c("lmg","last","first","pratt"),rela=TRUE)
#this reports percentage of importance by variable that adds up to 100

car::vif(final3) #VIF values for the regression to ensure no problems with multi-collinearity

#normality
shapiro.test(residuals(final3)) #residual plot to check for normality

#Homoscedasticity check
plot(final3, which = 1) #residual plot


########predict on training set####################

pred_model <- predict(final3, newdata = test)

#get correlation and r squared

r_test <- cor(test$BT_easiness, pred_model)
r_test
r_test^2 #### 0.2612385
