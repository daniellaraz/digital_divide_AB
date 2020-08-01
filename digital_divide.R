# loading data
ab_df5 <- read_dta("Documents/ABV_Crossectional_Data_Release_ENG.dta")
ab_df4 <- read_dta("Documents/ABIV_English.dta")

country_pops = c(41657488,99413317,40194216,10458413,2916467,6100075,6754507,
                 34314130,2798494,43120843,11516189,28667230)
country_weights = data.frame(country=as.factor(c(1,5,7,8,9,10,11,13,15,19,21,22)),
                             norm_weight = country_pops/sum(country_pops))

# removing missing and don't know from variable of interest: q409, internet usage
ab_df5 = subset(ab_df5,Q409 != 98 & Q409 != 99)
ab_df4 = subset(ab_df4,q409 != 98 & q409 != 99)

# creating variable of user_internet, 0 = offline, 1 = any amount of internet usage
use_internet_ab5 = as.numeric(ab_df5$Q409 %in% c(1:5))
use_internet_ab4 = as.numeric(ab_df4$q409 %in% c(1:5))

# creating variables of interest for quantity of internet usage for ordinal regression
throughout_day = as.numeric(ab_df5$Q409 == 1)
once_aday = as.numeric(ab_df5$Q409 == 2)
several_aweek = as.numeric(ab_df5$Q409 == 3)
once_aweek = as.numeric(ab_df5$Q409 == 4)
less_once_aweek = as.numeric(ab_df5$Q409 == 5)
never = as.numeric(ab_df5$Q409 == 6)

# creating gender variable, 1 = male, 0 = female
gender_male_ab5 = as.numeric(ab_df5$Q1002 == 1)
gender_male_ab4 = as.numeric(ab_df4$q1002 == 1)

# age variable, age is missing if age = 99999
age_ab5 = ab_df5$Q1001
age_missing_ab5 = as.numeric(age_ab5 > 200)
age_ab5[(age_ab5==99999)] = median(age_ab5,na.rm=TRUE)
age_ab4 = ab_df4$q1001
age_missing_ab4 = as.numeric(age_ab4 > 200)
age_ab4[(age_ab4==99999)] = median(age_ab4,na.rm=TRUE)


# factorizing country variables
ab_df5$country = as.factor(ab_df5$country)
ab_df4$country = as.factor(ab_df4$country)

# setting all NAs to 0 
ab_df5[,309:343][is.na(ab_df5[,309:343])] = 0
ab_df4[,263:269][is.na(ab_df4[,263:269])] = 0

# creating new income variable where 2 = income is above said country's median, 1 = income below said country's median
income_combined_ab5 = ab_df5$Q1015A_AL + ab_df5$Q1015A_LEB + ab_df5$Q1015A_JO + ab_df5$Q1015A_PAL + 
  ab_df5$Q1015A_MO + ab_df5$Q1015A_TUN + ab_df5$Q1015A_EG + ab_df5$Q1015A_SUD + ab_df5$Q1015A_YEM + 
  ab_df5$Q1015A_IR + ab_df5$Q1015A_LI + ab_df5$Q1015A_KU

income_combined_ab4 = ab_df4$q1015aalg + ab_df4$q1015aegy + ab_df4$q1015ajor + ab_df4$q1015aleb + 
  ab_df4$q1015amor + ab_df4$q1015apal + ab_df4$q1015atun

income_above_ab5 = as.numeric(income_combined_ab5 == 2)
income_missing_ab5 = as.numeric(income_combined_ab5 > 2 | income_combined_ab5 < 1)

income_above_ab4 = as.numeric(income_combined_ab4 == 2)
income_missing_ab4 = as.numeric(income_combined_ab4 > 2 | income_combined_ab4 < 1)

# creating variable for settlement type, 1=urban, 2=rural, 3=refugee camps (only in Palestine)
urban_ab5 = as.numeric(ab_df5$q13 == 1)
rural_ab5 = as.numeric(ab_df5$q13 == 2)
refugee_ab5 = as.numeric(ab_df5$q13 == 3)

urban_ab4 = as.numeric(ab_df4$q13 == 1)
rural_ab4 = as.numeric(ab_df4$q13 == 2)
refugee_ab4 = as.numeric(ab_df4$q13 == 3)

# creating variable for education levels
# basic education = no formal education, elementary education, & preparatory/basic education
# secondary education = secondary
# higher education = mid-level diploma (professional or technical), BA, MA and higher
basic_ed_ab5 = as.numeric(ab_df5$Q1003 <= 3)
secondary_ed_ab5 = as.numeric(ab_df5$Q1003 == 4)
higher_ed_ab5 = as.numeric(ab_df5$Q1003 >= 5 & ab_df5$Q1003 < 98)
missing_education_ab5 = as.numeric(ab_df5$Q1003 >= 98)

basic_ed_ab4 = as.numeric(ab_df4$q1003 <= 3)
secondary_ed_ab4 = as.numeric(ab_df4$q1003 == 4)
higher_ed_ab4 = as.numeric(ab_df4$q1003 >= 5 & ab_df4$q1003 < 98)
missing_education_ab4 = as.numeric(ab_df4$q1003 >= 98)

# creating religiosity variables
religious_ab5 = as.numeric(ab_df5$Q609 == 1)
somewhat_religious_ab5 = as.numeric(ab_df5$Q609 == 2)
not_religious_ab5 = as.numeric(ab_df5$Q609 == 3)
missing_religious_ab5 = as.numeric(ab_df5$Q609 >= 98)

religious_ab4 = as.numeric(ab_df4$q609 == 1)
somewhat_religious_ab4 = as.numeric(ab_df4$q609 == 2)
not_religious_ab4 = as.numeric(ab_df4$q609 == 3)
missing_religious_ab4 = as.numeric(ab_df4$q609 >= 98)

# finalizing dataframes w/ new variables
ab_df5 = cbind(ab_df5, use_internet_ab5, gender_male_ab5, age_ab5, income_above_ab5,
               income_missing_ab5, urban_ab5, rural_ab5, refugee_ab5, age_missing_ab5,
               basic_ed_ab5, secondary_ed_ab5, higher_ed_ab5, missing_education_ab5,
               religious_ab5,somewhat_religious_ab5,not_religious_ab5,missing_religious_ab5)

ab_df4 = cbind(ab_df4, use_internet_ab4, gender_male_ab4, age_ab4, income_above_ab4,
               income_missing_ab4, urban_ab4, rural_ab4, refugee_ab4, age_missing_ab4,
               basic_ed_ab4, secondary_ed_ab4, higher_ed_ab4, missing_education_ab4)

ab_df5$country = as.character(ab_df5$country)
country_weights$country = as.character(country_weights$country)

ab_df_test = left_join(ab_df5,country_weights)

# logistic regression
model_binomial = glm(use_internet_ab5 ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
      income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
      secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
      not_religious_ab5 + missing_religious_ab5, data=ab_df5,weights = wt,family = binomial)

summary(model_binomial)


# ordinal logistic regression
mod_ordinal = clm(as.factor(Q409) ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
                           income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
                           secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
                           not_religious_ab5 + missing_religious_ab5, data=ab_df5,weights = wt,family = binomial)

ab_df5_users = subset(ab_df5,Q409 !=6)

mod_ordinal_users = clm(as.factor(Q409) ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
                    income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
                    secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
                    not_religious_ab5 + missing_religious_ab5, data=ab_df5_users,weights = wt,family = binomial)

# plots
library(ggplot2)
library(reshape2)
