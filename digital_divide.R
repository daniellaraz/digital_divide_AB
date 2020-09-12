# library imports
library(ggplot2)
library(reshape2)
library(dplyr)

# loading data
#ab_df5 <- read_dta("Documents/ABV_Crossectional_Data_Release_ENG.dta")
#ab_df5 <- read_dta("ABV_Crossectional_Data_Release_ENG.dta")
#ab_df4 <- read_dta("ABIV_English.dta")
ab_df5 = ABV_Crossectional_Data_Release_ENG

# estimations for country populations
country_pops = c(41657488,99413317,40194216,10458413,2916467,6100075,6754507,
                 34314130,2798494,43120843,11516189,28667230)
country_weights = data.frame(country=as.factor(c(1,5,7,8,9,10,11,13,15,19,21,22)),
                             norm_weight = country_pops/sum(country_pops))

# removing missing and don't know from variable of interest: q409, internet usage
ab_df5 = subset(ab_df5,Q409 != 98 & Q409 != 99)
ab_df4 = subset(ab_df4,q409 != 98 & q409 != 99)

# creating variable of user_internet, 0 = offline, 1 = any amount of internet usage
# For purposes of this analysis, respondents are identified as either being “online” or “offline”. The former includes those respondents who say they use the internet at any frequency - from ---- to ----. The latter includes only those respondents who say they “never” use the Internet. Those who did not respond or for whom responses are missing (n = ) are dropped from the analysis. Thus, all respondents fall into one of these two categories.. on computers, smartphones, or tablets
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

# simple proportions:
# urban vs. rural 
urb_count = aggregate(ab_df5$use_internet_ab5,by=list(ab_df5$country,ab_df5$urban_ab5),mean)[12:22,3]
rural_count = aggregate(ab_df5$use_internet_ab5,by=list(ab_df5$country,ab_df5$rural_ab5),mean)[13:23,3]
urb_rural_diff = urb_count - rural_count
urb_rural_diff

# unweighted gender differences (men versus women)
gend_m = subset(ab_df5,gender_male_ab5==1)
gend_w = subset(ab_df5,gender_male_ab5==0)
gendm_count = aggregate(gend_m$use_internet_ab5,by=list(gend_m$country),mean)
gendw_count = aggregate(gend_w$use_internet_ab5,by=list(gend_w$country),mean)
gend_diff = gendm_count$x - gendw_count$x
gend_diff = as.data.frame(cbind(as.numeric(as.character(gendm_count$Group.1)),gend_diff))

# weighted gender differences (men versus women)
gend_m_num = aggregate(gend_m$use_internet_ab5*gend_m$wt,by=list(gend_m$country),sum)
gend_m_den = aggregate(gend_m$wt,by=list(gend_m$country),sum)
gendm_weighted = gend_m_num$x/gend_m_den[,2]

gend_w_num = aggregate(gend_w$use_internet_ab5*gend_w$wt,by=list(gend_w$country),sum)
gend_w_den = aggregate(gend_w$wt,by=list(gend_w$country),sum)
gendw_weighted = gend_w_num$x/gend_w_den[,2]

gend_weighted_diff = gendm_weighted - gendw_weighted

# weighted age differences ([18-29] = young adults, 60+ = older)

age18 = subset(ab_df5,age_ab5 < 30)
age60 = subset(ab_df5,age_ab5 >= 60)

age18_num = aggregate(age18$use_internet_ab5*age18$wt,by=list(age18$country),sum,na.rm=TRUE)
age60_num = aggregate(age60$use_internet_ab5*age60$wt,by=list(age60$country),sum,na.rm=TRUE)

age18_den = aggregate(age18$wt,by=list(age18$country),sum,na.rm=TRUE)
age60_den = aggregate(age60$wt,by=list(age60$country),sum,na.rm=TRUE)

age18_count = age18_num$x/age18_den[,2]
age60_count = age60_num$x/age60_den[,2]

age_diff = age18_count - age60_count

# unweighted:
#age_diff = age18_count$x - age60_count$x

age_diff = cbind(as.numeric(as.character(age18_count$Group.1)),age_diff)


# religious vs. not religious
religious_count = aggregate(ab_df5$use_internet_ab5,by=list(ab_df5$country,ab_df5$religious_ab5),mean)


ab_df5$country = as.character(ab_df5$country)
country_weights$country = as.character(country_weights$country)

ab_df_test = left_join(ab_df5,country_weights)

# lin regression
model_linreg = glm(use_internet_ab5 ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
                       income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
                       secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
                       not_religious_ab5 + missing_religious_ab5, data=ab_df_test,weights = wt*norm_weight)

# logistic regression
model_binomial = glm(use_internet_ab5 ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
      income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
      secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
      not_religious_ab5 + missing_religious_ab5, data=ab_df_test,weights = wt*norm_weight,family = binomial)

summary(model_binomial)

# computation for change in odds calculation: 
# taking log odds converting to odds scale [exp(coefs_model)] and making a percentage [-1]
coefs_model = coef(model_binomial)
odds_change = exp(coefs_model)-1

### age is put in the regression in units of years, want the coeffs in units of ten-years
age_change = exp(coefs_model[14]*10) - 1








### optional further work
# ordinal logistic regression
# mod_ordinal = clm(as.factor(Q409) ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
#                           income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
#                           secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
#                           not_religious_ab5 + missing_religious_ab5, data=ab_df5,weights = wt,family = binomial)

#ab_df5_users = subset(ab_df5,Q409 !=6)

#mod_ordinal_users = clm(as.factor(Q409) ~ country + gender_male_ab5 + age_ab5 + age_missing_ab5 + income_above_ab5 + 
#                    income_missing_ab5 + rural_ab5 + refugee_ab5 + age_ab5:rural_ab5 + income_above_ab5:rural_ab5 + 
#                    secondary_ed_ab5 + higher_ed_ab5 + missing_education_ab5 + somewhat_religious_ab5 + 
#                    not_religious_ab5 + missing_religious_ab5, data=ab_df5_users,weights = wt,family = binomial)