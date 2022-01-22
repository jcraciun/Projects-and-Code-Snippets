###########################################################################
###                                                                     ###
###                 PART 1: LOADING THE DATA SET                        ###
###                                                                     ###
###########################################################################


airbnb_listings = read.csv("airbnb_listings.csv")
head(airbnb_listings) 


###########################################################################
###                                                                     ###
###   QUESTION 1: PERCENTAGE OF MODERATE CANCELLATION POLICY LISTINGS   ###
###                                                                     ###
###########################################################################

library("rlang")
library(mosaic)

tally(airbnb_listings$cancellation_policy, format = 'percent')

# 28.009 percent of listings are categorized as moderate


###########################################################################
###                                                                     ###
###         QUESTION 2: SECOND MOST COMMON NAME IN THE DATASET          ###
###                                                                     ###
###########################################################################

sort(table(airbnb_listings$host_name), TRUE)

# the second most common name is "Jason" with 25 occurrences. 


###########################################################################
###                                                                     ###
###       QUESTION 3: UNIQUE HOSTS WITH SECOND MOST COMMON NAME         ###
###                                                                     ###
###########################################################################

library("dplyr")

non_duplicate_host = airbnb_listings %>% distinct(host_id, .keep_all = TRUE)
count(non_duplicate_host, host_name == "Jason")

# There are only 7 non-duplicate instances of the name "Jason"


###########################################################################
###                                                                     ###
###                   QUESTION 4: HYPOTHESIS TEST                       ###
###                                                                     ###
###########################################################################

# Isolating the dataset and organizing the variables to townhouses and lofts 

airbnb_lofts = airbnb_listings[airbnb_listings$property_type == "Loft",]
airbnb_townhouses = airbnb_listings[airbnb_listings$property_type == "Townhouse",]

# determining the number of rows to find the sample size for each set 
# (n = sample size)

n_lofts = nrow(airbnb_lofts)
n_townhouses = nrow(airbnb_townhouse)

# finding the sample mean price for both groups 

xbar_townhouses = mean(airbnb_townhouses$price)
xbar_lofts = mean(airbnb_lofts$price)

# Sample mean price for the townhouses is $187.2353 while sample mean for the 
# lofts is about $228.3846

# Finding the standard error of the townhouse sample mean 

s_townhouses = sd(airbnb_townhouses$price) # sample standard deviation for the townhouses (about 181.2614)
SE_townhouses = s_townhouses/sqrt(n_townhouses) # standard error of the townhouse mean (about 43.96234)

# calculating the t-test statistic in reference to the loft mean 

t_stat = (xbar_townhouses - xbar_lofts)/SE_townhouses

# finding the p value 
p_val = 2*(1-pt(t_stat, df = n_townhouses - 1))
p_val <.01 # returns FALSE

# The p-value is greater than the significance level. Therefore, we don't have strong evidence to
# reject the null hypothesis (that there is a statistically significant difference between the average
# price of a loft and the average price of a townhouse. 

