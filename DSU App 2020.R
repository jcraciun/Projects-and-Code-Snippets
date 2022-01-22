### LOADING THE DATA ############################################################

basketball <- read.csv("techapp.csv")
head(basketball)

### PROBLEM 1: FINDING THE 75TH PERCENTILE ######################################

winning_percentage_50 <-subset(basketball, Overall_W.L.>=.5) 
# isolating the dataset to just NCAA teams with a winning percentage of 50% or more # 

head(winning_percentage_50) 

quantile(winning_percentage_50$Totals_3P., probs = .75)
# determining the 75th percentile #

# NOTE: Periods replace percents in the name of the columns in R. Eg: Totals_3P. = Totals_3P% #


### PROBLEM 2: PERCENTAGE OF TEAMS GREATER THAN FIELD GOAL PERCENTAGE ##########

mean(basketball$Totals_FG.) # attempt to find the NCAA average. Returns NA value 

basketball_no_ommisions <- basketball[complete.cases(basketball),] 
# removing all null values #
 
mean(basketball_no_ommisions$Totals_FG.) # computing new NCAA average

Pac_12 <- basketball_no_ommisions[basketball_no_ommisions$Pac12=="True",] # isolating the Pac-12 conference members
mean(Pac_12$Totals_FG. > 0.4404841) # computing the proportion greater than the NCAA average above

## PROBLEM 3: LINEAR REGRESSION #################################################

linear_model <- lm(basketball$Overall_W.L. ~ basketball$Totals_FG.)
summary(linear_model) # used the multiple R-squared value


## PROBLEM 4: HYPOTHESIS TEST ###################################################

xbar <- mean(basketball_no_ommisions$Totals_FT.) #sample mean
s <- sd(basketball_no_ommisions$Totals_FT.) #sample standard deviation 
n <- nrow(basketball_no_ommisions) #sample size
SE <- s/sqrt(n) #standard error of the sample mean

t_stat <- (xbar-.7)/SE #t-test statistic
p_val <- 2*(1-pt(t_stat, df=n-1)) #p-value
p_val < .01 
p_val < .05 #used various significance levels to confirm result

# both p values are less than the significance level, therefore, we have strong evidence to reject the null hypothesis that the free throw percentage of all NCAA teams is .7


