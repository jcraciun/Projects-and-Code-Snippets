data <- read.csv("CQL.csv")
head(data)
str(data)

rownames(data) <- data$UA_Name

# PERCENTAGE REMOVAL ###########################################################

data$Diversity...Non.White. <- as.numeric(sub("%", "", data$Diversity...Non.White.))
data$Diversity..Standard.deviation.of.Population.Groups. <- as.numeric(sub("%", "", data$Diversity..Standard.deviation.of.Population.Groups.))
data$Diversity...Non.White. <- as.numeric(sub("%", "", data$Diversity...Non.White.))
data$Growth.2010.2020 <- as.numeric(sub("%", "", data$Growth.2010.2020))

head(data)

# REMOVE CHARACTER VECTORS #####################################################

data$Adjusted.Median.Household.Income <- as.numeric(gsub(",", "", data$Adjusted.Median.Household.Income))
data$X2020_pop <- as.numeric(gsub(",", "", data$X2020_pop))
data$X2010_pop <- as.numeric(gsub(",", "", data$X2010_pop))

# REMOVE NON-IMPORTANT COLUMNS #################################################

subset_data <- subset(data, select = -c(UA_Continent, Economy, UA_Name, UA_Country, Business.Freedom, Diversity.Rank, Growth.Rank, MHI.Rank))
head(subset_data)

# ISOLATE COLUMNS FOR THE JOB-SEEKER ###########################################

jobseeker_data <- subset(subset_data, select = c(Adjusted.Median.Household.Income, Fortune.500.Companies.per.capita, Cost.of.Living,
                                                 Unemployment.Rate, Commute, Taxation, ))

# CORRELATION MATRIX ###########################################################

correlation <- cor(subset_data[,sapply(subset_data, is.numeric)], use = "complete.obs", method = "pearson")

library(corrplot)
library(RColorBrewer)

corrplot(correlation, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


# UPDATED DATASET - POST CLEANING ##############################################

clean_data <- read.csv("FINAL_DATASET.csv")
head(clean_data)

rownames(clean_data) <- clean_data$UA_Name

# CORRELATION CHART ############################################################

correlation_clean <- cor(clean_data[,sapply(clean_data, is.numeric)], use = "complete.obs", method = "pearson")

library(lares)

corr_cross(clean_data, 
           max_pvalue = .05,
           top = 30)

# PCA ##########################################################################

library(devtools)
install_github("vqv/ggbiplot")
library(plyr)
library(dplyr)
library(ggbiplot)


clean_data.pca <- prcomp(clean_data[-(1)], center = TRUE, scale. = TRUE)

str(clean_data.pca)

ggbiplot(clean_data.pca, labels = rownames(clean_data))

# ISOLATE AND RENAME VARIABLES FOR BETTER VIZUALIZATION # 

PCA <- read.csv("Secondary_Analysis.csv")
rownames(PCA) <- PCA$UA_Name

PCA <- subset(PCA, select = -c(Econper, Population_2020, Population_2010, Fortune_500))
head(PCA)

PCA.pca <- prcomp(PCA[-(1)], center = TRUE, scale. = TRUE)
ggbiplot(PCA.pca, labels = rownames(clean_data))

# POSSIBLE QUESTION FOR GROUP: CAN INCLUDE CIRCLES TO GROUP FACTORS THAT ARE IMPORTANT
# FOR CERTAIN GROUPS OF PEOPLE: E.G. VENTURE CAPITAL AND FORTUNE 500 WOULD BE JOB/FINANCE RELATED

# GGPLOT VISUALIZATIONS ########################################################

library(tidyverse)

ggplot(data = clean_data) + geom_point(mapping = aes(x = X, y = `Cost.of.Living`)) 

# WORKSPACE ####################################################################


