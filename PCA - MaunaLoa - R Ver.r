# Part 1: Exploratory data analysis

## Read file
maunaloa <- read.csv("MaunaLoa.csv")

## Display the first few rows
head(maunaloa)

## Check data types
str(maunaloa)

## Check missing values (apply any function for each row)
maunaloa[apply(is.na(maunaloa), 1, any), ]

## There are 21 missing values for CFC11 which belong to entire year 2006, 7 months of 2007 and 2 months of 2008, 
## which could be due to the instrumental and operational disruptions. 
## Otherwise, there are sufficient observations for other gases C0, C02, Ch4 and N20 

## Summarize data
summary(maunaloa)

## Histogram of gas concentration measurements
par(mfrow = c(2,3))
hist(maunaloa$C0, breaks = 10, main = 'Histogram of CO', xlab = 'CO', col = '#AAD7D9', border = 'black')
rug(maunaloa$C0)

hist(maunaloa$C02, breaks = 10, main = 'Histogram of CO2', xlab = 'CO2',
 col = '#AAD7D9', border = 'black')
rug(maunaloa$C02)

hist(maunaloa$CH4, breaks = 10, main = 'Histogram of CH4', xlab = 'CH4',
 col = '#AAD7D9', border = 'black')
rug(maunaloa$CH4)

hist(maunaloa$N20, breaks = 10, main = 'Histogram of N20', 
 xlab = 'N20', col = '#AAD7D9', border = 'black')
rug(maunaloa$N20)

hist(maunaloa$CFC11, breaks = 10, main = 'Histogram of CFC11', xlab = 'CFC11', 
    col = '#AAD7D9', border = 'black')
rug(maunaloa$CFC11)

## Boxplots
par(mfrow = c(1,1))
boxplot(maunaloa[,3:7])

## Some right skewness and bimodal distribution can be observed for all the gas variables. Besides, 5 variables have 
## varying values where CH4 has the highest median of 1812.63 and CO values center around the lowest median of 89.95.
## There are no potential outliers spotted from the box plots.

## Matrix of Plots between variables
library(psych)

pairs.panels(maunaloa[, 3:7], method = 'pearson', density = TRUE, ellipse = TRUE,
            hist.col = "#00AFBB", cex.main = 1,
            main = 'Matrix of Plots between variables')

## All the gas concentration measurements seem to have some relationship with each other. In particular, there exist some strong positive 
## correlations in the dataset, between CO2 and CH4 (0.91), C02 and N20 (0.98), C02 and FC11 (-0.98), and so on, which would imply that 
## the increase in one variable may be associated with an increase or decrease in another variable. 
## On the other hand, C0 does not seem to strongly correlate with any other gases.

## Pre-processing data

## Upon review, the rows containing null values of CFC 11 will be removed since there exists uncertainty regarding changes in CFC11 concentrations 
## in those months and years. The missing data, potentially affected by operational disruptions at the Mauna Loa Observatory, contributes to this 
## uncertainty. The natural variations in atmospheric trace gases influenced by environmental and climatic factors, make it challenging to 
## interpolate or estimate these missing values.
## Also, to maintain the consistency of the analysis within the dataset where a year fully include 12 months, all the observations of year 2006, 
## 2007 and 2008 will be removed. Following that, the results will be interpreted in the absence of these 3 years.

## Remove 2006, 2007 and 2008 observations and take only numeric variables

maunaloa_cleaned <- subset(maunaloa, !(Year %in% c(2006, 2007, 2008)))
maunaloa_cleaned <- maunaloa_cleaned[,-c(1,2)]
maunaloa_cleaned

## Re-visualize correlation matrix
pairs.panels(maunaloa_cleaned, method = 'pearson', density = TRUE, ellipse = TRUE,
            hist.col = "#00AFBB", cex.main = 1,
            main = 'Matrix of Plots between variables')

## After removing null values, there are hardly any changes in the correlation among gas concentration variables.

# Part 2: Principal Component Analysis

## Scaling data
maunaloa_scaled <- scale(maunaloa_cleaned)

## Perform PCA
pr.out <- prcomp(maunaloa_scaled)
pr.out

summary(pr.out)

## Scree Plot to assess the Importance of Components

library(factoextra)

fviz_screeplot(pr.out, addlabels = TRUE, barcolor = "black", barfill= '#76ABAE', main = 'Scree Plot')

## The first principal component PC1 accounts for a substantial proportion of variance (78%), and PC2 takes up about 20% of it. Together, 
## these components account for 97.9% of the total variance in the dataset. Therefore, reducing data dimension to 2 principal components is 
## sufficient since they preserve the majority of the information contained in the original features and keeping more components may not add significant information.

## PCA Biplot
fviz_pca_biplot(pr.out, axes = c(1,2), repel = TRUE, col.var = 'red') +
    ggtitle('PCA Biplot')

## The PCA Biplot can visualize the relationship between the original features and new variables (principal components). 
## In the biplot, the length of the four vectors N20, CO2, CH4, and CFC11 are quite similar but N20 is the longest one, 
## which implies that it explains the majority of variance in PC1. These four variables contribute most to PC1, 
## while CO is the prominent contributor to PC2.

## CO2, CH4, and N20 have positive loadings on PC1. This may suggest that an observation with a high score 
## on PC1 is likely to have high concentration measurements of all these 3 gases. By contrast, CFC11 has a negative 
## loading on PC1 meaning that samples with high scores on PC1 tend to have low concentration of CFC11 and vice 
## versa. Since CO2, CH4, and N20 are greenhouse gases, PC1 may represent a general trend of greenhouse 
## gas emissions given the removal of CFCs in favor of substances with less ozone-depleting potential, where the gas 
## measurements tend to vary across samples and each sample corresponds to a month in each year. On the other hand, CO 
## does not make a significant impact on PC1 but still has a slight inverse relationship with other greenhouse gases.

## In terms of PC2, CO stands out with a very strong loading on PC2, suggesting that PC2 primarily captures the variation 
## in CO levels across the dataset. A high score on PC2 corresponds to a high CO concentration measure and vice versa. 
## While CO is also a greenhouse gas, PC2 may reflect another dimension of the dataset that is mainly related to 
## industrial processes, vehicle emissions, or other combustion-related factors that are different from other greenhouse gases