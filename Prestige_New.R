# Load the data set
pres_new<-read.csv("Prestige_New.csv")

# View the first few rows
head(pres_new)

# Calculate minimum income of the incumbents
min_income <- min(pres_new$income)
#Print Result
print(paste("Minimum Income of the incumbents:",min_income))

#Calculate maximum income of the incumbents
max_income <- max(pres_new$income)
#Print Result
print(paste("Maximum Income of the incumbents:",max_income))

#Calculate mean income of the incumbents
mean_income <- mean(pres_new$income)
#Print Result
print(paste("Mean Income of the incumbents:", mean_income))

#Calculate median income of the incumbents
median_income <- median(pres_new$income)
#Print Result
print(paste("Median Income of the incumbents:", median_income))

# Define a function to find the mode
get_mode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

#Calculate mode income of the incumbents
mode_income <- get_mode(pres_new$income)
#Print Result
print(paste("Mode Income of the incumbents:",mode_income))


# Summary statistics for prestige, education, and income
summary_statistics <- summary(pres_new[c("prestige", "education", "income")])
#Print Result
print(summary_statistics )


#finding central tendency analysis for prestige variable

mean_prestige <- mean(pres_new$prestige)
print(mean_prestige)

sd_prestige <- sd(pres_new$prestige)
print(sd_prestige)

#bell curve for prestige variable graphical analysis

curve(dnorm(x,mean_prestige,sd_prestige),
      col = "red",xlim = c(min(pres_new$prestige),max(pres_new$prestige)),
      main = "Bell Curve for prestige")



#finding central tendency analysis for income variable

mean_income <- mean(pres_new$income)
print(mean_income)

sd_income <- sd(pres_new$income)
print(sd_income)

#bell curve for income variable graphical analysis

curve(dnorm(x,mean_income,sd_income),
      col = "blue",xlim = c(min(pres_new$income),max(pres_new$income)),
      main = "Bell Curve for income")



#finding central tendency analysis for education variable

mean_education <- mean(pres_new$education)
print(mean_education)

sd_education <- sd(pres_new$education)
print(sd_education)

#bell curve for education variable graphical analysis

curve(dnorm(x,mean_education,sd_education),
      col = "purple",xlim = c(min(pres_new$education),max(pres_new$education)),
      main = "Bell Curve for education")


#Statistical Analysis

# Create subsets of the prestige scores
Occupation_type_Blue_Collar <- pres_new$prestige[pres_new$type == "bc"]
Occupation_type_Professional_Managerial_Technical <- pres_new$prestige[pres_new$type == "prof"]
Occupation_type_White_Collar <- pres_new$prestige[pres_new$type == "wc"]
Occupation_type_Not_Available <- pres_new$prestige[is.na(pres_new$type)]

# Perform pairwise t-tests
t_test_bc_prof <- t.test(Occupation_type_Blue_Collar, Occupation_type_Professional_Managerial_Technical)
t_test_bc_wc <- t.test(Occupation_type_Blue_Collar, Occupation_type_White_Collar)
t_test_bc_na <- t.test(Occupation_type_Blue_Collar, Occupation_type_Not_Available)
t_test_prof_wc <- t.test(Occupation_type_Professional_Managerial_Technical, Occupation_type_White_Collar)
t_test_prof_na <- t.test(Occupation_type_Professional_Managerial_Technical, Occupation_type_Not_Available)
t_test_wc_na <- t.test(Occupation_type_White_Collar, Occupation_type_Not_Available)


# Display results
t_test_bc_prof
t_test_bc_wc
t_test_bc_na
t_test_prof_wc
t_test_prof_na
t_test_wc_na


# Create a box plot (Manual Grouping)
boxplot(Occupation_type_Blue_Collar, 
        Occupation_type_Professional_Managerial_Technical, 
        Occupation_type_White_Collar, 
        Occupation_type_Not_Available,
        names = c("bc", "prof", "wc", "NA"),
        main = "Box Plot of Prestige Scores by Occupation Type",
        ylab = "Prestige Score",
        xlab = "Occupation Type",
        col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),
        las = 2) 


# Create a box plot (Automatic Grouping)
boxplot(pres_new$prestige~pres_new$type,
        main = "Box Plot of Prestige Scores by Occupation Type",
        ylab = "Prestige Score",
        xlab = "Occupation Type",
        col = c("lightblue", "lightgreen", "lightcoral"),
        las = 2) 



# Perform one-way ANOVA
anova_result <- aov(prestige ~ type, data = pres_new)
print(anova_result)

# Summary of the ANOVA test
summary(anova_result)


# Create a box plot for the ANOVA test
boxplot(prestige ~ type, data = pres_new,
        main = "Prestige Scores by Occupation Type",
        xlab = "Occupation Type", ylab = "Prestige Score",
        col = c("lightblue", "lightgreen", "lightpink"))




# Test normality for prestige
prestige_Normality <- shapiro.test(pres_new$prestige)
print(prestige_Normality)

# Test normality for education
education_Normality <- shapiro.test(pres_new$education)
print(education_Normality)


# Find the Correlation between the prestige and education variables
#using Spearman (since normality is violated)
correlation <- cor(pres_new$prestige, pres_new$education, method = "spearman")
print(correlation)


# Scatter plot of Prestige vs Education
plot(pres_new$prestige, pres_new$education, 
     xlab = "Prestige", ylab = "Education", 
     main = "Scatterplot of Prestige vs Education",
     pch = 19, col = "blue")





# Test normality for prestige
prestige_Normality <- shapiro.test(pres_new$prestige)
print(prestige_Normality)

#Test normality for income
income_Normality <- shapiro.test(pres_new$income)
print(income_Normality)


#Find the Correlation between the prestige and income variables
correlation <- cor(pres_new$prestige,pres_new$income,method = "pearson")
print(correlation)

# Scatter plot of Prestige vs Income
plot(pres_new$prestige,pres_new$income, xlab = "Prestige",
     ylab = "Income", main = "Scatterplot of Prestige vs Income",
     pch = 19, col = "purple")


# Fit a linear model and add the regression line to the plot
abline(lm(income ~ prestige, data = pres_new), col = "red") 

