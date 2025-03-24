# Lab 7 Assignment: Difference in Means and ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions bellow

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`


# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:
# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results

# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)

# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.

# 2. [30 points] Run a T-test and an ANOVA test in your data.


# Bonus: [30 points] Provide an HTML file with your answers using R-Markdown.



library(data.table)
library(foreign)
library(ggplot2)


#Q1.1

HTS <- data.table(read.spss("datasets/HTS.household.10regions.sav", to.data.frame = TRUE))

#Q2.1
HTS[,.N, by = sf]

names(HTS)


p1 <- ggplot(data=HTS, aes(x=hhincome)) +
  geom_histogram(bins = 30) +
  facet_grid(sf ~ .)

ggsave(filename = "household_income.pdf", plot = p1, width = 8, height = 6)


two_tailed_t_test<-HTS[,t.test(formula = hhincome ~ sf)] 
two_tailed_t_test

curve(dt(x, df = 5934.7), from = -10, to = 10)
abline(h=0,col='blue')
points(x=two_tailed_t_test$statistic,y=0,col='red')
upper975 <- qt(p = .975, df = 5934.7)
abline(v = upper975,y=0,col='red')
lower025 <- qt(p = .025, df = 5934.7)
abline(v = lower025,y=0,col='red')

#Households in single family homes have a much higher average income compared to households in other types of housing.




#Q 2.3
HTS_SA <- HTS[region == "San Antonio, TX"]

names(HTS_SA)


jobpop_median <- median(HTS_SA$jobpop, na.rm = TRUE)
HTS_SA[, jobpop_group := ifelse(jobpop >= jobpop_median, "Above Median", "Below Median")]
t_test_result <- t.test(lnvmt ~ jobpop_group, data = HTS_SA)
print(t_test_result)


#Q 2.3

bartlett_result <- bartlett.test(lnvmt ~ income_cat, data = HTS_SA)
print(bartlett_result)

result_anova <- aov(lnvmt ~ income_cat, data = HTS_SA)
summary(result_anova)

tukey_result <- TukeyHSD(result_anova)
print(tukey_result)



