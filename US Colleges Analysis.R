library(tidyverse)
install.packages('ISLR2')
library(ISLR2)

#College Dataset
#Main Questions:
#Q1) Do Colleges with larger full-time enrollments have lower graduation rates?
#Q2) Is this different for public/private institutions?
#Q3) Is it different for more selective schools?

?College
glimpse(College)

#Exploratory Visuals before digging into data
ggplot(College,aes(x= Grad.Rate))+
  geom_histogram() # based on results we notice odd outliers (any college +100% acceptance)
#Overall the histogram is nominally skewed

odd_finding <- filter(College,Grad.Rate>=100)
View(odd_finding)

#Plotting graduation rate based on size of college (# of full time undergrads)
ggplot(College, aes(x=F.Undergrad,
                    y=Grad.Rate))+
  geom_point()

#Due to the data skewed towards smaller populated schools, it will be better to log 10
#the data for school populations to better see results and potentially use regression
ggplot(College, aes(x=log10(F.Undergrad),
                    y=Grad.Rate))+
  geom_point()

#Creating a refined college variable
college_refined <- College %>% 
  mutate(log_fulltime = log10(F.Undergrad)) %>% 
  select(Grad.Rate,
         log_fulltime,
         Private,
         Top25perc)
View(college_refined)

#Q1) Is there a relation between School Size and Lower Grad Rate?
#Using Linear Model to test if there is a relation between School Size and Grad Rate
ggplot(College,aes(x=log10(F.Undergrad),
                   y=Grad.Rate))+
  geom_point()+
  geom_smooth(method="lm")
#Due to the flatness of the line (it being horizontal) we can see there isn't 
#any strong relation between school size and graduation rate
#we can further support this by looking at the P-Value Below
model_undergrad <- lm(Grad.Rate~log_fulltime,
                      data=college_refined)
summary(model_undergrad)
#We get a P-Value of 0.9499 for this model which means we cannot reject the null
#There is not a statistically significant evidence to show a relation between
#school size and graduation rates

plot(model_undergrad)

#Q2) Private Schools vs Graduation Rates
#plotting private vs public school size vs graduation rates
ggplot(College,aes(x=log10(F.Undergrad),
                   y=Grad.Rate,
                   color=Private))+
  geom_point()+
  geom_smooth(method='lm',
              se=FALSE) + #removing standard error line for visual purposes
  scale_color_brewer(palette = "Dark2")

#We see a relation may exist for both private and public schools as when population
#grows both tend to have a positive slope in graduation rate

#We can look into this further with a Model (modelling Graduation rate based on Private 
#schools and school size)
model_private <- lm(Grad.Rate ~ Private + log_fulltime,
                    data=college_refined)
summary(model_private)

#We see that the P-Value for both Private and Non-Private schools related to size of 
#the campus is very small (<2e-16) so we can deny the null and state there is a relation
#between Private/Public schools and Graduation rates based on size of schools
#The multiple R Squared also shows that 20.16% of variability in graduation rates 
#can be explained by Public/Private schools and the log of school sizes

#Nested F Test to verify if Private schools have a impact on graduation rates 
model_private_interactive <- lm(Grad.Rate ~ Private * log_fulltime,
                                data = college_refined)
anova(model_private_interactive)
#We see here that the interactive variables (private * school size) when isolated together
#have a statistically significant P-Value allowing us to reject the null
#and state that when addressing for Private schools, the more students in a school
#the higher the graduation rate seems to be

#Question 3: What about the Top25%?
model_top <- lm(Grad.Rate ~ Private + 
                  log_fulltime+
                  Top25perc,
                data = college_refined)
summary(model_top)
#Here we see each of the variables is statistically significant and this model
#is the greatest at explaining the variability in our graduation rates as the
# Multiple R-Squared is 32.84% which means this model explains about 33% of 
#the variability. 