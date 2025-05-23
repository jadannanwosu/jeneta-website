---
layout: post
title: Pedestrian-Involved Fatal Crashes and Walk Score
description: Examining the relationship between road safety and walkability using R.
---

As someone with a personal interest in urbanism and transportation, Walk Score has been a fun tool with which to compare different U.S. cities. When I discovered that the National Highway Traffic Safety Administration maintains an annually updated database of deadly car crashes, I wanted to see whether or not it is reasonable to draw a relationship between the Walk Score index and fatal collisions involving pedestrians. I also wanted to try my hand at applying skills learned in a semester in DS 1000 at Vanderbilt. I stumbled upon a paper that performs a much more thorough (and probably more useful) examination of the same question, though I don’t believe it uses Walk Score. Refer to the following auto-generated citation if you are interested in that:

  Behram Wali, Lawrence D. Frank,
    Redefining walkability to capture safety: Investing in pedestrian, bike, and street level design features to make it safe to walk and bike,
    Transportation Research Part A: Policy and Practice, Volume 181, 2024, 103968, ISSN 0965-8564, https://doi.org/10.1016/j.tra.2024.103968
    (https://www.sciencedirect.com/science/article/pii/S0965856424000168)

All the files involved in my process are available on the project's dedicated GitHub repo [here](https://github.com/jadannanwosu/Walk-Score-FARS).


# Introduction

## Why, What, and How

Walkability advocates constructed **Walk Score**, available at
walkscore.com, as a measurement of the quality of pedestrian-centered
infrastructure in cities. Walk Score achieves this by integrating data
points like walking times to desired amenities, population and
intersection density, and the length of each city block. While Walk
Score reflects the convenience of the built environment for walkers and
rollers, the same factors are intertwined with implications for road
safety. When urban planners make pedestrian-convenient city streets and
sidewalks, this usually includes features that prevent lethal collisions
with motor vehicles. Additionally, drivers may adjust their behavior to
reflect increased foot traffic. Using data from the 2022 U.S. National
Highway Traffic Safety Administration’s **Fatality Analysis Reporting
System**, the relationship between the conveniences represented by Walk
Score and fatal collisions between pedestrians and drivers can be
measured and understood.

## Fatal Crashes Involving Pedestrians

Here, the `accident` csv file detailing data for all fatal car accidents
in the U.S. was filtered to include only cases appearing within the
`pbtype` file, which focuses on details pertaining specifically to
accidents between pedestrians and automobiles. Both came from the 2022
NTHSA FARS database
(<https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars>).
`pbtype` was first filtered via the `PEDCTYPE` column to remove
incidents not involving true pedestrians (i.e., cyclists).

``` r
require(tidyverse)
```

``` r
ped_crash <- pbtype %>%
  filter(PEDCTYPE!=0)
ped_crash_acc <- accident %>%
  filter(ST_CASE %in% ped_crash$ST_CASE)
```

## Creating a Clean Data Frame Incorporating Walk Score and City Data with FARS data

The “US Cities Urban Connectivity” dataset by VivianEllis on Kaggle
(doi.org/10.34740/kaggle/dsv/7816051) includes information on 100
cities, all of which have populations over 200,000, and their
convenience for publicly accessible transportation option. Its inclusion
of Walk Score and 2022 population numbers are useful for this project.
To merge this data with the FARS data, some cleaning and reformatting
was necessary.

``` r
require(usdata)

# Trimming nonrelevant info + matching records
most_pop_cities <- urban_connectivity %>%
  select(City, State, Population_2022_Census, Place_name, 'Walk Score') %>%
  mutate(City = replace(City, City == "Washington D.C.", "Washington")) %>%
  mutate(City = replace(City, City == "Boise City", "Boise")) %>%
  mutate(City = replace(City, City == "St. Petersburg", "Saint Petersburg")) %>%
    mutate(City = replace(City, City == "Lexington", "Lexington-Fayette")) %>%
  mutate(Place_name = replace(Place_name, Place_name == "St. Petersburg, FL", "Saint Petersburg, FL")) %>%
   mutate(Place_name = replace(Place_name, Place_name == "Irvine, CA", "East Irvine, CA")) %>%
  mutate(Place_name = replace(Place_name, Place_name == "Charlotte/Mecklenburg, NC", "Charlotte, NC"))%>%
  mutate(Place_name = replace(Place_name, Place_name == "Nashville/Davidson, TN", "Nashville, TN")) %>%
   mutate(Place_name = replace(Place_name, Place_name == "Lexington/Fayette, KY", "Lexington-Fayette, KY"))%>%
  arrange(-Population_2022_Census) %>%
  head(102) %>%
  rename(WalkScore = "Walk Score")

# Selecting cities appearing within both datasets

ped_crash_acc <- ped_crash_acc %>%
  filter((state2abbr(STATENAME) %in% most_pop_cities$State & tolower(CITYNAME) %in% tolower(most_pop_cities$City))) %>%
  mutate(STATEAB = state2abbr(STATENAME)) %>%
  mutate(CITYNAME = replace(CITYNAME, CITYNAME == "East Irvine", "Irvine"))
  
# Counting up incidents for each city

ped_crash_count <- ped_crash_acc %>%
  mutate(CITYNAME = str_to_title(CITYNAME)) %>%
  mutate(PLACENAME = paste(CITYNAME, STATEAB, sep = ", ")) %>%
  group_by(STATENAME, CITYNAME, STATEAB, PLACENAME) %>%
  summarize(case_count = n()) 
  
# Final step to consolidate Ellis and NHTSA datasets

crash_and_pop <- left_join(ped_crash_count, most_pop_cities, join_by(PLACENAME == Place_name, STATEAB == State, CITYNAME == City))

# Banishing duplicates and erroneously selected cities
crash_and_pop <- crash_and_pop %>%
  drop_na(WalkScore)%>%
  group_by(CITYNAME) %>% 
  summarise_each(funs(first(.[!is.na(.)])))

# Crashes per capita 
crash_and_pop <- crash_and_pop %>%
  mutate(per_capita_crash = case_count / Population_2022_Census)
```

## Visualizations of Relevant Relationships

These plots explain both the interest in investigating the relationship
between Walk Score and pedestrian-involved fatal collisions, but also
the rationale for accounting for population when comparing cities.

``` r
require(plotly)
# Top Cities for Crashes
crash_and_pop %>%
  arrange(-case_count)%>%
  head(10) %>%
  plot_ly( x = ~reorder(PLACENAME, -case_count), y = ~case_count) %>%
  add_trace(type = 'bar') %>%
  layout(title = "U.S. Cities By Fatal Pedestrian-Involved Car Accidents", xaxis = list(title = "City"), yaxis = list(title = "Number of Crashes"))
```
 <a href="https://jadannanwosu.github.io/Walk-Score-FARS/plot1.html" target="_blank">Click here to see the plot generated by this code.</a>


``` r
# Top Cities for Crashes Per 100,000 people 
crash_and_pop %>%
  arrange(-per_capita_crash)%>%
  head(10) %>%
   plot_ly( x = ~reorder(PLACENAME,  -per_capita_crash), y = ~per_capita_crash*100000) %>%
  add_trace(type = 'bar') %>%
  layout(title = "U.S. Cities By Fatal Pedestrian-Involved Car Accidents, \n Per 100,000 Inhabitants", xaxis = list(title = "City"), yaxis = list(title = "Crashes Per 100,000 (2022 Population)"))
```
<a href="https://jadannanwosu.github.io/Walk-Score-FARS/plot2.html" target="_blank">Click here to see the plot generated by this code.</a>

``` r
# Top Cities for Walk Score
crash_and_pop %>%
  arrange(-WalkScore)%>%
  head(10) %>%
   plot_ly( x = ~reorder(PLACENAME,  -WalkScore), y = ~WalkScore) %>%
  add_trace(type = 'bar') %>%
  layout(title = "10 U.S. Cities With Highest Walk Score", xaxis = list(title = "City"), yaxis = list(title = "Walk Score"))
```
<a href="https://jadannanwosu.github.io/Walk-Score-FARS/plot3.html" target="_blank">Click here to see the plot generated by this code.</a>

``` r
# Bottom Cities for Walk Score
crash_and_pop %>%
  arrange(WalkScore)%>%
  head(10) %>%
  plot_ly( x = ~reorder(PLACENAME,  -WalkScore), y = ~WalkScore) %>%
   add_trace(type = 'bar') %>%
  layout(title = "10 U.S. Cities With Lowest Walk Score \n Out of 100 With Pop. Over 200K", xaxis = list(title = "City"), yaxis = list(title = "Walk Score"))
```
<a href="https://jadannanwosu.github.io/Walk-Score-FARS/plot4.html" target="_blank">Click here to see the plot generated by this code.</a>

``` r
# Population vs Crashes
scatter1 <- lm(case_count ~ log(Population_2022_Census), crash_and_pop)
summary(scatter1)
```

    ## 
    ## Call:
    ## lm(formula = case_count ~ log(Population_2022_Census), data = crash_and_pop)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -82.192  -7.043  -1.402   4.890  80.472 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 -340.308     36.544  -9.312 5.33e-15 ***
    ## log(Population_2022_Census)   27.728      2.788   9.944 2.42e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.52 on 94 degrees of freedom
    ## Multiple R-squared:  0.5127, Adjusted R-squared:  0.5075 
    ## F-statistic: 98.89 on 1 and 94 DF,  p-value: 2.415e-16

``` r
crash_and_pop %>%
  plot_ly(x = ~Population_2022_Census, y = ~case_count, hoverinfo = 'text', text = ~paste(PLACENAME), type = 'scatter', mode = 'markers', name = "City") %>%
  layout(title = "Population vs Fatal Pedestrian-Involved Car Accident Incidence", xaxis = list(title = "Population", type = "log"), yaxis = list(title = "Number of Crashes"))%>%
  add_trace(x = ~Population_2022_Census, y = fitted(scatter1), mode = 'lines', hoverinfo = 'skip', name = 'Regression Line')
```

<a href="https://jadannanwosu.github.io/Walk-Score-FARS/scatter1.html" target="_blank">Click here to see the plot generated by this code.</a>

``` r
# Population vs Walk Score
scatter2 <- lm(WalkScore ~ log(Population_2022_Census), crash_and_pop)
summary(scatter2)
```

    ## 
    ## Call:
    ## lm(formula = WalkScore ~ log(Population_2022_Census), data = crash_and_pop)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.284 -11.885  -3.178  10.166  40.844 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                  -38.209     30.298  -1.261  0.21039   
    ## log(Population_2022_Census)    6.650      2.312   2.877  0.00497 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.35 on 94 degrees of freedom
    ## Multiple R-squared:  0.08092,    Adjusted R-squared:  0.07114 
    ## F-statistic: 8.276 on 1 and 94 DF,  p-value: 0.004972

``` r
crash_and_pop %>%
  plot_ly(x = ~Population_2022_Census, y = ~WalkScore, hoverinfo = 'text', text = ~paste(PLACENAME), type = 'scatter', mode = 'markers', name = "City") %>%
  layout(title = "Population vs Walk Score", xaxis = list(title = "Population", type = "log"), yaxis = list(title = "Walk Score"))%>%
  add_trace(x = ~Population_2022_Census, y = fitted(scatter2), mode = 'lines', hoverinfo = 'skip', name = 'Regression Line')
```

<a href="https://jadannanwosu.github.io/Walk-Score-FARS/scatter2.html" target="_blank">Click here to see the plot generated by this code.</a>

## Regression For Crash Incidence and Walk Score

The method for tackling the guiding question about the relationship
between fatal pedestrian-involved crashes and Walk Score was a simple
linear regression to check for correlation. The linear model underwent
1000-fold cross-validation with a 70-30 split, from which the resulting
average RMSE was about 16.

``` r
reg_pedcrash <- lm(WalkScore ~ per_capita_crash, crash_and_pop)

crash_and_pop %>%
  plot_ly(x = ~per_capita_crash, y = ~WalkScore, hoverinfo = 'text', text = ~paste(PLACENAME), type = 'scatter', mode = 'markers', name = "City") %>%
  layout(title = "Crashes Per Capita vs Walk Score \n From 100 Cities With Pop. Over 200K", xaxis = list(title = "Fatal Crashes Involving Pedestrians Divided by Population"), yaxis = list(title = "Walk Score"))%>%
  add_trace(x = ~per_capita_crash, y = fitted(reg_pedcrash), mode = 'lines', hoverinfo = 'skip', name = 'Regression Line')
```

<a href="https://jadannanwosu.github.io/Walk-Score-FARS/scatter3.html" target="_blank">Click here to see the plot generated by this code.</a>

``` r
# High p-value!

summary(reg_pedcrash)
```

    ## 
    ## Call:
    ## lm(formula = WalkScore ~ per_capita_crash, data = crash_and_pop)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.999 -10.690  -4.484  11.968  38.971 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          51.609      2.974  17.353   <2e-16 ***
    ## per_capita_crash -79136.841  71039.201  -1.114    0.268    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.91 on 94 degrees of freedom
    ## Multiple R-squared:  0.01303,    Adjusted R-squared:  0.00253 
    ## F-statistic: 1.241 on 1 and 94 DF,  p-value: 0.2681

``` r
set.seed(196)
cvResult <- numeric(1000)
for(i in 1:1000) {

  inds <- sample(x = 1:nrow(crash_and_pop), size = round(.7*nrow(crash_and_pop)),  replace = F) 
  train <- crash_and_pop %>% slice(inds) 

  reg_score_CV <- lm(formula = WalkScore ~ per_capita_crash, 
               data = train)
  
  test <- crash_and_pop %>% slice(-inds)
  
  
  test$preds <- predict(reg_score_CV, newdata = test)
  
  e <- test$WalkScore - test$preds
  se <- e^2
  mse <- mean(se)
  rmse <- sqrt(mse)
  cvResult[i] <- rmse
  

}

mean(cvResult)
```

    ## [1] 16.09736

## Evaluating Alternative Subset of Data

The average RMSE after cross-validation was too high to be confident in
pedestrian-involved fatal crashes being a good predictor of walkability
as measured by Walk Score, especially with a relatively high p-value of
0.268. Earlier, all incidents coded in FARS as being pedestrian-involved
were included. To test whether selecting specific scenarios would
eliminate noise and make the relationship clearer, below are the results
when restricted to only the accidents that are most obviously relevant
to said relationship.

``` r
# Selecting common pedestrian situations. Chose "walking in the road against traffic" but not "with traffic" due to the former usually being the legal protocol when there are no sidewalks. Refer to FARS/CRSS Pedestrian Bicyclist Crash Typing Manual for PEDCTYPE code meanings.

ped_crash_small <- pbtype %>%
  filter(PEDCTYPE== 341 | PEDCTYPE==342 | PEDCTYPE==430 | PEDCTYPE==440 | PEDCTYPE==510 | PEDCTYPE == 520 | PEDCTYPE==590 | PEDCTYPE==710 | PEDCTYPE==720 | PEDCTYPE==770 | PEDCTYPE==781 | PEDCTYPE==782 | PEDCTYPE==791 | PEDCTYPE==792 | PEDCTYPE==795 | PEDCTYPE==794 | PEDCTYPE==799 )
ped_crash_acc_small <- accident %>%
  filter(ST_CASE %in% ped_crash_small$ST_CASE)

ped_crash_acc_small <- ped_crash_acc_small %>%
  filter((state2abbr(STATENAME) %in% most_pop_cities$State & tolower(CITYNAME) %in% tolower(most_pop_cities$City))) %>%
  mutate(STATEAB = state2abbr(STATENAME))

ped_crash_count_small <- ped_crash_acc_small %>%
  mutate(CITYNAME = str_to_title(CITYNAME)) %>%
  mutate(PLACENAME = paste(CITYNAME, STATEAB, sep = ", ")) %>%
  group_by(STATENAME, CITYNAME, STATEAB, PLACENAME) %>%
  summarize(case_count = n()) 

crash_and_pop_small <- left_join(ped_crash_count_small, most_pop_cities, join_by(PLACENAME == Place_name, STATEAB == State, CITYNAME == City))

# Banishing duplicates and erroneously selected cities
crash_and_pop_small <- crash_and_pop_small %>%
  drop_na(WalkScore)%>%
  group_by(CITYNAME) %>% 
  summarise_each(funs(first(.[!is.na(.)])))

crash_and_pop_small <- crash_and_pop_small %>%
  mutate(per_capita_crash = case_count / Population_2022_Census)

# MUCH smaller sample size - will this cause problems? Let's see.
```

## Re-running Regression on Smaller Subset

``` r
reg_pedcrash_small <- lm(WalkScore ~ per_capita_crash, crash_and_pop_small)

crash_and_pop_small %>%
  plot_ly(x = ~per_capita_crash, y = ~WalkScore, hoverinfo = 'text', text = ~paste(PLACENAME), type = 'scatter', mode = 'markers', name = "City") %>%
  layout(title = "Crashes Per Capita vs Walk Score \n Subsetted from 100 Cities With Pop. Over 200K", xaxis = list(title = "Fatal Crashes Involving Pedestrians (selected PEDCTYPE codes) Divided by Population"), yaxis = list(title = "Walk Score"))%>%
  add_trace(x = ~per_capita_crash, y = fitted(reg_pedcrash_small), mode = 'lines', hoverinfo = 'skip', name = 'Regression Line')
```

<a href="https://jadannanwosu.github.io/Walk-Score-FARS/scatter4.html" target="_blank">Click here to see the plot generated by this code.</a>

``` r
summary(reg_pedcrash_small)
```

    ## 
    ## Call:
    ## lm(formula = WalkScore ~ per_capita_crash, data = crash_and_pop_small)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -22.651 -12.238  -3.987  13.222  41.213 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.606e+01  3.744e+00  12.301   <2e-16 ***
    ## per_capita_crash 7.144e+05  5.980e+05   1.195    0.236    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.73 on 71 degrees of freedom
    ## Multiple R-squared:  0.01971,    Adjusted R-squared:  0.005901 
    ## F-statistic: 1.427 on 1 and 71 DF,  p-value: 0.2362

``` r
# Only a tiny change in p-value...

set.seed(196)
cvResult2 <- numeric(1000)
for(i in 1:1000) {

  inds <- sample(x = 1:nrow(crash_and_pop_small), size = round(.7*nrow(crash_and_pop_small)),  replace = F) 
  train <- crash_and_pop_small %>% slice(inds) 

  reg_score_CV <- lm(formula = WalkScore ~ per_capita_crash, 
               data = train)
  
  test <- crash_and_pop_small %>% slice(-inds)
  
  
  test$preds <- predict(reg_score_CV, newdata = test)
  
  e <- test$WalkScore - test$preds
  se <- e^2
  mse <- mean(se)
  rmse <- sqrt(mse)
  cvResult2[i] <- rmse
  

}

mean(cvResult2)
```

    ## [1] 16.08465

## Conclusion

Regression analysis did not reveal a statistically significant
relationship between fatal pedestrian-involved crash incidence and Walk
Score. Predictions made with a simple linear model for these two
variables had considerable error. Pruning cases for relevance did not
seem to have any effect on this error or the statistical significance of
the relationship. The inconclusive finding may be due to the fact that
this work included only a narrow range of cities. Additionally, perhaps
Walk Score, intended for use by prospective tenants and homeowners to
choose where to live, is not the best performing measure for walkability
in the context of road safety. More work would be needed for a
definitive answer to the question driving this project.
