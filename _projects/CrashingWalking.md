---
layout: post
title: Pedestrian-Involved Fatal Crashes and Walk Score
description: Using R, I try to ascertain the relationship between road safety and walkability.
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

{% includes first_plot.html %}

``` r
# Top Cities for Crashes Per 100,000 people 
crash_and_pop %>%
  arrange(-per_capita_crash)%>%
  head(10) %>%
   plot_ly( x = ~reorder(PLACENAME,  -per_capita_crash), y = ~per_capita_crash*100000) %>%
  add_trace(type = 'bar') %>%
  layout(title = "U.S. Cities By Fatal Pedestrian-Involved Car Accidents, \n Per 100,000 Inhabitants", xaxis = list(title = "City"), yaxis = list(title = "Crashes Per 100,000 (2022 Population)"))
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-aaccc2b24f789f8de95a" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-aaccc2b24f789f8de95a">{"x":{"visdat":{"136211caa22bf":["function () ","plotlyVisDat"]},"cur_data":"136211caa22bf","attrs":{"136211caa22bf":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"U.S. Cities By Fatal Pedestrian-Involved Car Accidents, <br /> Per 100,000 Inhabitants","xaxis":{"domain":[0,1],"automargin":true,"title":"City","type":"category","categoryorder":"array","categoryarray":["Memphis, TN","Tucson, AZ","New Orleans, LA","Baton Rouge, LA","Phoenix, AZ","Atlanta, GA","Bakersfield, CA","St. Louis, MO","Albuquerque, NM","Sacramento, CA"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Crashes Per 100,000 (2022 Population)"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["Memphis, TN","Tucson, AZ","New Orleans, LA","Baton Rouge, LA","Phoenix, AZ","Atlanta, GA","Bakersfield, CA","St. Louis, MO","Albuquerque, NM","Sacramento, CA"],"y":[13.149827230281993,10.205848315579409,9.0061344641607324,7.9130270405718486,7.4067463316874562,7.1785280525235438,6.8171968660372126,6.7192786182475448,6.69908081559546,6.3556272536773841],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

``` r
# Top Cities for Walk Score
crash_and_pop %>%
  arrange(-WalkScore)%>%
  head(10) %>%
   plot_ly( x = ~reorder(PLACENAME,  -WalkScore), y = ~WalkScore) %>%
  add_trace(type = 'bar') %>%
  layout(title = "10 U.S. Cities With Highest Walk Score", xaxis = list(title = "City"), yaxis = list(title = "Walk Score"))
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-4931ea9ad3be87e3948a" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-4931ea9ad3be87e3948a">{"x":{"visdat":{"1362165cf384c":["function () ","plotlyVisDat"]},"cur_data":"1362165cf384c","attrs":{"1362165cf384c":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"10 U.S. Cities With Highest Walk Score","xaxis":{"domain":[0,1],"automargin":true,"title":"City","type":"category","categoryorder":"array","categoryarray":["San Francisco, CA","New York, NY","Jersey City, NJ","Boston, MA","Chicago, IL","Washington, DC","Miami, FL","Newark, NJ","Oakland, CA","Philadelphia, PA"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Walk Score"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["San Francisco, CA","New York, NY","Jersey City, NJ","Boston, MA","Chicago, IL","Washington, DC","Miami, FL","Newark, NJ","Oakland, CA","Philadelphia, PA"],"y":[88.700000000000003,88,86.599999999999994,82.799999999999997,77.200000000000003,76.700000000000003,76.599999999999994,75.900000000000006,75.299999999999997,74.799999999999997],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

``` r
# Bottom Cities for Walk Score
crash_and_pop %>%
  arrange(WalkScore)%>%
  head(10) %>%
  plot_ly( x = ~reorder(PLACENAME,  -WalkScore), y = ~WalkScore) %>%
   add_trace(type = 'bar') %>%
  layout(title = "10 U.S. Cities With Lowest Walk Score \n Out of 100 With Pop. Over 200K", xaxis = list(title = "City"), yaxis = list(title = "Walk Score"))
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-98179c9cc8ea6544c851" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-98179c9cc8ea6544c851">{"x":{"visdat":{"1362177939cc":["function () ","plotlyVisDat"]},"cur_data":"1362177939cc","attrs":{"1362177939cc":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"10 U.S. Cities With Lowest Walk Score <br /> Out of 100 With Pop. Over 200K","xaxis":{"domain":[0,1],"automargin":true,"title":"City","type":"category","categoryorder":"array","categoryarray":["Raleigh, NC","Anchorage, AK","Durham, NC","Henderson, NV","Greensboro, NC","Gilbert, AZ","Nashville, TN","Charlotte, NC","Jacksonville, FL","Chesapeake, VA"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Walk Score"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["Chesapeake, VA","Jacksonville, FL","Charlotte, NC","Nashville, TN","Gilbert, AZ","Greensboro, NC","Henderson, NV","Durham, NC","Anchorage, AK","Raleigh, NC"],"y":[21.300000000000001,25.600000000000001,26.399999999999999,28.800000000000001,29,29.399999999999999,30,30.300000000000001,30.899999999999999,31.300000000000001],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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

<div class="plotly html-widget html-fill-item" id="htmlwidget-f6098ea6add7dd4c862d" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f6098ea6add7dd4c862d">{"x":{"visdat":{"1362127c183e9":["function () ","plotlyVisDat"]},"cur_data":"1362127c183e9","attrs":{"1362127c183e9":{"x":{},"y":{},"hoverinfo":"text","text":{},"mode":"markers","name":"City","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"1362127c183e9.1":{"x":{},"y":[27.044076760265959,13.338792272876388,8.4900247658030246,17.335924365700627,24.387974234597259,17.288361719631126,42.861615126853884,18.09190988504912,27.510759915799046,1.707738311543519,32.293697897413878,7.3835996460721987,7.8717598978603061,46.999005080927574,4.8998045185502992,70.819630010399834,7.3303070885532939,10.461537720175787,15.313297728248219,23.295385444697104,40.687285906308958,11.191881538282885,50.474182083969872,34.59251832177916,0.55448708441076855,30.27844632885926,9.0121274569232668,32.367421655128823,41.821459572801402,2.7161392846492469,26.061811196763845,4.4894941343322126,7.3603464617702912,4.5077911023292927,9.7292392179626397,12.166705408856101,1.4648385948168627,43.336456382606769,66.525256051240916,5.6170288272516391,42.148112814953244,9.7724691843746854,24.52454753387935,5.2696346696868606,30.803146601896266,11.781946666207723,9.137355289406667,21.481003577092576,80.52756419899697,30.515202213708573,5.7907379786755033,7.1843227442744677,30.005846356873462,24.309914409203795,20.97536539602218,27.486524152303051,19.94566144318571,33.430705173134086,16.558177314396108,103.1919493199436,10.759329808622038,3.0815078235615818,7.0894156349769339,20.416789368720135,32.978608752690619,23.414543099088291,11.26088937263718,56.12564534011031,56.602225398551383,9.8668117668627922,8.5895861890929002,31.471071093524404,22.457761512632388,6.826556193652829,2.1685500884441176,10.882793106895004,25.419345056205493,5.4050189257090073,53.12740428741612,51.987144131439059,39.340484465590592,43.284664480818435,10.205498444135451,3.8789319972360987,35.197334487997026,2.3774410941913326,9.1634510147511286,10.718845579920558,11.480515945174741,16.818090747800778,6.3307192802444492,26.12282111364874,18.67528638525323,21.486320100679659,33.126124458470784,17.342167412976849],"hoverinfo":"skip","text":{},"mode":"lines","name":"Regression Line","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Population vs Fatal Pedestrian-Involved Car Accident Incidence","xaxis":{"domain":[0,1],"automargin":true,"title":"Population","type":"log"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Number of Crashes"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[567242,346023,290509,399679,515426,398994,1003496,410726,576870,227473,685476,279145,284103,1164981,255227,2750534,278609,311917,371562,495511,927811,320242,1320535,744729,218206,637423,296031,687301,966549,235898,547499,251478,278911,251644,303787,331701,225489,1020829,2355890,261915,978003,304261,517971,258654,649600,327130,297371,464125,3903648,642889,263561,277146,631187,513977,455738,576366,439124,714169,388624,8840134,315285,239027,276199,446649,702619,497645,321040,1619078,1647147,305298,291554,665438,480766,273593,231285,316692,534959,259920,1453138,1394592,883822,1018924,309050,246001,761152,233034,297651,314825,323593,392284,268744,548705,419459,464214,706367,399769],"y":[38,11,6,8,37,20,50,28,16,18,12,4,4,25,1,56,3,9,10,7,27,8,75,22,4,36,2,24,32,5,31,4,1,14,11,2,4,9,113,9,41,1,19,3,15,4,2,25,161,32,10,2,83,21,22,25,7,44,35,21,10,13,6,16,29,9,18,61,122,5,2,28,23,10,12,17,34,11,67,55,21,24,9,4,21,6,20,5,10,18,2,56,20,6,15,16],"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["Albuquerque, NM","Anaheim, CA","Anchorage, AK","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Baton Rouge, LA","Boston, MA","Buffalo, NY","Chandler, AZ","Charlotte, NC","Chesapeake, VA","Chicago, IL","Chula Vista, CA","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Corpus Christi, TX","Dallas, TX","Denver, CO","Des Moines, IA","Detroit, MI","Durham, NC","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Gilbert, AZ","Glendale, AZ","Greensboro, NC","Henderson, NV","Hialeah, FL","Honolulu, HI","Houston, TX","Irving, TX","Jacksonville, FL","Jersey City, NJ","Kansas City, MO","Laredo, TX","Las Vegas, NV","Lexington-Fayette, KY","Lincoln, NE","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Omaha, NE","Orlando, FL","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Plano, TX","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","Saint Petersburg, FL","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","St. Paul, MN","Stockton, CA","Tampa, FL","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Washington, DC","Wichita, KS"],"mode":"markers","name":"City","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[567242,346023,290509,399679,515426,398994,1003496,410726,576870,227473,685476,279145,284103,1164981,255227,2750534,278609,311917,371562,495511,927811,320242,1320535,744729,218206,637423,296031,687301,966549,235898,547499,251478,278911,251644,303787,331701,225489,1020829,2355890,261915,978003,304261,517971,258654,649600,327130,297371,464125,3903648,642889,263561,277146,631187,513977,455738,576366,439124,714169,388624,8840134,315285,239027,276199,446649,702619,497645,321040,1619078,1647147,305298,291554,665438,480766,273593,231285,316692,534959,259920,1453138,1394592,883822,1018924,309050,246001,761152,233034,297651,314825,323593,392284,268744,548705,419459,464214,706367,399769],"y":[27.044076760265959,13.338792272876388,8.4900247658030246,17.335924365700627,24.387974234597259,17.288361719631126,42.861615126853884,18.09190988504912,27.510759915799046,1.707738311543519,32.293697897413878,7.3835996460721987,7.8717598978603061,46.999005080927574,4.8998045185502992,70.819630010399834,7.3303070885532939,10.461537720175787,15.313297728248219,23.295385444697104,40.687285906308958,11.191881538282885,50.474182083969872,34.59251832177916,0.55448708441076855,30.27844632885926,9.0121274569232668,32.367421655128823,41.821459572801402,2.7161392846492469,26.061811196763845,4.4894941343322126,7.3603464617702912,4.5077911023292927,9.7292392179626397,12.166705408856101,1.4648385948168627,43.336456382606769,66.525256051240916,5.6170288272516391,42.148112814953244,9.7724691843746854,24.52454753387935,5.2696346696868606,30.803146601896266,11.781946666207723,9.137355289406667,21.481003577092576,80.52756419899697,30.515202213708573,5.7907379786755033,7.1843227442744677,30.005846356873462,24.309914409203795,20.97536539602218,27.486524152303051,19.94566144318571,33.430705173134086,16.558177314396108,103.1919493199436,10.759329808622038,3.0815078235615818,7.0894156349769339,20.416789368720135,32.978608752690619,23.414543099088291,11.26088937263718,56.12564534011031,56.602225398551383,9.8668117668627922,8.5895861890929002,31.471071093524404,22.457761512632388,6.826556193652829,2.1685500884441176,10.882793106895004,25.419345056205493,5.4050189257090073,53.12740428741612,51.987144131439059,39.340484465590592,43.284664480818435,10.205498444135451,3.8789319972360987,35.197334487997026,2.3774410941913326,9.1634510147511286,10.718845579920558,11.480515945174741,16.818090747800778,6.3307192802444492,26.12282111364874,18.67528638525323,21.486320100679659,33.126124458470784,17.342167412976849],"hoverinfo":["skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip"],"text":["Albuquerque, NM","Anaheim, CA","Anchorage, AK","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Baton Rouge, LA","Boston, MA","Buffalo, NY","Chandler, AZ","Charlotte, NC","Chesapeake, VA","Chicago, IL","Chula Vista, CA","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Corpus Christi, TX","Dallas, TX","Denver, CO","Des Moines, IA","Detroit, MI","Durham, NC","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Gilbert, AZ","Glendale, AZ","Greensboro, NC","Henderson, NV","Hialeah, FL","Honolulu, HI","Houston, TX","Irving, TX","Jacksonville, FL","Jersey City, NJ","Kansas City, MO","Laredo, TX","Las Vegas, NV","Lexington-Fayette, KY","Lincoln, NE","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Omaha, NE","Orlando, FL","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Plano, TX","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","Saint Petersburg, FL","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","St. Paul, MN","Stockton, CA","Tampa, FL","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Washington, DC","Wichita, KS"],"mode":"lines","name":"Regression Line","type":"scatter","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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

<div class="plotly html-widget html-fill-item" id="htmlwidget-9024e8d92550d148dd31" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-9024e8d92550d148dd31">{"x":{"visdat":{"1362111bb4a63":["function () ","plotlyVisDat"]},"cur_data":"1362111bb4a63","attrs":{"1362111bb4a63":{"x":{},"y":{},"hoverinfo":"text","text":{},"mode":"markers","name":"City","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"1362111bb4a63.1":{"x":{},"y":[49.89822187911048,46.611083139910001,45.448132343482357,47.56977376469149,49.26116998022809,47.558366119955785,53.69197334566627,47.751092823716604,50.010153322846314,43.821437431409286,51.157315261806815,45.1827622319434,45.299844841652593,54.684304063771975,44.587037335358673,60.397552779599621,45.169980298777936,45.920989117314157,47.084657649004434,48.999118437948795,53.170472181702479,46.096158152268842,55.517806568793993,51.708674971147332,43.544836329970892,50.673968016711285,45.57335586462861,51.174997508403536,53.442497632873419,44.063296978826429,49.66263076905782,44.488626597493251,45.177185080808826,44.493015026815421,45.745351261894371,46.32996441231451,43.763179241341973,53.805861465842987,59.367570292570392,44.759059726575558,53.520843655354795,45.755719736727428,49.29392635119023,44.675739107134653,50.799814553903268,46.237682098191861,45.603391086426527,48.563948709790779,62.725948558491169,50.73075264176142,44.800722931854104,45.134966739312084,50.608586380230555,49.242447751130094,48.442674112788097,50.004340505379069,48.19570516030916,51.430020338904015,47.383235319320704,68.161879377714243,45.992412947374731,44.150928656917372,45.112203779821201,48.308702658213576,51.321587445091339,49.027697760193583,46.112709310022844,56.873279621507351,56.987584784152709,45.778347297500659,45.472011615140467,50.960012649266538,48.798218844911915,45.049158357208043,43.931960656572798,46.022024955136843,49.50853872221289,44.70821029246887,56.154167637889614,55.880682374965836,52.847449102986403,53.793439456879803,45.859579474646523,44.342186554210372,51.853736874232865,43.982062040305038,45.609650005735077,45.982703022957345,46.16538556168404,47.445574157786204,44.930234541515645,49.677263669341812,47.891012538270829,48.565223849366106,51.356968293373384,47.571271125988233],"hoverinfo":"skip","text":{},"mode":"lines","name":"Regression Line","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Population vs Walk Score","xaxis":{"domain":[0,1],"automargin":true,"title":"Population","type":"log"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Walk Score"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[567242,346023,290509,399679,515426,398994,1003496,410726,576870,227473,685476,279145,284103,1164981,255227,2750534,278609,311917,371562,495511,927811,320242,1320535,744729,218206,637423,296031,687301,966549,235898,547499,251478,278911,251644,303787,331701,225489,1020829,2355890,261915,978003,304261,517971,258654,649600,327130,297371,464125,3903648,642889,263561,277146,631187,513977,455738,576366,439124,714169,388624,8840134,315285,239027,276199,446649,702619,497645,321040,1619078,1647147,305298,291554,665438,480766,273593,231285,316692,534959,259920,1453138,1394592,883822,1018924,309050,246001,761152,233034,297651,314825,323593,392284,268744,548705,419459,464214,706367,399769],"y":[42.600000000000001,55.700000000000003,30.899999999999999,38.100000000000001,47.700000000000003,42.5,41.700000000000003,37.299999999999997,64.299999999999997,39.100000000000001,82.799999999999997,66.599999999999994,35.399999999999999,26.399999999999999,21.300000000000001,77.200000000000003,45.600000000000001,49.100000000000001,57.100000000000001,36.100000000000001,41.200000000000003,40.299999999999997,46,61.200000000000003,45,51.100000000000001,30.300000000000001,40.399999999999999,34.899999999999999,49.899999999999999,46.600000000000001,40.200000000000003,29,40.200000000000003,29.399999999999999,30,67.900000000000006,65.700000000000003,47.5,44.799999999999997,25.600000000000001,86.599999999999994,35.299999999999997,36.799999999999997,42,34.299999999999997,44.100000000000001,73.299999999999997,68.599999999999994,34.299999999999997,39,49.700000000000003,35,37.899999999999999,76.599999999999994,61.5,71.400000000000006,28.800000000000001,58,88,75.900000000000006,45.799999999999997,33.700000000000003,75.299999999999997,34.100000000000001,48.200000000000003,41.100000000000001,74.799999999999997,41.399999999999999,62.399999999999999,40.5,67.299999999999997,31.300000000000001,40.200000000000003,50.899999999999999,42.700000000000003,49,43,36.899999999999999,53.299999999999997,88.700000000000003,50.5,67.099999999999994,31.899999999999999,74.400000000000006,49.200000000000003,65.700000000000003,60.399999999999999,43.700000000000003,49.5,46.399999999999999,43.200000000000003,39,33.100000000000001,76.700000000000003,34.799999999999997],"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["Albuquerque, NM","Anaheim, CA","Anchorage, AK","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Baton Rouge, LA","Boston, MA","Buffalo, NY","Chandler, AZ","Charlotte, NC","Chesapeake, VA","Chicago, IL","Chula Vista, CA","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Corpus Christi, TX","Dallas, TX","Denver, CO","Des Moines, IA","Detroit, MI","Durham, NC","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Gilbert, AZ","Glendale, AZ","Greensboro, NC","Henderson, NV","Hialeah, FL","Honolulu, HI","Houston, TX","Irving, TX","Jacksonville, FL","Jersey City, NJ","Kansas City, MO","Laredo, TX","Las Vegas, NV","Lexington-Fayette, KY","Lincoln, NE","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Omaha, NE","Orlando, FL","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Plano, TX","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","Saint Petersburg, FL","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","St. Paul, MN","Stockton, CA","Tampa, FL","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Washington, DC","Wichita, KS"],"mode":"markers","name":"City","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[567242,346023,290509,399679,515426,398994,1003496,410726,576870,227473,685476,279145,284103,1164981,255227,2750534,278609,311917,371562,495511,927811,320242,1320535,744729,218206,637423,296031,687301,966549,235898,547499,251478,278911,251644,303787,331701,225489,1020829,2355890,261915,978003,304261,517971,258654,649600,327130,297371,464125,3903648,642889,263561,277146,631187,513977,455738,576366,439124,714169,388624,8840134,315285,239027,276199,446649,702619,497645,321040,1619078,1647147,305298,291554,665438,480766,273593,231285,316692,534959,259920,1453138,1394592,883822,1018924,309050,246001,761152,233034,297651,314825,323593,392284,268744,548705,419459,464214,706367,399769],"y":[49.89822187911048,46.611083139910001,45.448132343482357,47.56977376469149,49.26116998022809,47.558366119955785,53.69197334566627,47.751092823716604,50.010153322846314,43.821437431409286,51.157315261806815,45.1827622319434,45.299844841652593,54.684304063771975,44.587037335358673,60.397552779599621,45.169980298777936,45.920989117314157,47.084657649004434,48.999118437948795,53.170472181702479,46.096158152268842,55.517806568793993,51.708674971147332,43.544836329970892,50.673968016711285,45.57335586462861,51.174997508403536,53.442497632873419,44.063296978826429,49.66263076905782,44.488626597493251,45.177185080808826,44.493015026815421,45.745351261894371,46.32996441231451,43.763179241341973,53.805861465842987,59.367570292570392,44.759059726575558,53.520843655354795,45.755719736727428,49.29392635119023,44.675739107134653,50.799814553903268,46.237682098191861,45.603391086426527,48.563948709790779,62.725948558491169,50.73075264176142,44.800722931854104,45.134966739312084,50.608586380230555,49.242447751130094,48.442674112788097,50.004340505379069,48.19570516030916,51.430020338904015,47.383235319320704,68.161879377714243,45.992412947374731,44.150928656917372,45.112203779821201,48.308702658213576,51.321587445091339,49.027697760193583,46.112709310022844,56.873279621507351,56.987584784152709,45.778347297500659,45.472011615140467,50.960012649266538,48.798218844911915,45.049158357208043,43.931960656572798,46.022024955136843,49.50853872221289,44.70821029246887,56.154167637889614,55.880682374965836,52.847449102986403,53.793439456879803,45.859579474646523,44.342186554210372,51.853736874232865,43.982062040305038,45.609650005735077,45.982703022957345,46.16538556168404,47.445574157786204,44.930234541515645,49.677263669341812,47.891012538270829,48.565223849366106,51.356968293373384,47.571271125988233],"hoverinfo":["skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip"],"text":["Albuquerque, NM","Anaheim, CA","Anchorage, AK","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Baton Rouge, LA","Boston, MA","Buffalo, NY","Chandler, AZ","Charlotte, NC","Chesapeake, VA","Chicago, IL","Chula Vista, CA","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Corpus Christi, TX","Dallas, TX","Denver, CO","Des Moines, IA","Detroit, MI","Durham, NC","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Gilbert, AZ","Glendale, AZ","Greensboro, NC","Henderson, NV","Hialeah, FL","Honolulu, HI","Houston, TX","Irving, TX","Jacksonville, FL","Jersey City, NJ","Kansas City, MO","Laredo, TX","Las Vegas, NV","Lexington-Fayette, KY","Lincoln, NE","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Omaha, NE","Orlando, FL","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Plano, TX","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","Saint Petersburg, FL","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","St. Paul, MN","Stockton, CA","Tampa, FL","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Washington, DC","Wichita, KS"],"mode":"lines","name":"Regression Line","type":"scatter","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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

<div class="plotly html-widget html-fill-item" id="htmlwidget-bd42a0be050d03d45b6f" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-bd42a0be050d03d45b6f">{"x":{"visdat":{"136211428bca":["function () ","plotlyVisDat"]},"cur_data":"136211428bca","attrs":{"136211428bca":{"x":{},"y":{},"hoverinfo":"text","text":{},"mode":"markers","name":"City","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"136211428bca.1":{"x":{},"y":[46.307414081416205,49.093111110494242,49.974409788591842,50.024847020640685,45.927994684652262,47.642036413315594,47.665797889740801,46.213940770580336,49.413924684352608,45.34673538956374,50.223478870922335,50.474865989636612,50.494655705097969,49.910611838518783,51.298790469645951,49.997654019953551,50.756727030453746,49.32545407494792,49.4790128553473,50.490902252239835,49.305913238102619,49.63192903314954,47.114265263599151,49.271077774241277,50.158173705611887,47.139411212099823,51.074202613694766,48.845459955558681,48.988833766656121,49.931502062330416,47.128039393424515,50.350107266996694,51.325119902420184,47.20614414369048,48.74334315112349,51.131697317513634,50.205028815590481,50.911155812655437,47.813065296165668,48.889531683181993,48.291267558532098,51.348759753789231,48.705989832060098,50.690985871584779,49.781495686443989,50.641204946136313,51.076611840561554,47.34616495077303,48.344976863547885,47.669794129049876,48.606254435611277,51.037770875963687,41.202497158713825,48.375493087729836,47.788654585220414,48.176277404023864,50.347348263397315,46.733228931112492,44.481684707629356,51.420863095715404,49.098845303504547,47.304826877908113,49.88972841382882,48.773990371020432,48.342549389782093,50.1776508973714,47.171828044484378,48.627314095362777,45.747389948319523,50.312796057479169,51.065992680886275,48.278970711981223,47.822923096806313,48.716352606613633,47.502916053957961,47.360796024774039,46.579212380044986,48.259727390747621,47.960083630448551,48.487851695353847,49.728528809641404,49.744845342544536,49.304271384565368,50.322082334765064,49.42548866230559,49.571293768018712,46.291430178479679,50.352016429370309,49.163287861252115,47.977651245742216,51.019916528178342,43.532269066586025,47.83557366594858,50.586005531266672,49.928350942086695,48.441552253057473],"hoverinfo":"skip","text":{},"mode":"lines","name":"Regression Line","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Crashes Per Capita vs Walk Score <br /> From 100 Cities With Pop. Over 200K","xaxis":{"domain":[0,1],"automargin":true,"title":"Fatal Crashes Involving Pedestrians Divided by Population"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Walk Score"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[6.6990808155954599e-05,3.1789794320030747e-05,2.065340488590715e-05,2.0016062890469602e-05,7.1785280525235438e-05,5.0126067058652509e-05,4.9825808971834465e-05,6.8171968660372124e-05,2.7735885034756532e-05,7.9130270405718483e-05,1.7506083363968978e-05,1.43294703469523e-05,1.4079400780702774e-05,2.1459577452336133e-05,3.9180807673169377e-06,2.035968288339646e-05,1.0767778499617744e-05,2.8853829704697084e-05,2.6913408798531604e-05,1.4126830685897992e-05,2.9100754356221256e-05,2.4981108037046984e-05,5.6795162566686987e-05,2.954094710961974e-05,1.8331301614071109e-05,5.6477409820480281e-05,6.7560491975502561e-06,3.4919198429800046e-05,3.3107478255111742e-05,2.1195601488779048e-05,5.6621107983758876e-05,1.5905963941179747e-05,3.5853731118528851e-06,5.5634149830713228e-05,3.6209581055147192e-05,6.0295265917196511e-06,1.7739224529799681e-05,8.8163639551776057e-06,4.7964888004108848e-05,3.436229311036023e-05,4.1922161792959736e-05,3.28665192055505e-06,3.668159028208143e-05,1.1598506112412721e-05,2.309113300492611e-05,1.2227554794729924e-05,6.7256053885550371e-06,5.3864799353622408e-05,4.1243472772135193e-05,4.9775311134581556e-05,3.7941880627255167e-05,7.2164130097493736e-06,0.00013149827230281993,4.085785939837775e-05,4.8273350038838101e-05,4.3375216442330046e-05,1.5940827647771471e-05,6.1610067084961678e-05,9.0061344641607317e-05,2.3755296017006077e-06,3.1717335109504099e-05,5.4387161283034968e-05,2.1723467499882331e-05,3.5822312375041697e-05,4.1274147155143829e-05,1.8085181203468334e-05,5.6067779715923246e-05,3.767576361361219e-05,7.4067463316874565e-05,1.6377441057589633e-05,6.8597926970646947e-06,4.2077548922664468e-05,4.7840321486960394e-05,3.6550642743052636e-05,5.1884039172449572e-05,5.3679916133025147e-05,6.3556272536773842e-05,4.2320714065866422e-05,4.6107114396567981e-05,3.9438057869254952e-05,2.3760440450678984e-05,2.3554259198919644e-05,2.9121501375182011e-05,1.6260096503672749e-05,2.7589758681577397e-05,2.5747315842323438e-05,6.7192786182475447e-05,1.5881839116969745e-05,3.0903017061555719e-05,4.5885124042785326e-05,7.442026612687167e-06,0.00010205848315579409,4.7680464598447046e-05,1.292507334979126e-05,2.1235420114473072e-05,4.0023113347958447e-05],"y":[42.600000000000001,55.700000000000003,30.899999999999999,38.100000000000001,47.700000000000003,42.5,41.700000000000003,37.299999999999997,64.299999999999997,39.100000000000001,82.799999999999997,66.599999999999994,35.399999999999999,26.399999999999999,21.300000000000001,77.200000000000003,45.600000000000001,49.100000000000001,57.100000000000001,36.100000000000001,41.200000000000003,40.299999999999997,46,61.200000000000003,45,51.100000000000001,30.300000000000001,40.399999999999999,34.899999999999999,49.899999999999999,46.600000000000001,40.200000000000003,29,40.200000000000003,29.399999999999999,30,67.900000000000006,65.700000000000003,47.5,44.799999999999997,25.600000000000001,86.599999999999994,35.299999999999997,36.799999999999997,42,34.299999999999997,44.100000000000001,73.299999999999997,68.599999999999994,34.299999999999997,39,49.700000000000003,35,37.899999999999999,76.599999999999994,61.5,71.400000000000006,28.800000000000001,58,88,75.900000000000006,45.799999999999997,33.700000000000003,75.299999999999997,34.100000000000001,48.200000000000003,41.100000000000001,74.799999999999997,41.399999999999999,62.399999999999999,40.5,67.299999999999997,31.300000000000001,40.200000000000003,50.899999999999999,42.700000000000003,49,43,36.899999999999999,53.299999999999997,88.700000000000003,50.5,67.099999999999994,31.899999999999999,74.400000000000006,49.200000000000003,65.700000000000003,60.399999999999999,43.700000000000003,49.5,46.399999999999999,43.200000000000003,39,33.100000000000001,76.700000000000003,34.799999999999997],"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["Albuquerque, NM","Anaheim, CA","Anchorage, AK","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Baton Rouge, LA","Boston, MA","Buffalo, NY","Chandler, AZ","Charlotte, NC","Chesapeake, VA","Chicago, IL","Chula Vista, CA","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Corpus Christi, TX","Dallas, TX","Denver, CO","Des Moines, IA","Detroit, MI","Durham, NC","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Gilbert, AZ","Glendale, AZ","Greensboro, NC","Henderson, NV","Hialeah, FL","Honolulu, HI","Houston, TX","Irving, TX","Jacksonville, FL","Jersey City, NJ","Kansas City, MO","Laredo, TX","Las Vegas, NV","Lexington-Fayette, KY","Lincoln, NE","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Omaha, NE","Orlando, FL","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Plano, TX","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","Saint Petersburg, FL","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","St. Paul, MN","Stockton, CA","Tampa, FL","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Washington, DC","Wichita, KS"],"mode":"markers","name":"City","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[6.6990808155954599e-05,3.1789794320030747e-05,2.065340488590715e-05,2.0016062890469602e-05,7.1785280525235438e-05,5.0126067058652509e-05,4.9825808971834465e-05,6.8171968660372124e-05,2.7735885034756532e-05,7.9130270405718483e-05,1.7506083363968978e-05,1.43294703469523e-05,1.4079400780702774e-05,2.1459577452336133e-05,3.9180807673169377e-06,2.035968288339646e-05,1.0767778499617744e-05,2.8853829704697084e-05,2.6913408798531604e-05,1.4126830685897992e-05,2.9100754356221256e-05,2.4981108037046984e-05,5.6795162566686987e-05,2.954094710961974e-05,1.8331301614071109e-05,5.6477409820480281e-05,6.7560491975502561e-06,3.4919198429800046e-05,3.3107478255111742e-05,2.1195601488779048e-05,5.6621107983758876e-05,1.5905963941179747e-05,3.5853731118528851e-06,5.5634149830713228e-05,3.6209581055147192e-05,6.0295265917196511e-06,1.7739224529799681e-05,8.8163639551776057e-06,4.7964888004108848e-05,3.436229311036023e-05,4.1922161792959736e-05,3.28665192055505e-06,3.668159028208143e-05,1.1598506112412721e-05,2.309113300492611e-05,1.2227554794729924e-05,6.7256053885550371e-06,5.3864799353622408e-05,4.1243472772135193e-05,4.9775311134581556e-05,3.7941880627255167e-05,7.2164130097493736e-06,0.00013149827230281993,4.085785939837775e-05,4.8273350038838101e-05,4.3375216442330046e-05,1.5940827647771471e-05,6.1610067084961678e-05,9.0061344641607317e-05,2.3755296017006077e-06,3.1717335109504099e-05,5.4387161283034968e-05,2.1723467499882331e-05,3.5822312375041697e-05,4.1274147155143829e-05,1.8085181203468334e-05,5.6067779715923246e-05,3.767576361361219e-05,7.4067463316874565e-05,1.6377441057589633e-05,6.8597926970646947e-06,4.2077548922664468e-05,4.7840321486960394e-05,3.6550642743052636e-05,5.1884039172449572e-05,5.3679916133025147e-05,6.3556272536773842e-05,4.2320714065866422e-05,4.6107114396567981e-05,3.9438057869254952e-05,2.3760440450678984e-05,2.3554259198919644e-05,2.9121501375182011e-05,1.6260096503672749e-05,2.7589758681577397e-05,2.5747315842323438e-05,6.7192786182475447e-05,1.5881839116969745e-05,3.0903017061555719e-05,4.5885124042785326e-05,7.442026612687167e-06,0.00010205848315579409,4.7680464598447046e-05,1.292507334979126e-05,2.1235420114473072e-05,4.0023113347958447e-05],"y":[46.307414081416205,49.093111110494242,49.974409788591842,50.024847020640685,45.927994684652262,47.642036413315594,47.665797889740801,46.213940770580336,49.413924684352608,45.34673538956374,50.223478870922335,50.474865989636612,50.494655705097969,49.910611838518783,51.298790469645951,49.997654019953551,50.756727030453746,49.32545407494792,49.4790128553473,50.490902252239835,49.305913238102619,49.63192903314954,47.114265263599151,49.271077774241277,50.158173705611887,47.139411212099823,51.074202613694766,48.845459955558681,48.988833766656121,49.931502062330416,47.128039393424515,50.350107266996694,51.325119902420184,47.20614414369048,48.74334315112349,51.131697317513634,50.205028815590481,50.911155812655437,47.813065296165668,48.889531683181993,48.291267558532098,51.348759753789231,48.705989832060098,50.690985871584779,49.781495686443989,50.641204946136313,51.076611840561554,47.34616495077303,48.344976863547885,47.669794129049876,48.606254435611277,51.037770875963687,41.202497158713825,48.375493087729836,47.788654585220414,48.176277404023864,50.347348263397315,46.733228931112492,44.481684707629356,51.420863095715404,49.098845303504547,47.304826877908113,49.88972841382882,48.773990371020432,48.342549389782093,50.1776508973714,47.171828044484378,48.627314095362777,45.747389948319523,50.312796057479169,51.065992680886275,48.278970711981223,47.822923096806313,48.716352606613633,47.502916053957961,47.360796024774039,46.579212380044986,48.259727390747621,47.960083630448551,48.487851695353847,49.728528809641404,49.744845342544536,49.304271384565368,50.322082334765064,49.42548866230559,49.571293768018712,46.291430178479679,50.352016429370309,49.163287861252115,47.977651245742216,51.019916528178342,43.532269066586025,47.83557366594858,50.586005531266672,49.928350942086695,48.441552253057473],"hoverinfo":["skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip"],"text":["Albuquerque, NM","Anaheim, CA","Anchorage, AK","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Baton Rouge, LA","Boston, MA","Buffalo, NY","Chandler, AZ","Charlotte, NC","Chesapeake, VA","Chicago, IL","Chula Vista, CA","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Corpus Christi, TX","Dallas, TX","Denver, CO","Des Moines, IA","Detroit, MI","Durham, NC","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Gilbert, AZ","Glendale, AZ","Greensboro, NC","Henderson, NV","Hialeah, FL","Honolulu, HI","Houston, TX","Irving, TX","Jacksonville, FL","Jersey City, NJ","Kansas City, MO","Laredo, TX","Las Vegas, NV","Lexington-Fayette, KY","Lincoln, NE","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Omaha, NE","Orlando, FL","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Plano, TX","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","Saint Petersburg, FL","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","St. Paul, MN","Stockton, CA","Tampa, FL","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Washington, DC","Wichita, KS"],"mode":"lines","name":"Regression Line","type":"scatter","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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

<div class="plotly html-widget html-fill-item" id="htmlwidget-c24c1d0e7d26086dcfc7" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-c24c1d0e7d26086dcfc7">{"x":{"visdat":{"1362175adede8":["function () ","plotlyVisDat"]},"cur_data":"1362175adede8","attrs":{"1362175adede8":{"x":{},"y":{},"hoverinfo":"text","text":{},"mode":"markers","name":"City","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"1362175adede8.1":{"x":{},"y":[49.83764095022174,50.188569185443555,47.846768080247571,51.60353399919795,53.221415522772638,51.754673391613849,49.538073674433278,47.297733841815145,48.143728491010087,48.573924032580322,46.672547387900487,49.176127425616258,50.640075495913706,49.904748085845512,47.501073177954687,46.829304930823,49.305309627789306,47.977885594851962,51.663197049500418,49.177635268031239,48.2767125716965,52.116243872919149,53.888467069919038,48.900152425293776,48.898278429706487,49.22757699411855,50.001481024412563,48.786948270159094,48.250743201090216,50.197047394690941,48.821337169974491,48.258846374035087,52.216352109396965,52.098662597381292,50.504303262469818,51.480516487012892,48.637046595266121,53.982263007546287,50.229200819493613,55.464836010530107,54.735853911212416,50.939999577379943,51.06099243036828,51.574223902535252,46.786639267540224,48.325226262190839,49.048132870010932,48.645884845955557,50.857771284758151,48.092871374748114,50.471755348970056,51.264013439336196,48.399349577354528,48.206495519659086,47.545291757138983,48.670522217983667,55.325919630932503,55.082705232104189,50.065650646657744,48.025838813593097,50.157479651508446,51.717536529375472,47.461592255057724,50.682570409361205,48.963401317623024,49.813668318238257,49.12499724154862,48.459467897730278,48.717636814814256,56.475242269970309,49.465646957962377,47.598275855785509,47.846365669919074],"hoverinfo":"skip","text":{},"mode":"lines","name":"Regression Line","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Crashes Per Capita vs Walk Score <br /> Subsetted from 100 Cities With Pop. Over 200K","xaxis":{"domain":[0,1],"automargin":true,"title":"Fatal Crashes Involving Pedestrians (selected PEDCTYPE codes) Divided by Population"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Walk Score"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[5.2887480123122053e-06,5.7799626036419541e-06,2.5020078613087003e-06,7.7605708675930198e-06,1.0025213411730502e-05,7.9721294354935149e-06,4.8694263328837227e-06,1.7334928146722833e-06,2.9176805606614966e-06,3.5198501951756935e-06,8.5838309809344531e-07,4.362789189299242e-06,6.4119621565993522e-06,5.382681759706321e-06,2.0181186694139989e-06,1.0778057168970836e-06,4.5436130053349588e-06,2.6855406463290674e-06,7.8440846972889279e-06,4.3648998037250057e-06,3.103826086416726e-06,8.4782405955116202e-06,1.0958924125888815e-05,3.9764909852949366e-06,3.9738678450509451e-06,4.4348061324499203e-06,5.5180844606496909e-06,3.8180325678178033e-06,3.0674752531433952e-06,5.7918300445391732e-06,3.8661687041375737e-06,3.078817733990148e-06,8.6183678965795856e-06,8.4536310650960334e-06,6.2219138918226945e-06,7.5883761254510338e-06,3.6082065048746868e-06,1.1090215736382403e-05,5.8368370569111071e-06,1.3165459101501301e-05,1.2145060603852413e-05,6.831783277616345e-06,7.0011439869274637e-06,7.7195438264234842e-06,1.0180841150145462e-06,3.1717335109504098e-06,4.1836277910026899e-06,3.6205779166470553e-06,6.716683570320319e-06,2.8464929072512986e-06,6.1763546907560968e-06,7.2853242606761871e-06,3.2754882115179267e-06,3.0055392087617479e-06,2.0800139776939302e-06,3.6550642743052636e-06,1.2971009793112393e-05,1.263056850188827e-05,5.6079064003035745e-06,2.75266354606376e-06,5.736444780982538e-06,7.920146816892994e-06,1.9628549332433036e-06,6.4714447500404462e-06,4.0650241259181873e-06,5.2551921298242662e-06,4.2912193070539064e-06,3.3596393091237724e-06,3.7210133063435835e-06,1.4579783307970586e-05,4.7680464598447044e-06,2.1541788916318767e-06,2.5014445842474029e-06],"y":[42.600000000000001,55.700000000000003,38.100000000000001,47.700000000000003,42.5,41.700000000000003,37.299999999999997,64.299999999999997,82.799999999999997,35.399999999999999,26.399999999999999,77.200000000000003,49.100000000000001,57.100000000000001,36.100000000000001,41.200000000000003,46,61.200000000000003,51.100000000000001,40.399999999999999,34.899999999999999,49.899999999999999,46.600000000000001,40.200000000000003,40.200000000000003,67.900000000000006,47.5,44.799999999999997,25.600000000000001,35.299999999999997,36.799999999999997,42,73.299999999999997,68.599999999999994,34.299999999999997,39,49.700000000000003,35,37.899999999999999,76.599999999999994,61.5,71.400000000000006,28.800000000000001,58,88,75.900000000000006,45.799999999999997,33.700000000000003,75.299999999999997,34.100000000000001,74.799999999999997,41.399999999999999,62.399999999999999,67.299999999999997,31.300000000000001,40.200000000000003,50.899999999999999,42.700000000000003,49,36.899999999999999,53.299999999999997,88.700000000000003,50.5,67.099999999999994,31.899999999999999,74.400000000000006,49.200000000000003,65.700000000000003,46.399999999999999,43.200000000000003,39,33.100000000000001,34.799999999999997],"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["Albuquerque, NM","Anaheim, CA","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Boston, MA","Chandler, AZ","Charlotte, NC","Chicago, IL","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Dallas, TX","Denver, CO","Detroit, MI","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Glendale, AZ","Hialeah, FL","Houston, TX","Irving, TX","Jacksonville, FL","Kansas City, MO","Laredo, TX","Las Vegas, NV","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Wichita, KS"],"mode":"markers","name":"City","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[5.2887480123122053e-06,5.7799626036419541e-06,2.5020078613087003e-06,7.7605708675930198e-06,1.0025213411730502e-05,7.9721294354935149e-06,4.8694263328837227e-06,1.7334928146722833e-06,2.9176805606614966e-06,3.5198501951756935e-06,8.5838309809344531e-07,4.362789189299242e-06,6.4119621565993522e-06,5.382681759706321e-06,2.0181186694139989e-06,1.0778057168970836e-06,4.5436130053349588e-06,2.6855406463290674e-06,7.8440846972889279e-06,4.3648998037250057e-06,3.103826086416726e-06,8.4782405955116202e-06,1.0958924125888815e-05,3.9764909852949366e-06,3.9738678450509451e-06,4.4348061324499203e-06,5.5180844606496909e-06,3.8180325678178033e-06,3.0674752531433952e-06,5.7918300445391732e-06,3.8661687041375737e-06,3.078817733990148e-06,8.6183678965795856e-06,8.4536310650960334e-06,6.2219138918226945e-06,7.5883761254510338e-06,3.6082065048746868e-06,1.1090215736382403e-05,5.8368370569111071e-06,1.3165459101501301e-05,1.2145060603852413e-05,6.831783277616345e-06,7.0011439869274637e-06,7.7195438264234842e-06,1.0180841150145462e-06,3.1717335109504098e-06,4.1836277910026899e-06,3.6205779166470553e-06,6.716683570320319e-06,2.8464929072512986e-06,6.1763546907560968e-06,7.2853242606761871e-06,3.2754882115179267e-06,3.0055392087617479e-06,2.0800139776939302e-06,3.6550642743052636e-06,1.2971009793112393e-05,1.263056850188827e-05,5.6079064003035745e-06,2.75266354606376e-06,5.736444780982538e-06,7.920146816892994e-06,1.9628549332433036e-06,6.4714447500404462e-06,4.0650241259181873e-06,5.2551921298242662e-06,4.2912193070539064e-06,3.3596393091237724e-06,3.7210133063435835e-06,1.4579783307970586e-05,4.7680464598447044e-06,2.1541788916318767e-06,2.5014445842474029e-06],"y":[49.83764095022174,50.188569185443555,47.846768080247571,51.60353399919795,53.221415522772638,51.754673391613849,49.538073674433278,47.297733841815145,48.143728491010087,48.573924032580322,46.672547387900487,49.176127425616258,50.640075495913706,49.904748085845512,47.501073177954687,46.829304930823,49.305309627789306,47.977885594851962,51.663197049500418,49.177635268031239,48.2767125716965,52.116243872919149,53.888467069919038,48.900152425293776,48.898278429706487,49.22757699411855,50.001481024412563,48.786948270159094,48.250743201090216,50.197047394690941,48.821337169974491,48.258846374035087,52.216352109396965,52.098662597381292,50.504303262469818,51.480516487012892,48.637046595266121,53.982263007546287,50.229200819493613,55.464836010530107,54.735853911212416,50.939999577379943,51.06099243036828,51.574223902535252,46.786639267540224,48.325226262190839,49.048132870010932,48.645884845955557,50.857771284758151,48.092871374748114,50.471755348970056,51.264013439336196,48.399349577354528,48.206495519659086,47.545291757138983,48.670522217983667,55.325919630932503,55.082705232104189,50.065650646657744,48.025838813593097,50.157479651508446,51.717536529375472,47.461592255057724,50.682570409361205,48.963401317623024,49.813668318238257,49.12499724154862,48.459467897730278,48.717636814814256,56.475242269970309,49.465646957962377,47.598275855785509,47.846365669919074],"hoverinfo":["skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip","skip"],"text":["Albuquerque, NM","Anaheim, CA","Arlington, TX","Atlanta, GA","Aurora, CO","Austin, TX","Bakersfield, CA","Baltimore, MD","Boston, MA","Chandler, AZ","Charlotte, NC","Chicago, IL","Cincinnati, OH","Cleveland, OH","Colorado Springs, CO","Columbus, OH","Dallas, TX","Denver, CO","Detroit, MI","El Paso, TX","Fort Worth, TX","Fremont, CA","Fresno, CA","Garland, TX","Glendale, AZ","Hialeah, FL","Houston, TX","Irving, TX","Jacksonville, FL","Kansas City, MO","Laredo, TX","Las Vegas, NV","Long Beach, CA","Los Angeles, CA","Louisville, KY","Lubbock, TX","Madison, WI","Memphis, TN","Mesa, AZ","Miami, FL","Milwaukee, WI","Minneapolis, MN","Nashville, TN","New Orleans, LA","New York, NY","Newark, NJ","Norfolk, VA","North Las Vegas, NV","Oakland, CA","Oklahoma City, OK","Philadelphia, PA","Phoenix, AZ","Pittsburgh, PA","Portland, OR","Raleigh, NC","Reno, NV","Richmond, VA","Riverside, CA","Sacramento, CA","San Antonio, TX","San Diego, CA","San Francisco, CA","San Jose, CA","Santa Ana, CA","Scottsdale, AZ","Seattle, WA","Spokane, WA","St. Louis, MO","Toledo, OH","Tucson, AZ","Tulsa, OK","Virginia Beach, VA","Wichita, KS"],"mode":"lines","name":"Regression Line","type":"scatter","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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
