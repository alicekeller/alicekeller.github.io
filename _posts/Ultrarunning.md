---
title: "Ultrarunning"
author: "Ali Keller"
date: '2022-06-06'
---


This project stems from my love of running! While I've never done an ultramarathon, I'm fascinated with the reasons that would drive somebody to run that far (50+ miles). 

I mainly wanted to investigate the relationship between age, gender, and speed with this project. The dataset is from an old [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-26/readme.md) project that I thought looked interesting. It's a large dataset with over 100,000 observations from many ultra races around the world. Some of the most popular and well-known are the [UTMB](https://utmbmontblanc.com/jp/page/1/a-mythical-race,-an-unique-experience.html), [Western States](https://www.wser.org/), and the [Leadville 100](https://www.leadvilleraceseries.com/run/leadvilletrail100run/). 

Follow along below as I explore the data and predict speed from various factors within the dataset!

***

### Data Cleaning and Exploratory Data Analysis

<details>
  <summary>Click to expand!</summary>
  
  ```{r}
  library(tidyverse)
  library(tidytuesdayR)
  library(tidymodels)

  #load data
  tuesdata <- tidytuesdayR::tt_load(2021, week = 44)
  ultra_rankings <- tuesdata$ultra_rankings
  race <- tuesdata$race

  glimpse(ultra_rankings)
  glimpse(race)
  ```
 </details>
  
  
