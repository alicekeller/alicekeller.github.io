---
title: "Ultrarunning"
subtitle: "Diving into speed and gender differences in ultramarathon races"
---

This project stems from my love of running! While I've never done an ultramarathon, I'm fascinated with the reasons that would drive somebody to run that far (50+ miles). 

I mainly wanted to investigate the relationship between age, gender, and speed with this project. The dataset is from an old [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-26/readme.md) project that I thought looked interesting. It's a large dataset with over 100,000 observations from many ultra races around the world. Some of the most popular and well-known are the [UTMB](https://utmbmontblanc.com/jp/page/1/a-mythical-race,-an-unique-experience.html), [Western States](https://www.wser.org/), and the [Leadville 100](https://www.leadvilleraceseries.com/run/leadvilletrail100run/). 

Follow along below as I explore the data and predict speed from various factors within the dataset! I've collapsed my code to make things more readable, but click the little black arrows on the left hand side to check it out.

***

### Data Cleaning and Exploratory Data Analysis

<details>
  <summary>Click for R code!</summary>

  
{% highlight r %}
  library(tidyverse)
  library(tidytuesdayR)
  library(tidymodels)

  #load data
  tuesdata <- tidytuesdayR::tt_load(2021, week = 44)
  ultra_rankings <- tuesdata$ultra_rankings
  race <- tuesdata$race

  glimpse(ultra_rankings)
  glimpse(race)
{% endhighlight %}
        
</details>
  
Below is an initial plot of age and finishing time to get an idea of the shape of the data.
  
<details>
  <summary></summary>
  
  
{% highlight r %}
my.theme <- theme(
  plot.title = element_text(color = "black", size = 22, face = "bold"),
  axis.title = element_text(color = "black", size = 18),
  axis.text = element_text(color = "black", size = 16), 
  legend.text = element_text(color = "black", size = 12))

p1 <- ultra_rankings %>%
  ggplot(aes(age, time_in_seconds)) +
  geom_jitter(aes(color = race_year_id), alpha = 0.7, show.legend = FALSE) +
  labs(title = "Ultramarathon finishing time by Age", x = "Age", y = "Time (seconds)") +
  theme_classic() +
  my.theme
p1
{% endhighlight %}
        
</details>

![p1]({{site.url}}/assets/img/p1_ultra.png)

There seems to be many 0's present in the data, which I eventually mutated to NA's because there really isn't any value in keeping them as zero.

I also wanted to join the two datasets with race info on the variable `race_year_id`, which is essentially a unique identifier for each race. You'll see that I also changed a few of the variables to factors from numeric.

<details>
  <summary></summary>
  
{% highlight r %}
#inner join
joined <- ultra_rankings %>%
  inner_join(race, by = "race_year_id")

#changing race_year_id, rank, gender, nationality to factor
joined <- joined %>%
  mutate(race_year_id = as.factor(race_year_id),
         rank = as.factor(rank),
         gender = as.factor(gender),
         nationality = as.factor(nationality))
glimpse(joined)
{% endhighlight %}

</details>
  
After taking a look at the data a little more, I realized that one of my favorite races (the Leadville 100) didn't have any distance data. I double checked the exact distance and added it in so the race could be a part of my analysis.

<details>
  <summary></summary>

{% highlight r %}
#leadville 100 distance is 0 - want it to be 100 mi
#first create new df with only leadville so can manipulate  those distances
leadville <- joined %>%
  filter(race_year_id == "25331")

#remove leadville stats with 0 distance from joined df
joined <- joined %>%
  filter(race_year_id != "25331")

#add in 100 mile distance in km to subsetted df
leadville$distance <-replace(leadville$distance, leadville$distance == 0, 160.9)

#row bind the two dataframes back together with updated leadville distance
joined <- joined %>%
  rbind(leadville)
{% endhighlight %}
  
</details>
  
  
> collect_metrics(lm_res)
# A tibble: 2 × 6
  .metric .estimator  mean     n  std_err .config             
  <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
1 rmse    standard   0.192    25 0.000175 Preprocessor1_Model1
2 rsq     standard   0.544    25 0.000633 Preprocessor1_Model1
    
> collect_metrics(rf_res)
# A tibble: 2 × 6
  .metric .estimator  mean     n  std_err .config             
  <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
1 rmse    standard   0.180    25 0.000246 Preprocessor1_Model1
2 rsq     standard   0.641    25 0.000696 Preprocessor1_Model1

    
> metrics(results.b, truth = mph, estimate = .pred_lm)
# A tibble: 3 × 3
  .metric .estimator .estimate
  <chr>   <chr>          <dbl>
1 rmse    standard       0.192
2 rsq     standard       0.530
3 mae     standard       0.148
> metrics(results.b, truth = mph, estimate = .pred_rf)
# A tibble: 3 × 3
  .metric .estimator .estimate
  <chr>   <chr>          <dbl>
1 rmse    standard       0.179
2 rsq     standard       0.632
3 mae     standard       0.138
