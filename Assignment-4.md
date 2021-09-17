Assignment 4
================
Mubarak Ganiyu
23/09/2021

``` r
##install.packages("tidyverse")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
election <- read.csv("mod4-week4/week4-hw-data.csv") # loaded in the data set
#head(election)
ggplot(election, aes(x = trump, y = discrim)) + # instantiated ggplot with trump column mapped to the x-axis and the discrim column to the y-axis
  geom_point(color = "red") + # colored the points red
  geom_smooth(method = "lm", se = FALSE, color = 'black') + # applied a line plot that generalizes the relationship between the two variables
  labs(title = "Perceptions Of Discrimination Track Closely With Voting Against Trump.",x = "2016 Trump Vote", y = "% Saying Discrimination Prevalent") + # labelled the x and y-axis and added the title
  scale_x_continuous(breaks = seq(0,0.7, 0.1),labels = function(x) {paste0(x*100, '%')}) + # edited the x-axis ticks and label
  scale_y_continuous(breaks = seq(0,1,0.1), labels = function(x) {paste0(x*100, '%')}) + # edited the y-axis ticks and label
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) # centered the plot title
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Assignment-4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> The
chart above displays the relationship between the percentage of Trump
voters in different states and the percentage of people who think
discrimination against minorities is prevalent. The result might
misleading for numerous reasons. First of all, correlation does not
imply causation. Just because two variables are correlated does not mean
that a cause-and-effect relationship exists. Looking at the graph, one
might assume that the more prevalent Trump voters exist in a region, the
more indifferent people will be about racism. There could be a
confounding variable that leads to this relationship. For instance, some
of the states that vote for Trump might have a huge proportion of white
people. Hence, they are less likely to report any form of racism.
Multicultural places, however, are likelier to see interracial
interactions between groups of different races. Hence, there are higher
chances of increased racism in those states. Thus, people in more
homogenuous (white) states are less likely to perceive racism as
compared to people in heterogenuous states.

``` r
#head(election) ## Viewed the first five rows of the dataset
election_new <- election %>%
  filter(trump >= 0.5) %>% 
  arrange(desc(trump)) ## arranged the dataset by Trump voters in descending order
ggplot(election_new) + # instantiated ggplot for plotting
  geom_col(aes(x = reorder(state,-trump), y = trump), fill = "red") + # plotted Trump voting states in descending order on x-axis and plotted the percentage of Trump voters on the y-axis
  labs(title = "States that drew over 50% votes for Donald Trump", x = "State") + # appending the graphic with an appropriate title and x-label
  scale_y_continuous(name = "Trump votes", labels=function(x){paste0(x*100, '%')}) + # transformed the y-axis to include the % value
  coord_cartesian(ylim = c(0.5, 0.7)) + # focused more on variables that matter
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))
```

![](Assignment-4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Here we can see all the states that had a Trump majority vote. This
graph shows that most of the states that vote republican or voted for
Trump are located in different parts of U.S.A. This demystifies certain
claims that only people in the south voted for Trump. However, it also
reinforces the claim to an extent as most of the states in the graphic
above are located in the south.

## Part 2

Previously, we explored data about wine.

1.  Make a barplot with the wine data to explore our original question
    about the relationship between alcohol content and quality. Why
    might a simple bar plot be misleading here? (Read a bit about
    geom\_bar first, if needed)

``` r
#?geom_bar
#install.packages("janitor")
library(janitor) # loading janitor
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
winequality_red <- read_delim("https://raw.githubusercontent.com/VU-DSI/EDA-Code/main/mod4/winequality-red.csv", ";") %>% # loading the data
  clean_names() # cleaning the data
```

    ## Rows: 1599 Columns: 12

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## dbl (12): fixed acidity, volatile acidity, citric acid, residual sugar, chlo...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ggplot(winequality_red, aes(x=quality,y=alcohol)) + # instantiating ggplot and mapping wine quality to x-axis and alcohol content to y-axis
  geom_col(color = "blue") + # painting the bars blue
  labs(title = "Alcohol content of different wine quality") 
```

![](Assignment-4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> This
chart is misleading because it only sums up the total number of alcohol
percentage per wine quality. Hence, we cannot establish any relationship
between alcohol content and wine quality. Due to the fact that wine
quality of 5 and 6 are very prevalent in the dataset, they got a huge
values attributed to them disproportionately. If one were to use the
mean rather than the total, it will also be quite unfair due to the fact
that other wine quality values are not as represented as the middle
values. Thus, we might witness both the law of large numbers as well as
small numbers in effect.

2.  A lot of the hardwork in learning R skills is learning how to read
    helpfiles and use stackoverflow on your own. I showed you a glimpse
    of information about themes in class. Use the code below to make
    additional changes. This might take a bit of digging and reading
    about these graphical components online. Specifically, can you
    figure out how to (try to complete 2 out of 4):

-   get rid of panel border completely and keep the grid lines?
-   put the legend on the top or bottom?
-   capitalize the legend name appropriately?
-   add units for alcohol content and wine quality (if applicable)?

``` r
#install.packages("viridis")
library(viridis)
```

    ## Loading required package: viridisLite

``` r
ggplot(winequality_red, aes(x=quality, y= alcohol)) +
      geom_jitter(aes(col=fixed_acidity)) +
      scale_color_viridis(option = "D", direction = -1, alpha = .6) +
      labs(title="Alcohol Content, Fixed Acidity and Wine Quality", y="Alcohol Content", x="Wine Quality", color = "Fixed Acidity")  +
      scale_y_continuous(labels = function(x){paste0(x, "%")}) +
      theme_bw() +
      theme(legend.position = "bottom", plot.title = element_text(hjust=0.5), panel.border = element_blank())
```

![](Assignment-4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

2.  Explain in 1-3 sentences what we can learn from our wine graphic
    produced in class.

Answer: \`\`\`
