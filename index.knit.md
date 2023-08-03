---
title: "Introduction to R"
---




## Set up

We will need:

-   [R](https://cran.r-project.org/bin/windows/base/)
-   [RStudio](https://posit.co/download/rstudio-desktop/)
-   [Tidverse](https://www.tidyverse.org) meta package (collection of multiple packages)


## Setting up RStudio

-   Global settings
    -   Organise panes
    -   Dark theme
    -   Add margin marker
    -   Add rainbow parentheses
    -   Show whitespace characters 
    -   ....lots fo stuff to mess with


## Setting up a project

-   File structure
    - add folders: `data`, `figures`, `scripts` ...

<br/>

::: {.cell}
::: {.cell-output-display}
![](./docs/assets/file_structure.png)
:::
:::

<br/>



::: {.cell}

```{.r .cell-code}
getwd()
```

::: {.cell-output .cell-output-stdout}
```
[1] "E:/Dropbox/3_RandomR/R_tutorial_caw"
```
:::
:::



File paths and accessing files:

-   `"./figures/figure_1.png"`
-   `"./data/my_raw_data.csv"`
-   `"./scripts/my_helper_functions.r"`



## Gettin started

Set up a script


::: {.cell}

```{.r .cell-code}
# Description ----
# here is where I will describe what this script does ...


# Set up -----------------------------------------------------------------------

# load packages



# Load data --------------------------------------------------------------------




# data cleaning ----------------------------------------------------------------




# analysis ---------------------------------------------------------------------



# plot -------------------------------------------------------------------------



# save outputs -----------------------------------------------------------------


# save model output ...

# END --------------------------------------------------------------------------
```
:::



And an example with some simple code:

::: {.cell}

```{.r .cell-code}
# Description ----
# load socio-economic data, and look at the relationship between gdp and 
# life expectancy.

# Set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(gapminder)


# Load data --------------------------------------------------------------------


# load built-in gapminder dataset
gap_df <- gapminder::gapminder

# data cleaning ----------------------------------------------------------------


# get asia only and drop "pop" column
asia_df <- gap_df |>
  filter(continent == "Asia") |>
  filter(year == max(year)) |>
  select(-pop)


# analysis ---------------------------------------------------------------------

# run linear regression
life_exp_lm <- lm(formula = lifeExp ~ gdpPercap, data = asia_df)
summary(life_exp_lm)

# plot -------------------------------------------------------------------------

# plot gdp vs life expectency
p1 <- ggplot(data = asia_df, aes(x = log(gdpPercap),
                                 y = lifeExp,
                                 colour = country)) +
  geom_point() +
  labs(x = "Log(GDP/capita ($))",
       y = "Life expectency (yrs)") +
  theme_bw()

p1

# save outputs -----------------------------------------------------------------

# save figure
ggsave(filename = "./figures/fig1_life_gdp.png")


# save model output ...

# END --------------------------------------------------------------------------
```
:::






Start by installing a couple addins:

-   `lintr` - this package provides an addin that you can use to highlight formatting issues.
-   `styler` - provides an addin to automatically tidy code indentation.
-   [`gapminder`](https://www.gapminder.org) - provides some socioeconomic data to play with.






