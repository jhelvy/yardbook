#
#
#
#| echo: false
#| message: false
#| include: false

suppressWarnings(suppressMessages(source("_common.R")))
theme_set(theme_gray(base_size = 14))

knitr::opts_chunk$set(
  fig.path = "figs/",
  fig.width = 7.252,
  fig.height = 4.5,
  fig.retina = 3, 
  out.width = "70%"
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
path <- here::here("folder", "file.csv")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(here)

csvPath <- here('data', 'milk_production.csv')
milk_production <- read_csv(csvPath)

head(milk_production)
#
#
#
#
#
#
#
#| eval: false

Land-Ocean Temperature Index (C)
--------------------------------

Year No_Smoothing  Lowess(5)
----------------------------
1880     -0.15     -0.08
1881     -0.07     -0.12
1882     -0.10     -0.15
1883     -0.16     -0.19
#
#
#
#
#
txtPath <- here('data', 'nasa_global_temps.txt')
global_temps <- read.table(txtPath, skip = 5, header = FALSE)

head(global_temps)
#
#
#
#
#
#
#
library(readxl)

xlsxPath <- here('data', 'pv_cell_production.xlsx')
pv_cells <- read_excel(xlsxPath, sheet = 'Cell Prod by Country', skip = 2)

head(pv_cells)
#
#
#
#
#
#
#
#
#
#
#
#| label: 'scatter-basic'
#| fig.height: 5
#| fig.width: 6

plot(x = mtcars$hp, y = mtcars$mpg)
#
#
#
#
#
#| label: 'scatter-basic-pretty'
#| fig.height: 5
#| fig.width: 6

plot(
  x    = mtcars$hp,
  y    = mtcars$mpg,
  col  = 'darkblue', # "col" changes the point color
  pch  = 19, # "pch" changes the point shape
  main = "",
  xlab = "Horsepower",
  ylab = "Miles Per Gallon"
)
#
#
#
#
#
#
#
#
#
#| label: 'hist-basic'
#| fig.height: 5
#| fig.width: 6

hist(x = faithful$waiting)
#
#
#
#
#
#| label: 'hist-basic-pretty'
#| fig.height: 5
#| fig.width: 6

hist(
  x      = faithful$waiting,
  breaks = 20,
  col    = 'lightblue3',
  main   = "Waiting Times Between Eruptions",
  xlab   = "Waiting Time (in minutes)",
  ylab   = "Count"
)
#
#
#
#
#
#
#
#
#| label: ggplot-monsters
#| out-width: "500"
#| echo: false
#| fig-align: "center"
#| fig-alt: "Colorful illustration promoting ggplot2 data visualization. The text 'ggplot2: Build a data MASTERPIECE' is prominently displayed. Various framed charts and graphs are shown, including line plots, bar charts, and scatter plots. Three cute, fuzzy cartoon creatures resembling paint brushes or mops are walking across the bottom. They're carrying boxes labeled 'themes', 'geoms', and 'scales'. An easel with a bar chart stands to the right. The overall style is whimsical and artistic, emphasizing the creative aspect of data visualization."

knitr::include_graphics("images/horst_monsters_ggplot2.png")
#
#
#
#
#
#
#
#
#
#
#
#
#| label: ggplot-layers
#| out-width: "600"
#| echo: false
#| fig-align: "center"
#| fig-alt: "Diagram showing the layers of a ggplot2 plot. It has four columns labeled 'Aesthetics', 'Axis', 'Theme', and 'Output'. The 'Aesthetics' column shows a scatter plot with points transitioning from blue to red as they move up and right. The 'Axis' column displays the same plot with labeled axes: 'Variable 1' (x-axis) and 'Variable 2' (y-axis). The 'Theme' column shows a gray rectangle representing the plot background. The 'Output' column combines all elements, showing the scatter plot with axes and a gray background. Below, the columns are labeled 'Layer 1', 'Layer 2', and 'Layer 3' respectively. Plus signs between columns indicate the additive nature of these layers."

knitr::include_graphics("images/ggplot-layers.png")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
head(mpg)
#
#
#
#
#
#| label: ggblank
#| fig.height: 5
#| fig.width: 7

mpg %>% 
  ggplot()
#
#
#
#
#
#
#
#| label: ggaes
#| fig.height: 5
#| fig.width: 7

mpg %>% 
  ggplot(aes(x = displ, y = hwy))
#
#
#
#
#
#
#
#| label: ggpoint
#| fig.height: 5
#| fig.width: 7

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point()
#
#
#
#
#
#
#
#| label: gglabs
#| fig.height: 5
#| fig.width: 7

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  labs(
    x = "Engine displacement (liters)",
    y = "Highway fuel economy (mpg)",
    title = "Most larger engine vehicles are less fuel efficient"
  )
#
#
#
#
#
#
#
#| label: ggtheme_bw
#| fig.height: 5
#| fig.width: 7

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  labs(
    x = "Engine displacement (liters)",  
    y = "Highway fuel economy (mpg)", 
    title = "Most larger engine vehicles are less fuel efficient"
  ) + 
  theme_bw()
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: ggtheme_bw2
#| echo: false
#| fig.height: 4
#| fig.width: 6
#| out-width: "100%"

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  theme_bw()
#
#
#
#
#
#
#
#
#
#| label: ggtheme_minimal
#| fig.height: 4
#| fig.width: 6
#| echo: false
#| out-width: "100%"

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  theme_minimal()
#
#
#
#
#
#
#
#
#
#| label: ggtheme_classic
#| fig.height: 4
#| fig.width: 6
#| echo: false
#| out-width: "100%"

mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  theme_classic()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Format the data frame
wildlife_impacts %>%
  count(operator)
#
#
#
#
#
#
#
#| label: birds_before
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%

# Add geoms
ggplot() +
  geom_col(
    aes(x = operator, y = n),
    width = 0.7, alpha = 0.8
  )
#
#
#
#
#
#
#
#
#
#
#
#| label: birds_coord_flip
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%

# Add geoms
 ggplot() +
  geom_col(
    aes(x = operator, y = n),
    width = 0.7, alpha = 0.8
  ) +

# Flip coordinates
  coord_flip()
#
#
#
#
#
#| label: birds_coord_flip2
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%

# Add geoms
ggplot() +
  geom_col(
    aes(x = n, y = operator),
    width = 0.7, alpha = 0.8
  )
#
#
#
#
#
#
#
#
#
#| label: birds_fct_reorder
#| fig.height: 4
#| fig.width: 6

wildlife_impacts %>%
  count(operator) %>%

# Add geoms
ggplot() +
  geom_col(
    aes(x = n, y = reorder(operator, n)),
    width = 0.7, alpha = 0.8
  )
#
#
#
#
#
#
#
#
#
#| label: birds_scales1
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%

# Add geoms
ggplot() +
  geom_col(
    aes(x = n, y = reorder(operator, n)),
    width = 0.7, alpha = 0.8
  ) +

# Adjust x axis scale
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))
#
#
#
#
#
#| label: birds_scales2
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%

# Add geoms
ggplot() +
  geom_col(
    aes(x = n, y = reorder(operator, n)),
    width = 0.7, alpha = 0.8
  ) +

# Adjust x axis scale
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = c(0, 10000, 20000),
    limits = c(0, 20000)
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: birds_theme
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%

# Add geoms
  ggplot() +
  geom_col(
    aes(x = n, y = reorder(operator, n)),
    width = 0.7, alpha = 0.8
  ) +

# Adjust x axis scale
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +

# Adjust theme
  theme_minimal_vgrid()
#
#
#
#
#
#
#
#| label: birds_annotate
#| fig.height: 4
#| fig.width: 6

# Format the data frame
wildlife_impacts %>%
  count(operator) %>%
  # Make the operator names title case
  mutate(operator = str_to_title(operator)) %>%

# Add geoms
  ggplot() +
  geom_col(
    aes(x = n, y = reorder(operator, n)),
    width = 0.7, alpha = 0.8
  ) +

# Adjust x axis scale
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +

# Adjust theme
  theme_minimal_vgrid() +

# Annotate
  labs(
    x = 'Count',
    y = NULL, 
    title = "Number of wildlife impacts by operator", 
    subtitle = "Top four operating airlines",
    caption = "Data from the FAA Wildlife Strike Database"
  )
#
#
#
#
#
