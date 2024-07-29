library(knitr)
library(tidyverse)
library(cowplot)
library(fontawesome)
library(kableExtra)
library(countdown)
library(ggrepel)
library(readxl)
library(lubridate)
library(janitor)
library(here)
library(HistData)
library(metathis)
library(viridis)
library(gganimate)
library(magick)
library(waffle)
library(ggplot2)
library(forcats)
# install.packages("devtools")
# devtools::install_github("hadley/emo")
library(emo)
library(ggridges)

options(
    dplyr.print_min = 6,
    dplyr.print_max = 6,
    stringr.view_n = 10,
    pillar.bold = TRUE,
    width = 77 # 80 - 3 for #> comment
)

knitr::opts_chunk$set(
    comment    = "#>",
    fig.retina = 3,
    fig.width  = 6,
    fig.height = 4,
    fig.show = "hold",
    fig.align  = "center",
    fig.path   = "figs/",
    warning = FALSE,
    message = FALSE
)
