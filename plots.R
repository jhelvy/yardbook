source("_common.R")

# index ----

# intro ----

# quarto ----

# ggplot ----

# tidy-data ----

# cleaning- ----

# good-practices ----

# summarizing-data ----

# effective-data-viz ----

# A good chunk of this comes from the end come from John Rauser's code for his
# presentation on "How Humans See Data", first presented at Velocity Amsterdam 2016:
# http://conferences.oreilly.com/velocity/devops-web-performance-eu/public/schedule/detail/54354)
# I modified his plots to use the gapminder data instead of mtcars

## Climate change barcode ----

climateChangeBarcode <- read.table(
    here('data', 'nasa_global_temps.txt'),
    col.names = c('year', 'meanTempCelsius', 'smoothTempCelsius'), skip=5) %>%
    mutate(group = "group") %>%
    ggplot(aes(x = group, y = as.factor(year))) +
    geom_tile(aes(fill = meanTempCelsius)) +
    scale_fill_gradientn(colours = rev(brewer.pal(10, "RdBu"))) +
    coord_flip() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.margin = margin(-2, 0, -2, 0, "cm"))

ggsave(here('figs', 'climateChangeBarcode.png'),
       climateChangeBarcode, width = 9, height = 4, dpi = 150)

## Student engagement ----

engagement_data <- data.frame(
    Male   = c(643, 735, 590, 863),
    Female = c(793, 928, 724, 662),
    School = c('Special Ed., Charter', 'Special Ed., Public',
               'General Ed., Charter', 'General Ed., Public'),
    Highlight = c(0, 0, 0, 1)) %>%
    gather(Gender, Engagement, Male:Female) %>%
    mutate(
        Gender = fct_relevel(Gender, c('Male', 'Female')),
        Highlight = as.factor(Highlight),
        x = ifelse(Gender == 'Female', 1, 0))

engagement <- engagement_data %>%
    mutate(Highlight = as.factor(Highlight)) %>%
    ggplot(aes(x = Gender, y = Engagement,
               group = School, color = School)) +
    geom_point() +
    geom_line() +
    theme_cowplot() +
    labs(x = 'Sex', y = 'Engagement')

engagement_labeled <- engagement_data %>%
    mutate(Highlight = as.factor(Highlight)) %>%
    ggplot(aes(x = x, y = Engagement, group = School, color = School)) +
    geom_point() +
    geom_line() +
    labs(x = 'Sex', y = 'Engagement') +
    theme_cowplot() +
    scale_x_continuous(limits = c(-0.2, 2), labels = c('Male', 'Female'),
                       breaks = c(0, 1)) +
    theme(legend.position = 'none') +
    geom_text_repel(aes(label = School, color = as.factor(School)),
                    data          = subset(engagement_data, Gender == 'Female'),
                    size          = 5,
                    nudge_x       = 0.2,
                    hjust         = 0,
                    segment.color = NA)

engagement_final <- engagement_data %>%
    ggplot(aes(x = x, y = Engagement, group = School, color = Highlight)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c('#757575', '#ed573e')) +
    labs(x = 'Sex', y = 'Engagement',
         title = paste0('Female students in public, general education\n',
                        'schools have surprisingly low engagement')) +
    scale_x_continuous(limits = c(-1.2, 1.2), labels = c('Male', 'Female'),
                       breaks = c(0, 1)) +
    geom_text_repel(aes(label = Engagement, color = as.factor(Highlight)),
                    data          = subset(engagement_data, Gender == 'Female'),
                    size          = 5,
                    nudge_x       = 0.1,
                    segment.color = NA) +
    geom_text_repel(aes(label = Engagement, color = as.factor(Highlight)),
                    data          = subset(engagement_data, Gender == 'Male'),
                    size          = 5,
                    nudge_x       = -0.1,
                    segment.color = NA) +
    geom_text_repel(aes(label = School, color = as.factor(Highlight)),
                    data          = subset(engagement_data, Gender == 'Male'),
                    size          = 5,
                    nudge_x       = -0.25,
                    hjust         = 1,
                    segment.color = NA) +
    theme_cowplot() +
    background_grid(major = 'x') +
    theme(axis.line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none')

ggsave(here('figs', 'engagement.png'),
       engagement, width = 5, height = 4)
ggsave(here('figs', 'engagement_labeled.png'),
       engagement_labeled, width = 5, height = 4)
ggsave(here('figs', 'engagement_final.png'),
       engagement_final, width = 6, height = 5)

## Milk region ----

milk_production  <- read_csv(here::here('data', 'milk_production.csv'))

milk_region <- milk_production %>%
    filter(region %in% c(
        'Pacific', 'Northeast', 'Lake States', 'Mountain')) %>%
    group_by(year, region) %>%
    summarise(milk_produced = sum(milk_produced)) %>%
    ungroup() %>%
    mutate(label = ifelse(year == max(year), region, NA))

milk_region <- ggplot(milk_region,
    aes(x = year, y = milk_produced,
        color = region)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
        'sienna', 'forestgreen', 'dodgerblue', 'orange')) +
    theme_half_open(font_size = 18) +
    labs(x = 'Year',
         y = 'Milk produced (billion lbs)',
         color = 'Region',
         title = 'Milk production in four US regions')

milk_region_label <- milk_region +
    geom_text(aes(label = label),
              hjust = 0, nudge_x = 1,
              size = 6) +
    coord_cartesian(clip = 'off') +
    theme_half_open(font_size = 18) +
    theme(legend.position = 'none',
          plot.margin = margin(0.1, 2.7, 0.1, 0.1, "cm")) +
    labs(x = 'Year',
         y = 'Milk produced (billion lbs)',
         title = 'Milk production in four US regions')

ggsave(here('figs', 'milk_region.png'),
       milk_region, width = 7, height = 4.5)
ggsave(here('figs', 'milk_region_label.png'),
       milk_region_label, width = 8, height = 4.5)

## Background Checks ----

backgroundChecks <- data.frame(
    gunOwner       = c('Yes', 'Yes', 'No', 'No'),
    supportPolicy  = c('Yes', 'No', 'Yes', 'No'),
    count          = c(113, 23, 174, 9)) %>%
    mutate(percent = 100*(count / sum(count))) %>%
    ggplot(aes(x = supportPolicy, y = percent, fill = gunOwner)) +
    geom_col(position = 'stack', width = 0.7) +
    scale_fill_manual(values = c('orange', 'dodgerblue')) +
    scale_y_continuous(limits = c(0, 100),
                       expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid() +
    labs(
        x = 'Do you support universal background checks?',
        y = 'Percent of Americans',
        fill = 'Owns gun',
        title = paste0('The vast majority of Americans',
                       '\nsupport universal background checks,\n',
                       'including gun owners'))

ggsave(here('figs', 'backgroundChecks.png'),
       backgroundChecks, width = 5, height = 5)


## Anscombe Plots ----

x       <- c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)
y       <- c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
quartet <- rep(c('A', 'B', 'C', 'D'), each = nrow(anscombe))
ansDf   <- tibble(x, y, quartet)
anscombePlot <- ansDf %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(alpha = 0.6, size = 2) +
    stat_smooth(method = 'lm', se = F, size = 0.4, alpha = 0.6) +
    facet_wrap(vars(quartet)) +
    scale_x_continuous(breaks=seq(4, 18, 2), limits=c(4, 19)) +
    scale_y_continuous(breaks=seq(4, 12, 2), limits=c(3, 13)) +
    theme_bw() +
    theme(
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))

ggsave(here('figs', 'anscombePlot.png'),
       anscombePlot, width = 8, height = 6)

# Print out table of data
write_csv(anscombe, here('figs', 'anscombe.csv'))

# Graph quality over time ----

graphQuality <- data.frame(
    x = c(1600, 1700, 1800, 1900, 1980, 2020),
    y = c(20, 100, 200, 300, 400, 40)) %>%
    ggplot() +
    geom_line(aes(x = x, y = y), size = 1.5) +
    theme_minimal_hgrid() +
    labs(x = 'Year', y = 'Graph quality', title = 'Graphing quality over time')

ggsave(here('figs', 'graphQuality.png'),
       graphQuality, width = 6, height = 3.5)

## mammals ----

mammalsScatter <- mammals %>%
    ggplot(aes(x = body, y = brain)) +
    geom_point(alpha = 0.6) +
    theme_bw() +
    scale_x_log10() +
    scale_y_log10() +
    stat_smooth(method = 'lm', col = 'red', se = F, size = 0.7) +
    labs(x = 'log(body weight)', y = 'log(brain weight)')

ggsave(here('figs', 'mammalsScatter.png'),
       mammalsScatter, width = 5, height = 4)

## Mtcars scatterplot ----

mtCarsScatter <- mtcars %>%
    ggplot(aes(x = mpg, y = hp)) +
    geom_point() +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterSmooth <- mtCarsScatter +
    geom_smooth()

mtCarsScatterGradient <- mtcars %>%
    ggplot(aes(x = wt, y = hp)) +
    geom_point(aes(color = mpg), size = 1.5) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot()

mtCarsScatterColor <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, color = as.factor(cyl))) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterShape1 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, shape = as.factor(cyl))) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(shape = 'Cylinders')

mtCarsScatterShape2 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, shape = as.factor(cyl))) +
    scale_shape_manual("Cylinders", values = c(1,2,3)) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(shape = 'Cylinders')

mtCarsScatterShape3 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp, shape = as.factor(cyl))) +
    scale_y_continuous(limits = c(0, 340)) +
    scale_shape_manual(values = c(52,54,56)) +
    theme_cowplot() +
    labs(shape = 'Cylinders')

mtCarsScatterGray <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), color = 'gray80') +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterSmall <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), size = 0.3) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders')

mtCarsScatterGrid1 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp)) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(panel.grid.major = element_line(colour = "grey20", size = 0.2))

mtCarsScatterGrid2 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp)) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        panel.border     = element_rect(fill = NA),
        panel.grid.minor = element_line(size = 0.2),
        panel.grid.major = element_line(size = 0.5))

mtCarsScatterGrid3 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp)) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        panel.border     = element_rect(fill = NA),
        panel.grid.minor = element_line(size = 0.5),
        panel.grid.major = element_line(size = 1))

mtCarsScatterGrid4 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), size = NA) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        panel.border     = element_rect(fill = NA),
        panel.grid.major = element_line(size = 5))

mtCarsScatterGrid5 <- mtcars %>%
    ggplot() +
    geom_point(aes(x = mpg, y = hp), alpha = 0.7) +
    scale_y_continuous(limits = c(0, 340)) +
    theme_cowplot() +
    labs(color = 'Cylinders') +
    theme(
        axis.line = element_line(color = "grey70"),
        axis.ticks = element_line(color = "grey70"),
        axis.text = element_text(color = "grey20"),
        axis.title = element_text(color = "grey20"),
        panel.border     = element_rect(fill = NA, colour = "grey70"),
        panel.grid.major = element_line(colour = "grey80", size = 0.2))

mtCarsScatterLabels <- mtCarsScatter +
    geom_mark_ellipse(aes(fill = as.factor(cyl),
                          label = paste0(cyl, 'Cylinders'))) +
    geom_point() +
    scale_y_continuous(limits = c(0, 340)) +
    theme(legend.position = 'none')

ggsave(here('figs', 'mtCarsScatter.png'),
       mtCarsScatter, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterSmooth.png'),
       mtCarsScatterSmooth, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGradient.png'),
       mtCarsScatterGradient, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterColor.png'),
       mtCarsScatterColor, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterShape1.png'),
       mtCarsScatterShape1, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterShape2.png'),
       mtCarsScatterShape2, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterShape3.png'),
       mtCarsScatterShape3, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterSmall.png'),
       mtCarsScatterSmall, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGray.png'),
       mtCarsScatterGray, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGrid1.png'),
       mtCarsScatterGrid1, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGrid2.png'),
       mtCarsScatterGrid2, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGrid3.png'),
       mtCarsScatterGrid3, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGrid4.png'),
       mtCarsScatterGrid4, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterGrid5.png'),
       mtCarsScatterGrid5, width = 4, height = 3)
ggsave(here('figs', 'mtCarsScatterLabels.png'),
       mtCarsScatterLabels, width = 7, height = 5)

## Monster bars ----

monsterBars <- data.frame(
    year  = c(1972, 1974, 1976, 1978, 1980, '1982\nest.'),
    y = c(70, 85, 125, 200, 240, 310)) %>%
    ggplot() +
    geom_col(aes(x = year, y = y), width = 0.7) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid() +
    labs(x = 'Year', y = 'Campaign expenditures ($ mil)',
         title = 'Total House and Senate\ncampaign expenditures')

ggsave(here('figs', 'monsterBars.png'),
       monsterBars, width = 4.5, height = 4)

## Preattentive color vs. shape ----

plotTheme <- function() {
    theme_bw() +
    theme(
        legend.position = 'none',
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))
}

# Create function to generate the data so that each plot has different points
makePlotData <- function() {
    plotData <- tibble(
        x      = runif(60, 0, 10),
        y      = runif(60, 0, 10),
        color  = c(rep('2', 59), '1'),
        group1 = c(rep('2', 20), rep('2', 20), rep('1', 19), '1'),
        group2 = c(rep('2', 20), rep('1', 20), rep('2', 19), '1'))
    return(plotData)
}

preattentive1 <- makePlotData() %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = color), size = 2) +
    scale_color_manual(values = c('red', 'blue')) +
    plotTheme() +
    labs(x = '', y = '')

preattentive2 <- makePlotData() %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(shape = color), color = 'red', , size = 2) +
    plotTheme() +
    labs(x = '', y = '')

preattentive3 <- makePlotData() %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(shape = group2, color = group1), size = 2) +
    scale_color_manual(values = c('red', 'blue')) +
    plotTheme() +
    labs(x = '', y = '')

ggsave(here('figs', 'preattentive1.png'),
    preattentive1, width = 4, height = 4)
ggsave(here('figs', 'preattentive2.png'),
    preattentive2, width = 4, height = 4)
ggsave(here('figs', 'preattentive3.png'),
    preattentive3, width = 4, height = 4)

## Preattentive dots ----

preattentiveData <- data.frame(
    x        = rnorm(100),
    y        = rnorm(100),
    category = rep(letters[1:10], each = 10))

preattentive_bad <- preattentiveData %>%
    ggplot() +
    geom_point(aes(x = x, y = y, color = category), size = 1) +
    theme_bw()

preattentive_good <- preattentiveData %>%
    ggplot() +
    geom_point(aes(x = x, y = y), size = 1) +
    theme_bw() +
    facet_wrap(vars(category))

ggsave(here('figs', 'preattentive-bad.png'),
       preattentive_bad, width = 4, height = 3)
ggsave(here('figs', 'preattentive-good.png'),
       preattentive_good, width = 5, height = 5)

## faceted bars -----

facetedBars <- data.frame(
    count  = c(5, 12, 14, 7, 13, 17, 8, 18, 19),
    type = c(rep(paste('type', seq(3)), 3)),
    group = c(rep('group 1', 3), rep('group 2', 3), rep('group 3', 3))) %>%
    ggplot() +
    geom_col(aes(x = type, y = count)) +
    facet_grid(group ~ .) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    theme_bw()

ggsave(here('figs', 'facetedBars.png'),
       facetedBars, width = 7, height = 5)

## simple bars -----

simpleBars <- data.frame(
    count  = seq(5),
    group = c(LETTERS[1:5])) %>%
    ggplot() +
    geom_col(aes(x = group, y = count), width = 0.7) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid()

ggsave(here('figs', 'simpleBars.png'),
       simpleBars, width = 4.5, height = 4)

## mpg bars ----

mpg_bars <- mpg %>%
    ggplot() +
    geom_bar(aes(class), width = 0.8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_cowplot()
mpg_bars_flipped <- mpg_bars +
    coord_flip()
mpg_bars_flipped_sorted <- mpg %>%
    group_by(class) %>%
    summarise(count = n()) %>%
    mutate(class = fct_reorder(class, count)) %>%
    ggplot() +
    geom_col(aes(x=class, y=count), width = 0.8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    theme_cowplot()

ggsave(here('figs', 'mpg_bars.png'),
       mpg_bars, width = 6, height = 5)
ggsave(here('figs', 'mpg_bars_bad.png'),
       mpg_bars, width = 5, height = 5)
ggsave(here('figs', 'mpg_bars_flipped.png'),
       mpg_bars_flipped, width = 6, height = 4)
ggsave(here('figs', 'mpg_bars_flipped_sorted.png'),
       mpg_bars_flipped_sorted, width = 6, height = 4)

## Bar chart at zero ----

barchart_zero <- data.frame(
    x = c('March 27', 'March 31 Goal'),
    y = c(6, 7.066)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_col(fill = 'blue', width = 0.7) +
    theme_minimal_hgrid() +
    labs(x= '', y = 'Millions of enrollments') +
    scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0,8),
                       expand = expansion(mult = c(0, 0.05)))

ggsave(here('figs', 'barchart_zero.png'),
       barchart_zero, width = 3, height = 4)

## Pies ----

statesData <- data.frame(
    area = state.area,
    region = state.region) %>%
    group_by(region) %>%
    summarise(area = sum(area)) %>%
    mutate(region = fct_reorder(region, area)) %>%
    arrange(desc(region)) %>%
    mutate(
        prop = area / sum(area) *100,
        ypos = cumsum(prop)- 0.5*prop)

# Basic piechart
statesPie <- ggplot(statesData, aes(x = "", y = prop, fill = region)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = region), color = "white", size=5) +
  scale_fill_brewer(palette = "Spectral")

statesBar <- statesData %>%
    ggplot(aes(x = region, y = area, fill = region)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    theme_minimal_vgrid() +
    labs(x = 'Region', y = 'Area (Sq. Miles)') +
    theme(legend.position = 'none')

ggsave(here('figs', 'statesPie.png'),
    statesPie, width = 5, height = 4)
ggsave(here('figs', 'statesBar.png'),
    statesBar, width = 5, height = 4)

## Color blind ----

colorBlindData <- data.frame(
    x = runif(30, -1, 1),
    y = runif(30, -1, 1),
    group = rep(c('A', 'B'), 15))
colorBlindBad1 <- colorBlindData %>%
    ggplot() +
    geom_point(aes(x = x, y = y, color = group), size = 3) +
    scale_color_manual(values = c('red', 'green')) +
    theme_cowplot()
colorBlindBad2 <- colorBlindBad1 +
    scale_color_manual(values = c('brown', 'brown'))
colorBlindBad3 <- colorBlindData %>%
    ggplot() +
    geom_point(aes(x = x, y = y), size = 3) +
    facet_wrap(vars(group)) +
    theme_bw()

ggsave(here('figs', 'colorBlindBad1.png'),
       colorBlindBad1, width = 5, height = 4)
ggsave(here('figs', 'colorBlindBad2.png'),
       colorBlindBad2, width = 5, height = 4)
ggsave(here('figs', 'colorBlindBad3.png'),
       colorBlindBad3, width = 5, height = 4)

## Prisoner bars ----

prisoner_bars <- data.frame(
    category = c('Violent', 'Property', 'Drug', 'Public Order', 'Other'),
    rate = c(61.7, 73.8, 66.7, 62.2, 64.7)) %>%
    ggplot() +
    geom_point(aes(x = fct_reorder(category, rate), y = rate)) +
    geom_hline(yintercept = 67.5, linetype = 'dashed') +
    scale_y_continuous(breaks = seq(60, 75, 5), limits = c(60, 75),
                       expand = expansion(mult = c(0, 0.05))) +
    annotate('text', label = 'All released prisoners',
             x = 5, y = 67.7, hjust = 0) +
    coord_flip() +
    theme_minimal_vgrid() +
    labs(
        x = 'Offense category',
        y = 'Recidivism rate (%)',
        title = 'Recidivism rate of prisoners released in 1994')

ggsave(here('figs', 'prisoner_bars.png'),
       prisoner_bars, width = 7, height = 4)


## Hauser plots (Asia) ----

asia <- read_csv(here('data', 'gapminder.csv')) %>%
    filter(continent == "Asia") %>%
    filter(year == max(year)) %>%
    # Removing a "middle" country so that it's just 32 countries
    filter(country != "Sri Lanka") %>%
    mutate(country = ifelse(country == "Korea, Rep.", "Korea", country)) %>%
    arrange(lifeExp)

jr_theme <- function() {
    theme(
        axis.title       = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.border     = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background  = element_blank(),
        legend.key       = element_blank(),
        legend.background = element_rect(colour = NA))
}

make_segment <- function(df, scale_low, scale_high) {
    scale_range <- scale_high-scale_low
    fraction <- (df$lifeExp-scale_low) / scale_range
    theta <- fraction * pi/2
    return(data.frame(x=c(0, cos(theta)),
                      y=c(0, sin(theta))))
}

jr_hue <- ggplot(asia, aes(x = "", y = country, fill = lifeExp)) +
    geom_tile() +
    scale_fill_viridis() +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    jr_theme()

jr_hue_ordered <- ggplot(asia, aes(x = "", y = reorder(country, lifeExp), fill = lifeExp)) +
    geom_tile() +
    scale_fill_viridis() +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    jr_theme()

jr_saturation <- ggplot(asia, aes(x = "", y = country, fill = lifeExp)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = muted("blue")) +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    ylab(NULL) +
    jr_theme()

jr_saturation_ordered <- ggplot(asia, aes(x = "", y = reorder(country, lifeExp), fill = lifeExp)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = muted("blue")) +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    ylab(NULL) +
    jr_theme()

jr_saturation_ordered_zero <- ggplot(asia, aes(x = "", y = reorder(country, lifeExp), fill = lifeExp)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = muted("blue")) +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    ylab(NULL) +
    jr_theme() +
    expand_limits(fill=0)

jr_area <- ggplot(arrange(asia, lifeExp, country),
                  aes(rep(1:4, 32/4), rep(1:(32/4), each=4))) +
    geom_point(aes(size=lifeExp)) +
    geom_text(aes(label=country),
              position=position_nudge(y=0.5), size=2) +
    expand_limits(x=c(0.5,4.5), y=c(1,8.5)) +
    ylab(NULL) +
    scale_size_area() +
    jr_theme() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank())

jr_angle <- asia %>%
    arrange(lifeExp, country) %>%
    group_by(country) %>%
    do(make_segment(., 0, max(asia$lifeExp))) %>%
    ungroup() %>%
    mutate(country = fct_reorder(country, y)) %>%
    ggplot(aes(x, y, group=country)) +
    geom_path() +
    facet_wrap(~country,ncol=8) +
    coord_equal() +
    jr_theme() +
    theme(
        strip.text.x = element_text(size=rel(0.5), hjust=0),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

jr_length <- asia %>%
    mutate(random = runif(32, 0, 5)) %>%
    ggplot(aes(x=random, xend=lifeExp+random, y=country, yend=country)) +
    geom_segment(size=3) +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    xlab(NULL) +
    ylab(NULL) +
    jr_theme() +
    theme(axis.text.y=element_text(size=rel(0.7)),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

jr_bars <- asia %>%
    ggplot(aes(x=0, xend=lifeExp, y=country, yend=country)) +
    geom_segment(size=3) +
    scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
    xlab(NULL) +
    ylab(NULL) +
    jr_theme() +
    theme(axis.text.y=element_text(size=rel(0.7)),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

jr_position_non_aligned <- asia %>%
    mutate(country = fct_reorder(country, lifeExp)) %>%
    ggplot(aes(x=lifeExp, y="a")) +
    geom_point(size=1) +
    scale_x_continuous(breaks=c(10, 20, 30)) +
    facet_wrap(~country, ncol=8) +
    jr_theme() +
    theme(
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size=rel(0.5), hjust=0),
        panel.grid.major = element_line(colour = "grey50", size = 0.2),
        panel.grid.minor = element_line(colour = "grey50", size = 0.2, linetype="dotted"))

jr_position_aligned <- asia %>%
    ggplot(aes(x=lifeExp, y=country)) +
    geom_point() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    ylab(NULL) +
    theme_minimal_vgrid() +
    theme(
        axis.text.y=element_text(size=rel(0.7)),
        axis.title.y=element_text(size=rel(0.7)),
        panel.grid.major =   element_line(colour = "grey20", size = 0.2))

jr_position_aligned_ordered <- asia %>%
    mutate(country = fct_reorder(country, lifeExp)) %>%
    ggplot(aes(x=lifeExp, y=country)) +
    geom_point() +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    theme_minimal_vgrid() +
    ylab(NULL) +
    theme(
        axis.text.y=element_text(size=rel(0.7)),
        axis.title.y=element_text(size=rel(0.7)))

jr_position_aligned_ordered_zero <- jr_position_aligned_ordered +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    expand_limits(x=0)

jr_position_aligned_lollipop <- asia %>%
    mutate(country = fct_reorder(country, lifeExp)) %>%
    ggplot(aes(x=lifeExp, y=country)) +
    geom_point() +
    geom_segment(aes(x=0, xend=lifeExp, yend=country)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid() +
    ylab(NULL) +
    theme(
        axis.text.y=element_text(size=rel(0.7)),
        axis.title.y=element_text(size=rel(0.7)))

jr_diamonds_bars_stacked <- ggplot(diamonds, aes(clarity, fill=cut, group=cut)) +
    geom_bar(stat="count", position="stack") +
    scale_y_continuous("Count", labels = comma,
                       expand = expansion(mult = c(0, 0.05))) +
    theme_cowplot()

jr_diamonds_bars_dodged <- ggplot(diamonds, aes(clarity, fill=cut, group=cut)) +
    geom_bar(stat="count", position="dodge") +
    scale_y_continuous("Count", labels = comma,
                       expand = expansion(mult = c(0, 0.05))) +
    theme_cowplot()

jr_diamonds_line_total <- diamonds %>%
    group_by(clarity) %>%
    summarize(n=length(clarity)) %>%
    ggplot(aes(clarity, n)) +
    geom_line(group=1) +
    geom_point() +
    expand_limits(y=0) +
    scale_y_continuous("Count", labels=comma) +
    theme_cowplot()

jr_diamonds_line_cut <- diamonds %>%
    group_by(clarity, cut) %>%
    summarize(n=length(clarity)) %>%
    ggplot(aes(clarity, n, color=cut, group=cut)) +
    geom_line() +
    geom_point() +
    expand_limits(y=0) +
    scale_y_continuous("Count", labels=comma) +
    theme_cowplot()

# Phone data from wikipedia: https://en.wikipedia.org/wiki/Mobile_operating_system
phone_data<-
    "Quarter	Windows Mobile	RIM	Symbian	iOS	Android	Bada	Windows Phone	Other
2016 Q2[39]	0	400.4	0	44395	296912	0	1971	680.6
2016 Q1[40]	0	660	0	51630	293771	0	2400	791
2015 Q4[41]	0	906.9	0	71526	325394	0	4395	887.3
2015 Q3[42]	0	977	0	46062	298797	0	5874	1133.6
2015 Q2[43]	0	1153	0	48086	271010	0	8198	1229
2015 Q1[44]	0	1325	0	60177	265012	0	8271	1268
2014 Q4[45]	0	1734	0	74832	279058	0	10425	1286.9
2014 Q3[46]	0	2420	0	38187	254354	0	9033	1310.2
2014 Q2[47]	0	2044	0	35345	243484	0	8095	2044
2014 Q1[44]	0	1714	0	43062	227549	0	7580	1371
2013 Q4[48]	0	1807	0	50224	219613	0	8534	1994
2013 Q3[49]	0	4401	458	30330	205023	633	8912	475
2013 Q2[50]	0	6180	631	31900	177898	838	7408	472
2013 Q1[51]	0	6219	1349	38332	156186	1371	5989	600
2012 Q4[52]	0	7333	2569	43457	144720	2684	6186	713
2012 Q3[53]	0	8947	4405	23550	122480	5055	4058	684
2012 Q2[54]	0	7991	9072	28935	98529	4209	4087	863
2012 Q1[55]	0	9939	12467	33121	81067	3842	2713	1243
2011 Q4[56]	0	13185	17458	35456	75906	3111	2759	1167
2011 Q3[57]	0	12701	19500	17295	60490	2479	1702	1018
2011 Q2[58]	0	12652	23853	19629	46776	2056	1724	1051
2011 Q1	982	13004	27599	16883	36350	1862	1600	1495
2010 Q4[56]	3419	14762	32642	16011	30801	2027	0	1488
2010 Q3[57]	2204	12508	29480	13484	20544	921	0	1991
2010 Q2[58]	3059	11629	25387	8743	10653	577	0	2011
2010 Q1[59]	3696	10753	24068	8360	5227	0	0	2403
2009 Q4[60]	4203	10508	23857	8676	4043	0	0	2517
2009 Q3[61]	3260	8523	18315	7040	1425	0	0	2531
2009 Q2[62]	3830	7782	20881	5325	756	0	0	2398
2009 Q1[63]	3739	7534	17825	3848	575	0	0	2986
2008 Q4[64]	4714	7443	17949	4079	639	0	0	3319
2008 Q3[65]	4053	5800	18179	4720	0	0	0	3763
2008 Q2[66]	3874	5594	18405	893	0	0	0	3456
2008 Q1[64]	3858	4312	18400	1726	0	0	0	4113
2007 Q4[64]	4374	4025	22903	1928	0	0	0	3536
2007 Q3[65]	4180	3192	20664	1104	0	0	0	3612
2007 Q2[66]	3212	2471	18273	270	0	0	0	3628
2007 Q1[64]	2931	2080	15844	0	0	0	0	4087"
phones <- read.delim(textConnection(phone_data))
phones <- phones %>%
    mutate(Other = Other + Bada + Windows.Phone,
           Bada = NULL,
           Windows.Phone = NULL,
           Quarter = str_replace(Quarter, "\\[\\d+\\]", ""),
           Year = as.integer(str_split_fixed(Quarter, " ", 2)[,1]),
           Quarter = str_split_fixed(Quarter, " ", 2)[,2],
           qtr = as.integer(str_replace(Quarter, "Q", "")),
           year = Year+0.25*(qtr-1)) %>%
    gather(os, ct, -Year, -Quarter, -qtr, -year) %>%
    mutate(ct = as.numeric(ct)) %>%
    group_by(year) %>%
    mutate(share = ct/sum(ct))

jr_phones_area <- ggplot(phones, aes(year, share, group=os, fill=os)) +
    geom_area() +
    theme_cowplot()

jr_phones_line <- ggplot(phones, aes(year, share, group=os, color=os)) +
    geom_line(size = 1) +
    theme_cowplot()

circleData <- read.delim(textConnection(
    "x	y
1.972	1.236
1.112	1.994
0	1.009
0.665	1.942
0.235	0.356
0.247	1.658
1.275	1.961
0.702	0.045
1.76	0.35
1.691	0.277
1.628	1.778
1.957	1.29
0.111	0.542
0.902	0.005
0.598	0.085
1.613	1.79
1.298	1.955
0.651	1.937
1.949	1.316
0.099	0.567
0.862	0.01
0.027	0.768
0.706	1.956
1.042	1.999"))

jr_circle <- ggplot(circleData, aes(x, y)) +
    geom_point(size = 2) +
    coord_equal() +
    theme_cowplot()

ggsave(here('figs', 'jr_hue.png'),
       jr_hue, width = 5, height = 4)
ggsave(here('figs', 'jr_hue_ordered.png'),
       jr_hue_ordered, width = 5, height = 4)
ggsave(here('figs', 'jr_saturation.png'),
       jr_saturation, width = 5, height = 4)
ggsave(here('figs', 'jr_saturation_ordered.png'),
       jr_saturation_ordered, width = 5, height = 4)
ggsave(here('figs', 'jr_saturation_ordered_zero.png'),
       jr_saturation_ordered_zero, width = 5, height = 4)
ggsave(here('figs', 'jr_area.png'),
       jr_area, width = 5, height = 4)
ggsave(here('figs', 'jr_angle.png'),
       jr_angle, width = 5, height = 4)
ggsave(here('figs', 'jr_length.png'),
       jr_length, width = 5, height = 4)
ggsave(here('figs', 'jr_bars.png'),
       jr_bars, width = 5, height = 4)
ggsave(here('figs', 'jr_position_non_aligned.png'),
       jr_position_non_aligned, width = 5, height = 4)
ggsave(here('figs', 'jr_position_aligned.png'),
       jr_position_aligned, width = 5, height = 4)
ggsave(here('figs', 'jr_position_aligned_ordered.png'),
       jr_position_aligned_ordered, width = 5, height = 4)
ggsave(here('figs', 'jr_position_aligned_ordered_zero.png'),
       jr_position_aligned_ordered_zero, width = 5, height = 4)
ggsave(here('figs', 'jr_position_aligned_lollipop.png'),
       jr_position_aligned_lollipop, width = 5, height = 4)
ggsave(here('figs', 'jr_diamonds_bars_stacked.png'),
       jr_diamonds_bars_stacked, width = 6, height = 4)
ggsave(here('figs', 'jr_diamonds_bars_dodged.png'),
       jr_diamonds_bars_dodged, width = 6, height = 4)
ggsave(here('figs', 'jr_diamonds_line_total.png'),
       jr_diamonds_line_total, width = 6, height = 4)
ggsave(here('figs', 'jr_diamonds_line_cut.png'),
       jr_diamonds_line_cut, width = 6, height = 4)
ggsave(here('figs', 'jr_phones_area.png'),
       jr_phones_area, width = 7, height = 4)
ggsave(here('figs', 'jr_phones_line.png'),
       jr_phones_line, width = 7, height = 4)
ggsave(here('figs', 'jr_circle.png'),
       jr_circle, width = 5, height = 5)

## Making a GOOD plot ----

wildlife_impacts <- read_csv(here('data', 'wildlife_impacts.csv'))

birds_before <- wildlife_impacts %>%
  count(operator) %>%
  ggplot() +
  geom_col(aes(x = operator, y = n),
           width = 0.7, alpha = 0.8)

birds_coord_flip <- birds_before +
  coord_flip()

birds_fct_reorder <- wildlife_impacts %>%
  count(operator) %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(operator, n)),
           width = 0.7, alpha = 0.8)

birds_scales1 <- birds_fct_reorder +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)))

birds_scales2 <- birds_fct_reorder +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = c(0, 10000, 20000),
    limits = c(0, 20000))

birds_theme <- birds_scales1 +
  theme_minimal_vgrid()

birds_after <- wildlife_impacts %>%
  count(operator) %>%
  mutate(operator = str_to_title(operator)) %>%
  ggplot() +
  geom_col(aes(x = reorder(operator, n), y = n),
           width = 0.7, alpha = 0.8) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  theme_minimal_vgrid() +
  labs(x = NULL,
    y = 'Count' )

df <- data.frame(x = seq(5), y = seq(5))
half_open <- ggplot(df, aes(x = x, y = y)) +
  theme_half_open() +
  labs(title = "theme_half_open()")
minimal_grid <- ggplot(df, aes(x = x, y = y)) +
  theme_minimal_grid() +
  labs(title = "theme_minimal_grid()")
minimal_vgrid <- ggplot(df, aes(x = x, y = y)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(title = "theme_minimal_vgrid()")
minimal_hgrid <- ggplot(df, aes(x = x, y = y)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_hgrid() +
  labs(title = "theme_minimal_hgrid()")
cowplot_themes <- plot_grid(half_open, minimal_grid, minimal_hgrid, minimal_vgrid, nrow=1)

ggsave(here('figs', 'birds_before.png'),
       birds_before, width = 4, height = 3.5)
ggsave(here('figs', 'birds_coord_flip.png'),
       birds_coord_flip, width = 6, height = 4)
ggsave(here('figs', 'birds_fct_reorder.png'),
       birds_fct_reorder, width = 6.5, height = 4)
ggsave(here('figs', 'birds_scales1.png'),
       birds_scales1, width = 6.5, height = 4)
ggsave(here('figs', 'birds_scales2.png'),
       birds_scales2, width = 6.5, height = 4)
ggsave(here('figs', 'birds_theme.png'),
       birds_theme, width = 6.5, height = 4)
ggsave(here('figs', 'birds_after.png'),
       birds_after, width = 7, height = 3.5)
ggsave(here('figs', 'cowplot_themes.png'),
       cowplot_themes, width = 15, height = 4)

# trends ----

# amounts ----

# prop ----

# comparisons ----

# maps ----

# Read in data
milk_production <- read_csv(here::here('data', 'milk_production.csv'))
internet_users  <- read_csv(here::here('data', 'internet_users_country.csv'))
us_coffee_shops <- read_csv(here::here('data', 'us_coffee_shops.csv'))

# Filter out coffee shops to continental 48 states
us_coffee_shops <- us_coffee_shops %>%
  filter(lat > 22,    lat < 50,
         long > -150, long < -66)

# Load state_abbs data frame, containing state abbreviations 
source(here::here('data', 'state_abbs.R'))

# Projections 

# Albers
# "ESRI:102003"

# Robinson
# "ESRI:54030"
# "+proj=robin"

# https://ggplot2-book.org/maps.html

# Polygon maps -----------------------------------------------------
# https://www.r-graph-gallery.com/map.html

# Polygon maps
world <- map_data("world")
polygon_world <- ggplot(world,
                        aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "grey60")

ggsave(here::here('images', 'plots', 'polygon_world.png'),
       polygon_world, width = 6, height = 3.7)

us_states <- map_data("state")
polygon_us <- ggplot(us_states,
                     aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "grey60")

ggsave(here::here('images', 'plots', 'polygon_us.png'),
       polygon_us, width = 6, height = 3.7)

# Simple features maps -----------------------------------------------------

# World
world <- ne_countries(scale = "medium", returnclass = "sf")

sf_world <- ggplot(data = world) +
  geom_sf(fill = 'grey90', color = 'grey60')

ggsave(here::here('images', 'plots', 'sf_world.png'),
       sf_world, width = 6, height = 3.7)

# ggplot(data = world) +
#     geom_sf(aes(fill = pop_est)) +
#     scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# US States
us_states <- ne_states(
  country = 'united states of america',
  returnclass = 'sf')

sf_us <- ggplot(data = us_states) +
  geom_sf(fill = 'grey90', color = 'grey60')

ggsave(here::here('images', 'plots', 'sf_us.png'),
       sf_us, width = 6, height = 3.7)

# US States, continental
us_states_cont <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

sf_us_cont <- ggplot(data = us_states_cont) +
  geom_sf(fill = 'grey90', color = 'grey60')

ggsave(here::here('images', 'plots', 'sf_us_cont.png'),
       sf_us_cont, width = 6, height = 3.7)

# US with Hawaii and Alaska

library(tigris)

us_sf <- tigris::states(class = "sf", cb = TRUE) %>%
  shift_geometry() %>%
  filter(GEOID < 60)

us_sf %>%
  ggplot() +
  geom_sf()

ggsave(here::here('images', 'plots', 'sf_us_alhi.png'),
       width = 6, height = 3.7)

# US Counties
library(maps)

us_counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

sf_us_counties <- ggplot(data = us_counties) +
  geom_sf(fill = 'grey90', color = 'grey60')

ggsave(here::here('images', 'plots', 'sf_us_counties.png'),
       sf_us_counties, width = 6, height = 3.7)

sf_us_counties_albers <- ggplot(data = us_counties) +
  geom_sf(fill = 'grey90', color = 'grey60') +
  coord_sf(crs = "ESRI:102003")

ggsave(here::here('images', 'plots', 'sf_us_counties_albers.png'),
       sf_us_counties_albers, width = 6, height = 3.7)

# China
china <- ne_states(
  country = 'china',
  returnclass = 'sf')

sf_china <- ggplot(data = china) +
  geom_sf(fill = 'grey90', color = 'grey60')

ggsave(here::here('images', 'plots', 'sf_china.png'),
       sf_china, width = 6, height = 3.7)

# Africa
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world %>% 
  filter(continent == 'Africa')

sf_africa <- ggplot(data = africa) +
  geom_sf(fill = 'grey90', color = 'grey60')

ggsave(here::here('images', 'plots', 'sf_africa.png'),
       sf_africa, width = 6, height = 3.7)


# Read shape file --------------------------------------------------------

world <- st_read(here::here(
  'data', 'natural_earth_countries',
  'ne_50m_admin_0_countries.shp')) %>%
  clean_names()

ggplot(data = world) +
  geom_sf(fill = 'grey90', color = 'grey60') 

central_park <- st_read(here::here(
  'data', 'central_park', 'CentralPark.shp'))

sf_central_park <- ggplot(data = central_park) +
  geom_sf(color = 'grey75')

ggsave(here::here('images', 'plots', 'sf_central_park.png'),
       sf_central_park, width = 8, height = 7)

# Projections Polygons --------------------------------------------------------

us_states <- map_data("state")
polygon_us <- ggplot(us_states,
                     aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "grey60")

polygon_us +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


# Projections sf --------------------------------------------------------

# World
world <- ne_countries(scale = "medium", returnclass = "sf")

sf_world_robinson <- ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "ESRI:54030")

ggsave(here::here('images', 'plots', 'sf_world_robinson.png'),
       sf_world_robinson, width = 6, height = 3.7)

sf_world_mollweide <- ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "ESRI:54009")

ggsave(here::here('images', 'plots', 'sf_world_mollweide.png'),
       sf_world_mollweide, width = 6, height = 3.7)

# North America, Albers projection
us_states_cont <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

sf_us_cont_albers <- ggplot(data = us_states_cont) +
  geom_sf() +
  coord_sf(crs = "ESRI:102003")

ggsave(here::here('images', 'plots', 'sf_us_cont_albers.png'),
       sf_us_cont_albers, width = 6, height = 3.7)

sf_us_cont_nad83 <- ggplot(data = us_states_cont) +
  geom_sf() +
  coord_sf(crs = 4269)

ggsave(here::here('images', 'plots', 'sf_us_cont_nad83.png'),
       sf_us_cont_nad83, width = 6, height = 3.7)

sf_us_cont_merc <- ggplot(data = us_states_cont) +
  geom_sf() +
  coord_sf(crs = "ESRI:54004")

ggsave(here::here('images', 'plots', 'sf_us_cont_merc.png'),
       sf_us_cont_merc, width = 6, height = 3.7)

sf_us_cont_robinson <- ggplot(data = us_states_cont) +
  geom_sf() +
  coord_sf(crs = "ESRI:54030")

ggsave(here::here('images', 'plots', 'sf_us_cont_robinson.png'),
       sf_us_cont_robinson, width = 6, height = 3.7)

# China 
china <- ne_states(
  country = 'china',
  returnclass = 'sf')

china_crs <- '+proj=robin +datum=WGS84'
sf_china_proj <- ggplot(data = china) +
  geom_sf() +
  coord_sf(crs = st_crs(china_crs))

ggsave(here::here('images', 'plots', 'sf_china_proj.png'),
       sf_china_proj, width = 6, height = 3.7)

# Choropleth milk -------------------------------------------------------
# https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

# Milk 2017
milk_2017 <- milk_production %>% 
  filter(year == 2017) %>% 
  select(name = state, milk_produced) %>% 
  mutate(milk_produced = milk_produced / 10^9)

# US States, continental data
us_states <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii')) %>% 
  left_join(milk_2017, by = 'name')

# Milk fill
sf_us_milk_2017 <- ggplot(us_states) +
  geom_sf(aes(fill = milk_produced)) + #<<
  scale_fill_viridis(
    option = "plasma", 
    limits = c(0, 40)) +
  theme_void(base_size = 15) +
  theme(legend.position = 'bottom') +
  labs(fill = 'Milk produced\n(billions lbs)', 
       title = 'Milk Production by State in 2017')

ggsave(here::here('images', 'plots', 'sf_us_milk_2017.png'),
       sf_us_milk_2017, width = 6, height = 3.7)

# Milk fill quad
sf_us_milk_2017_quad <- ggplot(us_states) +
  geom_sf(aes(fill = milk_produced)) + #<<
  scale_fill_viridis(
    trans = 'sqrt',
    option = "plasma", 
    limits = c(0, 40)) +
  theme_void(base_size = 15) +
  theme(legend.position = 'bottom') +
  labs(fill = 'Milk produced\n(billions lbs)', 
       title = 'Milk Production by State in 2017')

ggsave(here::here('images', 'plots', 'sf_us_milk_2017_quad.png'),
       sf_us_milk_2017_quad, width = 6, height = 3.7)

# Albers projection
sf_us_milk_2017_albers <- ggplot(us_states) +
  geom_sf(aes(fill = milk_produced)) +
  coord_sf(crs = "ESRI:102003") +
  scale_fill_viridis(
    option = "plasma", 
    limits = c(0, 40)) +
  theme_void(base_size = 15) +
  theme(legend.position = 'bottom') +
  labs(fill = 'Milk produced\n(billions lbs)', 
       title = 'Milk Production by State in 2017')

ggsave(here::here('images', 'plots', 'sf_us_milk_2017_albers.png'),
       sf_us_milk_2017_albers, width = 6, height = 3.7)
# 
# # Animation...broken
# milk_years <- milk_production %>% 
#     select(name = state, year, milk_produced) %>% 
#     mutate(milk_produced = milk_produced / 10^9,
#            year = as.integer(year))
# 
# us_states <- ne_states(
#     country = 'united states of america',
#     returnclass = 'sf') %>%
#     filter(! name %in% c('Alaska', 'Hawaii')) %>% 
#     right_join(milk_years, by = 'name')
# 
# us_milk_anim_plot <- ggplot(us_states) +
#     geom_sf(aes(fill = milk_produced)) +
#     scale_fill_viridis(
#         option = "plasma", 
#         limits = c(0, 40)) +
#     theme_void(
#         base_size = 15,
#         base_family = 'Roboto Condensed') +
#     theme(legend.position = 'bottom') +
#     labs(fill = 'Milk produced (billions lbs)', 
#          title = 'Milk Production by State')
# 
# library(gganimate)
# 
# us_milk_anim_plot +
#     transition_time(year) 
# 
# us_milk_anim <- us_milk_anim_plot +
#     transition_time(year)
# labs(title = "Year: {frame_time}")
# 
# # transition_reveal(year)
# 
# # Render the animation
# animate(us_milk_anim, end_pause = 10,
#         width = 800, height = 600, res = 150)



# Choropleth internet -------------------------------------------------------

internet_users_2015 <- internet_users %>% 
  filter(year == 2015)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Join internet user data to world shape file
world_int <- world %>% 
  left_join(internet_users_2015, by = c('iso_a3' = 'code')) %>% 
  filter(iso_a3 != "ATA")  # No internet in Antarctica...sorry penguins.

# Base map 
sf_world_internet <- ggplot(world_int) +
  geom_sf(aes(fill = percent), color = NA) +
  scale_fill_gradient(
    low = "#e7e1ef",
    high = "#dd1c77",
    na.value = "grey70",
    limits = c(0, 100)) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 10, barheight = 0.5)) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Internet access by country in 2015",
    fill = '% of population with access')

# Robinson
sf_world_internet_robinson <- sf_world_internet +
  coord_sf(crs = "ESRI:54030")

# Mercator
sf_world_internet_mercator <- sf_world_internet +
  coord_sf(crs = "ESRI:54004")

ggsave(here::here('images', 'plots', 'sf_world_internet.png'),
       sf_world_internet, width = 6, height = 3.7)

ggsave(here::here('images', 'plots', 'sf_world_internet_robinson.png'),
       sf_world_internet_robinson, width = 6, height = 3.7)

ggsave(here::here('images', 'plots', 'sf_world_internet_mercator.png'),
       sf_world_internet_mercator, width = 6, height = 3.7)



# Bubble map - England -------------------------------------------------------
# https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2.html

uk_shape <- map_data("world") %>%
  filter(region == "UK")

uk_cities <- world.cities %>%
  filter(country.etc == "UK") %>%
  mutate(pop = pop / 10^6) %>%
  arrange(pop)

uk_top10cities <- uk_cities %>%
  arrange(desc(pop)) %>%
  slice(1:10)

# All cities as points
ggplot() +
  geom_polygon(data = uk_shape,
               aes(x = long, y = lat, group = group),
               fill = "grey", alpha = 0.3) +
  geom_point(data = uk_cities,
             aes(x = long, y = lat)) +
  scale_y_continuous(limits = c(50, 59)) +
  coord_map() +
  theme_void()

# Second graphic with names of the 10 biggest cities
uk_cities_plot <- ggplot() +
  geom_polygon(data = uk_shape,
               aes(x = long, y = lat, group = group),
               fill = "grey60", alpha = 0.3) +
  geom_point(data = uk_top10cities,
             aes(x = long, y = lat)) +
  geom_text_repel(data = uk_top10cities,
                  aes(x = long, y = lat, label = name),
                  size = 5) +
  geom_point(data = uk_top10cities,
             aes(x = long, y = lat),
             color = "red", size = 3) +
  scale_y_continuous(limits = c(50, 59)) +
  coord_map() +
  theme_void()

ggsave(here::here('images', 'plots', 'uk_cities_plot.png'),
       uk_cities_plot, width = 4.5, height = 4.5)

# Now all cities with population and color scale
ggplot() +
  geom_polygon(data = uk_shape,
               aes(x = long, y = lat, group = group),
               fill = "grey", alpha = 0.3) +
  geom_point(data = uk_cities,
             aes(x = long, y = lat,
                 size = pop, color = pop),
             alpha = 0.9) +
  scale_size_area() +
  scale_color_viridis(trans = 'log') +
  scale_y_continuous(limits = c(50, 59)) +
  coord_map() +
  theme_void()

# Same as above but polished
mybreaks <- c(0.02, 0.04, 0.08, 1, 7)

uk_pop_area <- ggplot() +
  geom_polygon(data = uk_shape,
               aes(x = long, y = lat, group = group),
               fill = "grey70", alpha = 0.3) +
  geom_point(data = uk_cities,
             aes(x = long, y = lat,
                 size = pop, color = pop),
             alpha = 0.9) +
  scale_size(range = c(0.02, 7), breaks = mybreaks) +
  scale_alpha_continuous(
    trans="log", range = c(0.1, 0.9), breaks = mybreaks) +
  scale_color_viridis(
    option = "magma", trans = "log", breaks = mybreaks) +
  scale_y_continuous(limits = c(50, 59)) +
  guides( colour = guide_legend()) +
  coord_map() +
  theme_void(base_family = 'Roboto Condensed') +
  theme(legend.position = c(0.95, 0.7)) +
  labs(size = "Population\n(in Millions)",
       alpha = "Population\n(in Millions)",
       color = "Population\n(in Millions)",
       title = "The 1000 most populous cities in the UK")

uk_pop_radius <- uk_pop_area +
  scale_radius(range = c(0.02, 7), breaks = mybreaks)

ggsave(here::here('images', 'plots', 'uk_pop_area.png'),
       uk_pop_area, width = 4.5, height = 4.5)

ggsave(here::here('images', 'plots', 'uk_pop_radius.png'),
       uk_pop_radius, width = 4.5, height = 4.5)

# Bubble map squirrels ------------------------------------------------------

central_park <- st_read(here::here(
  'data', 'central_park', 'CentralPark.shp'))
squirrels <- read_csv(here::here('data', 'nyc_squirrels.csv'))

squirrels <- squirrels %>% 
  filter(!is.na(primary_fur_color))

sf_central_park_squirrels <- ggplot(data = central_park) +
  geom_sf(color = 'grey75') + 
  geom_point(data = squirrels,
             aes(x = long, y = lat, color = primary_fur_color),
             size = .5) + 
  theme_void(base_size = 15) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(title.position = "top")) +
  labs(color = 'Primary fur color', 
       title = 'Squirrels in NYC Central Park')

ggsave(here::here('images', 'plots', 'sf_central_park_squirrels.png'),
       sf_central_park_squirrels, width = 8, height = 7)

# Facets
sf_central_park_squirrels_facet <- ggplot(data = central_park) +
  geom_sf(color = 'grey75') + 
  geom_point(data = squirrels,
             aes(x = long, y = lat, color = primary_fur_color),
             size = .5) +
  facet_wrap(~primary_fur_color, nrow = 1) +
  theme_void(base_size = 15) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Squirrels in NYC Central Park')

ggsave(here::here('images', 'plots', 'sf_central_park_squirrels_facet.png'),
       sf_central_park_squirrels_facet, width = 10, height = 4)



# Coffee maps ----------------------------------------------------

# US States, continental data
us_states_cont <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

# Directly plot points
sf_us_coffee <- ggplot() +
  geom_sf(data = us_states_cont) +
  geom_point(
    data = us_coffee_shops,
    aes(x = long, y = lat, color = name),
    size = 0.3) + 
  theme_void(base_size = 15) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(
    # Move legend title to top
    title.position = "top",
    # Increase legend point size
    override.aes = list(size = 3))) +
  labs(color = 'Coffee shop', 
       title = 'Coffee Shops in the US')

ggsave(here::here('images', 'plots', 'sf_us_coffee.png'),
       sf_us_coffee, width = 9, height = 6)

# Plot points with Albers projection
sf_us_coffee_albers_bad <- sf_us_coffee +
  coord_sf(crs = "ESRI:102003")

ggsave(here::here('images', 'plots', 'sf_us_coffee_albers_bad.png'),
       sf_us_coffee_albers_bad, width = 9, height = 6)

# First convert coordinate system of coffee shops
us_coffee_shops_sf <- st_as_sf(us_coffee_shops,
                               coords = c("long", "lat"),
                               crs = st_crs(us_states_cont))

# Now plot both
sf_us_coffee_base <- ggplot() +
  geom_sf(data = us_states_cont) +
  geom_sf(
    data = us_coffee_shops_sf,
    aes(color = name), 
    size = 0.3) +
  theme_void(base_size = 15) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(
    # Move legend title to top
    title.position = "top",
    # Increase legend point size
    override.aes = list(size = 3))) +
  labs(fill = 'Coffee shop', 
       title = 'Coffee Shops in the US')

ggsave(here::here('images', 'plots', 'sf_us_coffee_base.png'),
       sf_us_coffee_base, width = 9, height = 6)

# Now add the albers CRS
sf_us_coffee_albers <- sf_us_coffee_base +
  coord_sf(crs = "ESRI:102003")

ggsave(here::here('images', 'plots', 'sf_us_coffee_albers.png'),
       sf_us_coffee_albers, width = 9, height = 6)

# LCC projection
sf_us_coffee_lcc <- sf_us_coffee_base +
  coord_sf(crs = "ESRI:102004") 

ggsave(here::here('images', 'plots', 'sf_us_coffee_lcc.png'),
       sf_us_coffee_lcc, width = 9, height = 6)


# State centroid map ------------------------------------------------------

# US States, continental data
us_states_cont <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii')) %>% 
  st_transform(crs = "ESRI:102003")

# Compute centroids of each state
centroids <- as.data.frame(
  st_coordinates(st_centroid(us_states_cont)))
names(centroids) <- c('label_x', 'label_y')
us_states_cont <- bind_cols(us_states_cont, centroids) %>% 
  left_join(state_abbs, by = c('name' = 'state_name'))

# Make the plot 
sf_us_labeled <- ggplot(us_states_cont) +
  geom_sf() + 
  geom_label(aes(x = label_x, y = label_y, label = state_abb)) +
  theme_void(base_size = 15)

ggsave(here::here('images', 'plots', 'sf_us_labeled.png'),
       sf_us_labeled, width = 9, height = 5.5)


# Hexbin map -----------------------------------------------------------
# https://www.r-graph-gallery.com/hexbin-map.html
# Also statebins: http://socviz.co/maps.html#maps



# OTHER ----------------------------------------------------------------



# sf & spatial ------------------------------------------------------------
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html


# andrew - world projections ----------------------------------------------------------------
# https://datavizf17.classes.andrewheiss.com/assignment/07-assignment/

int_users_2015 <- internet_users %>%
  filter(year == 2015)

world_shapes <- st_read(here::here(
  'data', 'natural_earth_countries', 'ne_50m_admin_0_countries.shp'),
  stringsAsFactors = FALSE) %>%
  clean_names()

int_users_2015 <- world_shapes %>%
  left_join(int_users_2015, by = c('iso_a3' = 'code')) %>%
  filter(iso_a3 != "ATA")  # No internet in Antarctica...sorry penguins.

ggplot(int_users_2015) +
  geom_sf(aes(fill = percent), color = NA) +
  coord_sf(crs = "ESRI:54030", datum = NA) +
  scale_fill_gradient(
    low = "#e7e1ef",
    high = "#dd1c77",
    na.value = "grey70",
    limits = c(0, 100)) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 10, barheight = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = '% of population')

# internet animation ----------------------------------------------------------
# https://ourworldindata.org/internet

# Map insets ----------------------------------------------------------
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

# Polygon election ----------------------------------------------------
# http://socviz.co/maps.html#maps

library(socviz)

us_states <- map_data("state")
head(us_states)

ggplot(us_states,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(us_states,
       aes(x = long, y = lat, group = group,
           fill = region)) +
  geom_polygon(color = "grey90", size = 0.1) +
  theme(legend.position = 'none')

ggplot(us_states,
       aes(x = long, y = lat, group = group,
           fill = region)) +
  geom_polygon(color = "grey90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = 'none')

election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)
party_colors <- c("#2E74C0", "#CB454A")

ggplot(us_states_elec,
       aes(x = long, y = lat,
           group = group, fill = party)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = party_colors) +
  theme_void() +
  labs(title = "Election Results 2016", fill = NULL)

ggplot(us_states_elec,
       aes(x = long, y = lat,
           group = group, fill = pct_trump)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = "white", high = "#CB454A") +
  theme_void()
labs(title = "Trump vote",
     fill = "Percent")

ggplot(us_states_elec,
       aes(x = long, y = lat,
           group = group, fill = d_points)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(
    low = "red",
    mid = scales::muted("purple"),
    high = "blue",
    breaks = c(-25, 0, 25, 50, 75)) +
  theme_void() +
  labs(title = "Winning margins",
       fill = "Percent")

ggplot(us_states_elec %>%
         filter(! region %in% "district of columbia"),
       aes(x = long, y = lat,
           group = group, fill = d_points)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(
    low = "red",
    mid = scales::muted("purple"),
    high = "blue") +
  theme_void() +
  labs(title = "Winning margins",
       fill = "Percent")

# Polygon population -------------------------------------------------
# http://socviz.co/maps.html#maps

county_full <- county_map %>%
  left_join(county_data, by = "id") %>%
  mutate(
    pop_dens = fct_recode(pop_dens,
                          "0-10"        = "[    0,   10)",
                          "10-50"       = "[   10,   50)",
                          "50-100"      = "[   50,  100)",
                          "100-500"     = "[  100,  500)",
                          "500-1,000"   = "[  500, 1000)",
                          "1,000-5,000" = "[ 1000, 5000)",
                          ">5,000"      = "[ 5000,71672]"))

ggplot(county_full,
       aes(x = long, y = lat,
           fill = pop_dens, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal() +
  scale_fill_brewer(palette="Blues") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Population per\nsquare mile")

ggplot(county_full,
       aes(x = long, y = lat,
           fill = pct_black, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal() +
  scale_fill_brewer(palette="Greens") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "US Population, Percent Black")




# Animation reprex -------------------------------------------------

library(ggplot2)
library(gganimate)

us_states <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

fill_data <- data.frame(
  name = rep(us_states$name, 2), 
  year = c(rep(2000, nrow(us_states)), rep(2001, nrow(us_states))),
  fill = rnorm(2*nrow(us_states)))

us_states <- left_join(us_states, fill_data, by = 'name')

# Static works :)
ggplot(us_states) + 
  geom_sf(aes(fill = fill))

# Animation fails :(
ggplot(us_states) + 
  geom_sf(aes(fill = fill)) + 
  transition_time(year)


# scales ----

# aesthetics ----

# anim ----

# interact  ----

