# This file contains all the plots for the book 
# that are not written in code chunks. This just speeds
# things up when rendering the book.

suppressWarnings(suppressMessages(source("_common.R")))

# Large packages that are only needed for the map plots here
library(rnaturalearthdata)
library(rnaturalearthhires)

# summarizing-data ----

# Set main theme settings
theme_set(theme_gray(base_size = 18))

# Read in data
cor_w <- 5
cor_h <- 4

# wildlife histogram ----

labels <- wildlife_impacts %>%
    filter(! is.na(height)) %>%
    summarise(mean = round(mean(height)),
              median = round(median(height))) %>%
    gather(stat, value, mean:median) %>%
    mutate(y = c(22000, 25000), stat = paste0(stat, ": ", value))
wildlife_hist <- wildlife_impacts %>%
    filter(! is.na(height)) %>%
    filter(height < 20000) %>% 
    ggplot() +
    geom_histogram(aes(x = height), bins = 50) +
    scale_y_continuous(expand = expansion(c(0, 0.05))) +
    scale_x_continuous(expand = expansion(c(0, 0.05))) +
    labs(title = 'Histogram of impact height',
         x = 'Height (ft)', y = 'Count') +
    geom_vline(xintercept = labels$value, linetype = "dashed", color = "red",
               size = 1) +
    geom_label_repel(data = labels, aes(x = value, y = y, label = stat),
               hjust = 0, nudge_x = 3000, size = 6)

ggsave(
    here::here('figs', 'wildlife-hist.png'),
    wildlife_hist, width = 9, height = 5
)

# daysToShip -----

daysToShip %>%
    gather(warehouse, days, warehouseA:warehouseB) %>%
    group_by(warehouse) %>%
    summarise(
        mean   = mean(days),
        median = median(days),
        range = max(days) - min(days),
        sd    = sd(days)
    )
daysToShipLabels <- daysToShip %>%
    gather(warehouse, days, warehouseA:warehouseB) %>%
    group_by(warehouse) %>%
    summarise(
        range = max(days) - min(days),
        sd    = sd(days)) %>%
    mutate(
        x1 = 2, x2 = 4.5, y = 4,
        label1 = 'Range:\nSD:',
        label2 = paste0(range, '\n', round(sd, 2)))
daysToShip_fig <- daysToShip %>%
    gather(warehouse, days, warehouseA:warehouseB) %>%
    group_by(warehouse) %>%
    mutate(days = days - mean(days)) %>%
    ggplot() +
    geom_col(aes(x = as.factor(order), y = days), width = 0.7) +
    geom_text(data = daysToShipLabels, aes(x = x1, y = y, label = label1),
               hjust = 0, size = 8, fontface = "bold") +
    geom_text(data = daysToShipLabels, aes(x = x2, y = y, label = label2),
               hjust = 0, size = 8) +
    facet_wrap(~warehouse, nrow = 1) +
    geom_hline(yintercept = 0, size = 1) +
    labs(title = 'Difference from mean days to ship',
         x = 'Order', y = 'Days to ship')

ggsave(
    here::here('figs', 'days-to-ship.png'),
    daysToShip_fig, width = 12, height = 6
)

daysToShip_sd_fig <- daysToShip %>%
    gather(warehouse, days, warehouseA:warehouseB) %>%
    group_by(warehouse) %>%
    mutate(days = days - mean(days)) %>%
    filter(warehouse == 'warehouseB') %>%
    ggplot() +
    geom_col(aes(x = as.factor(order), y = days), width = 0.7) +
    geom_text(data = daysToShipLabels %>% filter(warehouse == 'warehouseB'),
              aes(x = x1, y = y, label = label1),
               hjust = 0, size = 8, fontface = "bold") +
    geom_text(data = daysToShipLabels %>% filter(warehouse == 'warehouseB'),
              aes(x = x2, y = y, label = label2),
               hjust = 0, size = 8) +
    geom_hline(yintercept = 0, size = 1) +
    labs(title = 'Difference from mean days to ship',
         x = 'Order', y = 'Days to ship')

ggsave(
    here::here('figs', 'days-to-ship-sd.png'),
    daysToShip_sd_fig, width = 6, height = 5
)


# anscombe-quartet ----

x       <- c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)
y       <- c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
quartet <- rep(c('A', 'B', 'C', 'D'), each = nrow(anscombe))
ansDf   <- tibble(x, y, quartet)

anscombe_quartet <- ansDf %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(alpha = 0.6, size = 2) +
    stat_smooth(method = 'lm', se = F, size = 0.4, alpha = 0.6) +
    facet_wrap(vars(quartet)) +
    scale_x_continuous(breaks=seq(4, 18, 2), limits=c(4, 19)) +
    scale_y_continuous(breaks=seq(4, 12, 2), limits=c(3, 13)) +
    theme(
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))

ggsave(
    here::here('figs', 'anscombe-quartet.png'),
    anscombe_quartet, width = 8, height = 6
)

# Galton scatterplot -----

# midparentHeight =  (father + 1.08*mother)/2
galtonCorr <- round(cor(
    GaltonFamilies$childHeight, GaltonFamilies$midparentHeight,
    method = 'pearson'), 2)

galtonScatterplot <- ggplot(GaltonFamilies) +
    geom_point(aes(x = midparentHeight, y = childHeight),
               size = 0.5, alpha = 0.7) +
    annotate(geom = 'text', x = 64, y = 79,
             label = paste('r = ', galtonCorr), hjust = 0,
             size = 5) +
    theme_classic(base_size = 18) +
    labs(x = 'Midparent height (inches)',
         y = 'Child height (inches)')

ggsave(here::here('figs', 'galtonScatterplot.png'),
       galtonScatterplot, width = 5, height = 4)

# Fitting a line
galtonScatterplotSmooth <- galtonScatterplot +
    geom_smooth(aes(x = midparentHeight, y = childHeight),
                method = 'lm', se = FALSE)

ggsave(
    here::here('figs', 'galtonScatterplotSmooth.png'),
    galtonScatterplotSmooth, width = 5, height = 4
)

# penguins -----

penguinScatterplotBase <- ggplot(penguins, 
                                 aes(x = flipper_length_mm, 
                                     y = body_mass_g)) +
    geom_point(size = 1, alpha = 0.7) +
    theme_classic(base_size = 20) +
    labs(x = "Flipper length (mm)",
         y = "Body mass (g)")

penguinCorr <- round(cor(
    penguins$body_mass_g, penguins$flipper_length_mm,
    method = 'pearson', use = "complete.obs"), 2)

penguinScatterplot <- penguinScatterplotBase +
    annotate(geom = 'text', x = 175, y = 6000,
             label = paste('r = ', penguinCorr), 
             hjust = 0, size = 5)

ggsave(
    here::here('figs', 'penguinScatterplotBase.png'),
    penguinScatterplotBase, width = 5, height = 4
)

ggsave(
    here::here('figs', 'penguinScatterplot.png'),
    penguinScatterplot, width = 5, height = 4
)

# Fitting a line

# Method 1
penguinScatterplotSmooth <- penguinScatterplot +
    geom_smooth(method = 'lm', se = FALSE)

# Add equation label, v1
penguinFit <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
coefs <- round(coef(penguinFit), 2)
eqLabel <- paste('y = ', coefs[1], ' + ', coefs[2], 'x')
penguinScatterplotEq <- penguinScatterplotSmooth +
    annotate(geom = 'text', x = 175, y = 5700,
             label = eqLabel, color = "blue", 
             hjust = 0, size = 5)

# Method 2
penguinScatterplotAbline <- penguinScatterplot +
    geom_abline(intercept = penguinFit$coefficients[1],
                slope = penguinFit$coefficients[2],
                color = 'blue', size = 1)

ggsave(
    here::here('figs', 'penguinScatterplotSmooth.png'),
    penguinScatterplotSmooth, width = 5, height = 4
)
ggsave(
    here::here('figs', 'penguinScatterplotAbline.png'),
    penguinScatterplotAbline, width = 5, height = 4
)
ggsave(
    here::here('figs', 'penguinScatterplotEq.png'),
    penguinScatterplotEq, width = 5, height = 4
)

# simpson paradox penguins ------

simpsonCorr <- cor(
    penguins$body_mass_g, penguins$bill_depth_mm,
    method = 'pearson', use = "complete.obs"
)

simpsonLabel <- paste("r = ", round(simpsonCorr, 2))

simpson_penguins <- penguins %>% 
    ggplot(aes(x = bill_depth_mm, y = body_mass_g)) +
    geom_point(size = 1, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    annotate(geom = 'text', x = 19, y = 6000,
             label = simpsonLabel, 
             hjust = 0, size = 5) +
    theme_classic(base_size = 18) +
    labs(x = "Bill depth (mm)",
         y = "Body mass (g)")

adelie <- filter(penguins, species == "Adelie")
chinstrap <- filter(penguins, species == "Chinstrap")
gentoo <- filter(penguins, species == "Gentoo")
corrAdelie <- cor(
    adelie$body_mass_g, adelie$bill_depth_mm,
    method = 'pearson', use = "complete.obs")
corrChinstrap <- cor(
    chinstrap$body_mass_g, chinstrap$bill_depth_mm,
    method = 'pearson', use = "complete.obs")
corrGentoo <- cor(
    gentoo$body_mass_g, gentoo$bill_depth_mm,
    method = 'pearson', use = "complete.obs")
labAdelie <- paste("r =", round(corrAdelie, 2))
labChinstrap <- paste("r =", round(corrChinstrap, 2))
labGentoo <- paste("r =", round(corrGentoo, 2))

simpson_penguins_good <- penguins %>% 
    ggplot(aes(x = bill_depth_mm, y = body_mass_g, color = species)) +
    geom_point(size = 1, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    annotate(geom = 'text', x = 19, y = 5100,
             label = labAdelie, color = "darkorange", 
             hjust = 0, size = 5) +
    annotate(geom = 'text', x = 20, y = 3000,
             label = labChinstrap, color = "purple", 
             hjust = 0, size = 5) +
    annotate(geom = 'text', x = 13, y = 6100,
             label = labGentoo, color = "cyan4", 
             hjust = 0, size = 5) +
    theme_classic(base_size = 18) +
    labs(x = "Bill depth (mm)",
         y = "Body mass (g)", 
         color = "Species")

ggsave(
    here::here('figs', 'simpson_penguins.png'),
    simpson_penguins, width = 5, height = 4
)

ggsave(
    here::here('figs', 'simpson_penguins_good.png'),
    simpson_penguins_good, width = 6, height = 4
)

 # correlations -----------------------------------------------------------

cor_theme <- function() {
    return(
        theme_half_open(font_size = 20) +
        theme(plot.title = element_text(hjust = 0.5, size = 20)))
}

cor_df <- data.frame(x = runif(100, 0, 10)) %>%
    mutate(quad       = -(x-5)^2 + rnorm(100, 5, 2),
           vstrong_p  = 2*x + rnorm(100, 0, 2),
           strong_p   = 2*x + rnorm(100, 0, 5),
           moderate_p = 2*x + rnorm(100, 0, 15),
           weak_p     = 2*x + rnorm(100, 0, 35),
           vstrong_n  = -1*vstrong_p,
           strong_n   = -1*strong_p,
           moderate_n = -1*moderate_p,
           weak_n     = -1*weak_p,
           cor_vstrong_p  = round(cor(x, vstrong_p),  2),
           cor_strong_p   = round(cor(x, strong_p),   2),
           cor_moderate_p = round(cor(x, moderate_p), 2),
           cor_weak_p     = round(cor(x, weak_p),     2),
           cor_vstrong_n  = round(cor(x, vstrong_n),  2),
           cor_strong_n   = round(cor(x, strong_n),   2),
           cor_moderate_n = round(cor(x, moderate_n), 2),
           cor_weak_n     = round(cor(x, weak_n),     2))

cor_quad <- ggplot(cor_df) +
    geom_point(aes(x = x, y = quad), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y')

cor_vstrong_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = vstrong_p), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_vstrong_p[1]))

cor_strong_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = strong_p), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_strong_p[1]))

cor_moderate_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = moderate_p), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_moderate_p[1]))

cor_weak_p <- ggplot(cor_df) +
    geom_point(aes(x = x, y = weak_p), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_weak_p[1]))

cor_vstrong_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = vstrong_n), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_vstrong_n[1]))

cor_strong_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = strong_n), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_strong_n[1]))

cor_moderate_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = moderate_n), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_moderate_n[1]))

cor_weak_n <- ggplot(cor_df) +
    geom_point(aes(x = x, y = weak_n), size = 1.5, alpha = 0.7) +
    cor_theme() +
    labs(x = 'x', y = 'y',
         title = paste0('r = ', cor_df$cor_weak_n[1]))

# Combined plots
cor_p <- plot_grid(cor_weak_p, cor_moderate_p,
                   cor_strong_p, cor_vstrong_p, ncol = 2)
cor_n <- plot_grid(cor_weak_n, cor_moderate_n,
                   cor_strong_n, cor_vstrong_n, ncol = 2)

ggsave(here::here('figs', 'cor_quad.png'),
       cor_quad, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_vstrong_p.png'),
       cor_vstrong_p, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_strong_p.png'),
       cor_strong_p, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_moderate_p.png'),
       cor_moderate_p, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_weak_p.png'),
       cor_weak_p, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_vstrong_n.png'),
       cor_vstrong_n, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_strong_n.png'),
       cor_strong_n, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_moderate_n.png'),
       cor_moderate_n, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_weak_n.png'),
       cor_weak_n, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'cor_n.png'),
       cor_n, width = cor_w*2, height = cor_h*2)
ggsave(here::here('figs', 'cor_p.png'),
       cor_p, width = cor_w*2, height = cor_h*2)

# outliers ----------------------------------------------------------------

# Make the plot data frames
outliers <- data.frame(x = rnorm(20, 0, 0.5)) %>%
    mutate(y1 = x - rnorm(20, 0, 0.5),
           y2 = y1, y3 = y1, y4 = y1, y5 = y1,
           y6 = y1, y7 = y1, y8 = y1, y9 = y1,
           point = 'base')
points <- expand.grid(data.frame(
    x = c(-5, 0, 5),
    y = c(-5, 0, 5)))
points_df <- as.data.frame(diag(points$y))
names(points_df) <- c('y7', 'y8', 'y9', 'y4', 'y5', 'y6', 'y1', 'y2', 'y3')
for (i in 1:nrow(points_df)) {
    cols <- seq(ncol(points_df))
    cols <- cols[-which(cols == i)]
    points_df[i,cols] <- NA
}
points_df <- bind_cols(data.frame(x = points$x), points_df)
points_df$point <- 'outlier'
outliers <- bind_rows(outliers, points_df) %>%
    gather(key = 'case', value = 'y', y1:y9) %>%
    filter(!is.na(y)) %>%
    group_by(case) %>%
    mutate(pearson  = round(cor(x, y, method = 'pearson'), 2),
           spearman = round(cor(x, y, method = 'spearman'), 2),
           case_p   = paste0('r = ', pearson),
           case_s   = paste0('r = ', spearman))

# Create facet labels
case_p <- outliers %>% distinct(case, case_p)
case_p_labels <- case_p$case_p
names(case_p_labels) <- case_p$case
case_s <- outliers %>% distinct(case, case_s)
case_s_labels <- case_s$case_s
names(case_s_labels) <- case_s$case

# Make the plots
outlier_plot <- function(df, labels) {
    plot <- ggplot(df) +
        geom_point(aes(x = x, y = y, color = point),
                   size = 3, alpha = 0.7) +
        facet_wrap(~case, ncol = 3,
                   labeller = labeller(case = labels)) +
        scale_color_manual(values = c('black', 'red')) +
        scale_x_continuous(limits = c(-6, 6)) +
        scale_y_continuous(limits = c(-6, 6)) +
        theme_bw(base_family = 'Roboto Condensed', base_size = 20) +
        panel_border() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = 'none',
              strip.text.x = element_text(size = 20, face = 'bold'),
              panel.grid.minor = element_blank()) +
        labs(x = 'x', y = 'y')
    return(plot)
}

pearson_grid  <- outlier_plot(outliers, case_p_labels)
spearman_grid <- outlier_plot(outliers, case_s_labels)
pearson_base  <- outlier_plot(outliers %>% filter(case == 'y5'), case_p_labels)
pearson_1     <- outlier_plot(outliers %>% filter(case == 'y2'), case_p_labels)
pearson_2     <- outlier_plot(outliers %>% filter(case == 'y1'), case_p_labels)

ggsave(here::here('figs', 'pearson_grid.png'),
       pearson_grid, width = 8, height = 7)
ggsave(here::here('figs', 'spearman_grid.png'),
       spearman_grid, width = 8, height = 7)
ggsave(here::here('figs', 'pearson_base.png'),
       pearson_base, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'pearson1.png'),
       pearson_1, width = cor_w, height = cor_h)
ggsave(here::here('figs', 'pearson2.png'),
       pearson_2, width = cor_w, height = cor_h)

# Plot summary of comparison
outlier_compare <- outliers %>%
    distinct(case, pearson, spearman) %>%
    gather(key = 'type', value = 'r', pearson:spearman) %>%
    mutate(type = str_to_title(type)) %>%
    ggplot(aes(x = '', y = r)) +
    facet_wrap(~type) +
    geom_boxplot(width = 0.2, color = 'grey42', outlier.shape = NA) +
    geom_jitter(size = 1.7, alpha = 0.7, width = 0.1) +
    geom_hline(yintercept = 0, color = 'grey42') +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1, -0.5, 0, 0.5, 1)) +
    theme_bw(base_size = 12) +
    labs(x = NULL)

ggsave(here::here('figs', 'outlier_compare.png'),
       outlier_compare, width = 4, height = 4)

# mtcars corr ------------------------------------------------------------

mtcarsScatterplotBase <- ggplot(mtcars) +
    geom_point(aes(x = mpg, y = hp),
               size = 2, alpha = 0.7) +
    theme_classic(base_size = 20) +
    labs(x = 'Fuel economy (mpg)',
         y = 'Engine power (hp)')

mtcarsCorr <- round(cor(mtcars$mpg,
                        mtcars$hp, method = 'pearson'), 2)
mtcarsScatterplot <- mtcarsScatterplotBase +
    annotate(geom = 'text', x = 25, y = 310,
             label = paste('r = ', mtcarsCorr), hjust = 0,
             size = 7)

ggsave(here::here('figs', 'mtcarsScatterplotBase.png'),
       mtcarsScatterplotBase, width = 5, height = 4)

ggsave(here::here('figs', 'mtcarsScatterplot.png'),
       mtcarsScatterplot, width = 5, height = 4)

# wildlife_impacts ------------------------------------------------------------

wildlife_impacts %>%
    ggcorr(label = TRUE)

corLab <- round(cor(
    wildlife_impacts$height, wildlife_impacts$speed,
    use = "complete.obs"), 2)

wildlife_cor <- wildlife_impacts %>%
    filter(!is.na(speed), !is.na(height)) %>%
    summarise(pearson = cor(speed, height, method = 'pearson'),
              spearman = cor(speed, height, method = 'spearman'))

ggplot(wildlife_impacts) +
    geom_point(aes(x = speed, y = height), alpha = 0.7) +
    annotate(geom = 'text', x = 50, y = 22000,
             label = paste('r = ', corLab),
             hjust = 0, size = 5) +
    theme_minimal_grid() +
    labs(x = 'Speed (mph)',
         y = 'Height (ft)')

# msleep ------------------------------------------------------------

# Linear
corLabLinear <- round(cor(
    msleep$bodywt, msleep$brainwt,
    use = "complete.obs"), 2)

modelLinear <- lm(brainwt ~ bodywt, data = msleep)
coefs <- round(coef(modelLinear), 3)
eqLabel <- paste('y = ', coefs[1], ' + ', coefs[2], 'x')

ggplot(msleep, aes(x = bodywt, y = brainwt)) +
    geom_point() +
    annotate(geom = 'text', x = 100, y = 6,
             label = paste('r = ', corLabLinear),
             hjust = 0, size = 5) +
    annotate(geom = 'text', x = 100, y = 5.5,
             label = eqLabel, hjust = 0,
             size = 5) +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_minimal_grid()

# Log
corLabLog <- round(cor(
    log(msleep$bodywt), log(msleep$brainwt),
    use = "complete.obs"), 2)

modelLog <- lm(log(brainwt) ~ log(bodywt), data = msleep)
coefs <- round(coef(modelLog), 3)
eqLabel <- paste('log(y) = ', coefs[1], ' + ', coefs[2], ' log(x)')

ggplot(msleep, aes(x = bodywt, y = brainwt)) +
    geom_point() +
    annotate(geom = 'text', x = 0.01, y = 6,
             label = paste('r = ', corLabLog),
             hjust = 0, size = 5) +
    annotate(geom = 'text', x = 0.01, y = 3,
             label = eqLabel, hjust = 0,
             size = 5) +
    geom_smooth(method = 'lm', se = FALSE) +
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal_grid()

# corr ------------------------------------------------------------

wildlife_impacts %>%
    ggcorr(label = TRUE)

corLab <- round(cor(
    wildlife_impacts$height, wildlife_impacts$speed,
    use = "complete.obs"), 2)

wildlife_cor <- wildlife_impacts %>%
    filter(!is.na(speed), !is.na(height)) %>%
    summarise(pearson = cor(speed, height, method = 'pearson'),
              spearman = cor(speed, height, method = 'spearman'))

ggplot(wildlife_impacts) +
    geom_point(aes(x = speed, y = height), alpha = 0.7) +
    annotate(geom = 'text', x = 50, y = 22000,
             label = paste('r = ', corLab),
             hjust = 0, size = 5) +
    theme_minimal_grid() +
    labs(x = 'Speed (mph)',
         y = 'Height (ft)')

# ggcorr ------------------------------------------------------------------

ggcor_mtcars <- mtcars %>%
    ggcorr()

ggcor_mtcars_labels <- mtcars %>%
    ggcorr(label = TRUE,
           label_size = 3,
           label_round = 2)

ggcor_mtcars_pearson <- mtcars %>%
    ggcorr(label = TRUE,
           label_size = 3,
           label_round = 2,
           method = c("pairwise", "pearson"))

ggcor_mtcars_spearman <- mtcars %>%
    ggcorr(label = TRUE,
           label_size = 3,
           label_round = 2,
           method = c("pairwise", "spearman"))

ggcor_mtcars_final <- mtcars %>%
    ggcorr(label = TRUE,
           label_size = 3,
           label_round = 2,
           label_color = 'white',
           method = c("pairwise", "spearman"),
           nbreaks = 5,
           palette = "RdBu")

ggcor_mtcars_colors <- mtcars %>%
    ggcorr(geom = "blank", label = TRUE) +
    geom_point(size = 10, aes(color = coefficient > 0,
                              alpha = abs(coefficient) > 0.7)) +
    scale_alpha_manual(values = c("TRUE" = 0.50, "FALSE" = 0)) +
    guides(color = FALSE, alpha = FALSE)

ggsave(here::here('figs', 'ggcor_mtcars.png'),
       ggcor_mtcars, width = 6, height = 5)

ggsave(here::here('figs', 'ggcor_mtcars_labels.png'),
       ggcor_mtcars_labels, width = 6, height = 5)

ggsave(here::here('figs', 'ggcor_mtcars_pearson.png'),
       ggcor_mtcars_pearson, width = 6, height = 5)

ggsave(here::here('figs', 'ggcor_mtcars_spearman.png'),
       ggcor_mtcars_spearman, width = 6, height = 5)

ggsave(here::here('figs', 'ggcor_mtcars_final.png'),
       ggcor_mtcars_final, width = 6, height = 5)

ggsave(here::here('figs', 'ggcor_mtcars_colors.png'),
       ggcor_mtcars_colors, width = 6, height = 5)

# ggpairs ------------------------------------------------------------------

# Correlogram

ggpairs_mtcars <- mtcars %>%
    dplyr::select(mpg, cyl, disp, hp, wt) %>%
    ggpairs()

ggpairs_mtcars_classic <- mtcars %>%
    dplyr::select(mpg, cyl, disp, hp, wt) %>%
    ggpairs() +
    theme_classic(base_size = 18)

ggsave(here::here('figs', 'ggpairs_mtcars.png'),
       ggpairs_mtcars, width = 7, height = 6)

ggsave(here::here('figs', 'ggpairs_mtcars_classic.png'),
       ggpairs_mtcars_classic, width = 7, height = 6)

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

mammals <- MASS::mammals

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

federal_spending_summary <- federal_spending %>%
    group_by(department) %>%
    summarise(rd_budget_bil = sum(rd_budget_mil) / 10^3) %>%
    mutate(department = fct_reorder(department, rd_budget_bil))

federal_spending_summary_highlight <- federal_spending_summary %>% 
    mutate(
        department = fct_reorder(department, rd_budget_bil),
        is_dod = if_else(department == 'DOD', TRUE, FALSE))

federal_spending_bars_unsorted <- federal_spending %>%
    group_by(department) %>%
    summarise(rd_budget_bil = sum(rd_budget_mil) / 10^3) %>%
    ggplot() +
    geom_col(aes(x = rd_budget_bil, y = department),
             width = 0.7, alpha = 0.8, fill = 'steelblue') +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid()

federal_spending_bars <- federal_spending_summary %>%
    ggplot() +
    geom_col(aes(x = rd_budget_bil, y = department),
             width = 0.7, alpha = 0.8, fill = 'steelblue') +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid()

federal_spending_bars_top5 <- federal_spending %>%
  mutate( 
    department = fct_other(department, 
      keep = c('DOD', 'HHS', 'NIH', 'NASA', 'DOE'))) %>% 
  group_by(department) %>%
  summarise(rd_budget_bil = sum(rd_budget_mil) / 10^3) %>%
  mutate(department = fct_reorder(department, rd_budget_bil)) %>% 
  ggplot() +
  geom_col(aes(x = rd_budget_bil,
               y = department), 
           width = 0.7, alpha = 0.8,
           fill = "steelblue") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid()

federal_spending_dots <- federal_spending_summary %>%
    ggplot() +
    geom_point(aes(x = rd_budget_bil, y = department),
               size = 2.5, color = 'steelblue') +
    theme_minimal_vgrid()

federal_spending_lollipop <- federal_spending_summary %>%
    ggplot() +
    geom_segment(aes(x = 0, xend = rd_budget_bil,
                     y = department, yend = department),
                 color = 'grey') +
    geom_point(aes(x = rd_budget_bil, y = department),
               size = 2.5, color = 'steelblue') +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid()

federal_spending_bars_highlight_badcolor <- federal_spending_summary_highlight %>% 
    ggplot() +
    geom_col(aes(x = rd_budget_bil, department,
                 fill = is_dod), 
             width = 0.7, alpha = 0.8) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid() +
    theme(legend.position = 'none')

federal_spending_bars_highlight <- federal_spending_summary_highlight %>% 
    ggplot() +
    geom_col(aes(x = rd_budget_bil, department,
                 fill = is_dod), 
             width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = c('grey', 'steelblue')) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid() +
    theme(legend.position = 'none')

federal_spending_bars_highlight_title <- federal_spending_summary_highlight %>% 
    ggplot() +
    geom_col(aes(x = rd_budget_bil, department,
                 fill = is_dod), 
             width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = c('grey', 'steelblue')) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_vgrid() +
    theme(legend.position = 'none') + 
    labs(x = "Total R&D spending from 1976 to 2017 ($Billions)", 
         y = NULL, 
         title = "The DOD's R&D budget is nearly the\nsame as all other departments combined")

ggsave(here('figs', 'federal_spending_bars_unsorted.png'),
       federal_spending_bars_unsorted, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_bars.png'),
       federal_spending_bars, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_bars_top5.png'),
       federal_spending_bars_top5, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_dots.png'),
       federal_spending_dots, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_lollipop.png'),
       federal_spending_lollipop, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_bars_highlight.png'),
       federal_spending_bars_highlight, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_bars_highlight_title.png'),
       federal_spending_bars_highlight_title, width = 6, height = 4)
ggsave(here('figs', 'federal_spending_bars_highlight_badcolor.png'),
       federal_spending_bars_highlight_badcolor, width = 6, height = 4)

# gapminder --------------------------------------------------------

gapminder_americas <- gapminder %>%
    filter(year == 2007, continent == "Americas") %>%
    mutate(country = fct_reorder(country, lifeExp))

life_expectancy_bars <- gapminder_americas %>% 
    ggplot() +
    geom_col(aes(x = lifeExp, y = country),
             fill = 'steelblue', alpha = 0.9) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.05)),
        limits = c(0, 85)) +
    theme_minimal_vgrid() +
    labs(
        x = 'Country',
        y = 'Life expectancy (years)')

life_expectancy_lollipop <- gapminder_americas %>% 
    ggplot() +
    geom_segment(aes(x = 0, xend = lifeExp,
                     y = country, yend = country), 
                 color = 'grey') +
    geom_point(aes(x = lifeExp, y = country),
               color = 'steelblue', alpha = 0.9, size = 2.5) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.05)),
        limits = c(0, 85)) +
    theme_minimal_vgrid() +
    labs(
        x = 'Country',
        y = 'Life expectancy (years)')

life_expectancy_dots <- gapminder_americas %>% 
    ggplot() +
    geom_point(aes(x = lifeExp, y = country),
               color = 'steelblue', size = 2.5) +
    theme_minimal_vgrid() +
    labs(x = 'Country',
         y = 'Life expectancy (years)')

ggsave(here('figs', 'life_expectancy_bars.png'),
       life_expectancy_bars, width = 6, height = 6)
ggsave(here('figs', 'life_expectancy_lollipop.png'),
       life_expectancy_lollipop, width = 6, height = 6)
ggsave(here('figs', 'life_expectancy_dots.png'),
       life_expectancy_dots, width = 6, height = 6)

# milk production --------------------------------------------------------

milk_summary <- milk_production %>%
  filter(year == 2017) %>%
  mutate(
    milk_produced = milk_produced / 10^9,
    state = fct_reorder(state, milk_produced))

milk_bars <- ggplot(milk_summary) +
  geom_col(aes(x = milk_produced, y = state),
           width = 0.7, alpha = 0.8, fill = 'steelblue') +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(x = 'Milk produced (billions lbs)',
       y = 'State')

milk_dots <- ggplot(milk_summary) +
  geom_point(aes(x = milk_produced, y = state),
             size = 2.5, color = 'steelblue') +
  theme_minimal_vgrid() +
  labs(x = 'Milk produced (billions lbs)',
       y = 'State')

milk_lollipop <- ggplot(milk_summary) +
  geom_segment(aes(x = 0, xend = milk_produced, 
                   y = state, yend = state),
               color = 'grey') +
  geom_point(aes(x = milk_produced, y = state),
             size = 2.5, color = 'steelblue') +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(x = 'Milk produced (billions lbs)',
       y = 'State')

milk_2017_bars_stacked <- milk_production %>%
  filter(year == 2017) %>% 
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(state) %>% #<<
  summarise(milk_produced = sum(milk_produced)) %>% 
  ggplot() +
  geom_col(aes(x = "", y = milk_produced, fill = state), #<<
           width = 0.7, alpha = 0.8) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_hgrid() +
  labs(x = NULL,
       y = 'Milk produced (lbs)',
       fill = 'State',
       title = '2017 Milk Production\nby State')

milk_2017_bars_stacked_rotated <- milk_2017_bars_stacked + 
  coord_flip() + 
  labs(title = '2017 Milk Production by State')

milk_2017_bars_stacked_toomany <- milk_production %>%
  filter(year == 2017) %>%
  mutate(state = fct_other(state, #<<
    keep = c('California', 'Wisconsin', #<<
             'New York', 'Idaho'))) %>% #<<
  group_by(state) %>%
  summarise(milk_produced = sum(milk_produced)) %>% 
  ggplot() +
  geom_col(aes(x = "", y = milk_produced, fill = state),
           width = 0.7, alpha = 0.8) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(x = NULL,
       y = 'Milk produced (lbs)',
       fill = 'State',
       title = '2017 Milk Production\nby State')

milk_2017_bars_dodged <- milk_production %>%
  filter(year == 2017) %>%
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(state) %>%
  summarise(milk_produced = sum(milk_produced)) %>%
  mutate(state = fct_reorder(state, milk_produced)) %>% 
  ggplot() +
  geom_col(aes(x = milk_produced, state), #<<
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(x = 'Milk produced (lbs)',
       y = 'State',
       title = '2017 Milk Production by State')
  
milk_compare_bars_stacked <- milk_production %>%
  filter(year %in% c(1970, 2017)) %>% #<<
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(year, state) %>% #<<
  summarise(milk_produced = sum(milk_produced)) %>% 
  ggplot() +
  geom_col(aes(x = milk_produced, 
               y = as.factor(year), #<<
               fill = state),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(x = 'Milk produced (lbs)',
       y = 'Year',
       fill = 'State',
       title = '1970 & 2017 Milk Production by State')

milk_compare_bars_dodged <- milk_production %>%
  filter(year %in% c(1970, 2017)) %>% #<<
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(year, state) %>% #<<
  summarise(milk_produced = sum(milk_produced)) %>% 
  ggplot() +
  geom_col(aes(x = milk_produced,
               y = as.factor(year),
               fill = state),
           position = 'dodge', #<<
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid() +
  labs(x = 'Milk produced (lbs)',
       y = 'Year',
       fill = 'State',
       title = '1970 & 2017 Milk Production by State')

# Format the data
milk_waffle_2017 <- milk_production %>%
  filter(year == 2017) %>%
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(state) %>%
  summarise(milk_produced = sum(milk_produced)) %>%
  mutate(milk_produced = milk_produced / 10^9) %>%
  ggplot() +
  geom_waffle( #<<
    aes(fill = state, values = milk_produced), #<<
    color = "white", size = 1, n_rows = 15) + #<<
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal() +
  labs(fill = 'State',
       x = NULL, y = NULL,
       title = '2017 Milk Production by State',
       subtitle = '(1 square = 1 billion lbs)')

ggsave(here('figs', 'milk_bars.png'),
       milk_bars, width = 6, height = 5)
ggsave(here('figs', 'milk_dots.png'),
       milk_dots, width = 6, height = 5)
ggsave(here('figs', 'milk_lollipop.png'),
       milk_lollipop, width = 6, height = 5)
ggsave(here('figs', 'milk_2017_bars_stacked.png'),
       milk_2017_bars_stacked, width = 4, height = 6)
ggsave(here('figs', 'milk_2017_bars_stacked_rotated.png'),
       milk_2017_bars_stacked_rotated, width = 7, height = 3.5)
ggsave(here('figs', 'milk_2017_bars_stacked_toomany.png'),
       milk_2017_bars_stacked_toomany, width = 4, height = 6)
ggsave(here('figs', 'milk_2017_bars_dodged.png'),
       milk_2017_bars_dodged, width = 7, height = 3.5)
ggsave(here('figs', 'milk_compare_bars_stacked.png'),
       milk_compare_bars_stacked, width = 7, height = 3.5)
ggsave(here('figs', 'milk_compare_bars_dodged.png'),
       milk_compare_bars_dodged, width = 7, height = 4)
ggsave(here('figs', 'milk_waffle_2017.png'),
       milk_waffle_2017, width = 5, height = 4)

# lotr_words -----------------------------------------------------------

lotr_bars <- lotr_words %>%
  gather(key = 'gender', value = 'wordCount',
         Female:Male) %>% 
  ggplot() +
  geom_col(aes(x = wordCount, y = Film),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid(font_size = 18)

lotr_bars_relevel <- lotr_words %>%
  gather(key = 'gender', value = 'wordCount',
         Female:Male) %>% 
  mutate( 
    Film = fct_relevel(Film, c( 
      'The Fellowship Of The Ring', 
      'The Two Towers', 
      'The Return Of The King'))) %>% 
  ggplot() +
  geom_col(aes(x = wordCount, y = Film),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid(font_size = 18)

lotr_bars_recode <- lotr_words %>%
  gather(key = 'gender', value = 'wordCount',
         Female:Male) %>% 
  mutate( #<<
    Film = fct_recode(Film, #<<
      'The Fellowship\nof the Ring' = 'The Fellowship Of The Ring', #<<
      'The Return\nof the King' = 'The Return Of The King')) %>% #<< 
  ggplot() +
  geom_col(aes(x = wordCount, y = Film),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid(font_size = 18)

lotr_bars_relevel_recode <- lotr_words %>%
  gather(key = 'gender', value = 'wordCount',
         Female:Male) %>% 
  mutate(
    Film = fct_relevel(Film, c( 
      'The Fellowship Of The Ring', 
      'The Two Towers', 
      'The Return Of The King')),
    Film = fct_recode(Film, 
      'The Fellowship\nof the Ring' = 'The Fellowship Of The Ring', 
      'The Return\nof the King' = 'The Return Of The King')) %>% 
  ggplot() +
  geom_col(aes(x = wordCount, y = Film),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid(font_size = 18)

ggsave(here('figs', 'lotr_bars.png'),
       lotr_bars, width = 7, height = 4)
ggsave(here('figs', 'lotr_bars_relevel.png'),
       lotr_bars_relevel, width = 7, height = 4)
ggsave(here('figs', 'lotr_bars_recode.png'),
       lotr_bars_recode, width = 7, height = 4)
ggsave(here('figs', 'lotr_bars_relevel_recode.png'),
       lotr_bars_relevel_recode, width = 7, height = 4)

# avengers --------------------------------------------------------------

avengers_bars <- avengers %>%
  mutate(name_alias =
         fct_reorder(name_alias, appearances)) %>%
  ggplot() +
  geom_col(aes(x = appearances, y = name_alias),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid()

avengers_bars_top10 <- avengers %>%
  mutate(name_alias =
         fct_reorder(name_alias, appearances)) %>%
  arrange(desc(appearances)) %>% #<<
  slice(1:10) %>% #<<
  ggplot() +
  geom_col(aes(x = appearances, y = name_alias),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid(font_size = 18)

avengers_bars_top10_gender <- avengers %>%
  mutate(name_alias =
         fct_reorder(name_alias, appearances)) %>%
  arrange(desc(appearances)) %>% #<<
  group_by(gender) %>% #<<
  slice(1:10) %>% #<<
  ggplot() +
  geom_col(aes(x = appearances, 
               y = name_alias,
               fill = gender),
           width = 0.7, alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_vgrid(font_size = 18)

ggsave(here('figs', 'avengers_bars.png'),
       avengers_bars, width = 8, height = 8)
ggsave(here('figs', 'avengers_bars_top10.png'),
       avengers_bars_top10, width = 8, height = 8)
ggsave(here('figs', 'avengers_bars_top10_gender.png'),
       avengers_bars_top10_gender, width = 8, height = 8)

# prop ----

# comparisons ----

# maps ----

# Filter out coffee shops to continental 48 states
us_coffee_shops <- us_coffee_shops %>%
  filter(lat > 22,    lat < 50,
         long > -150, long < -66)

# Projections 

# Albers
# "ESRI:102003"

# Robinson
# "ESRI:54030"
# "+proj=robin"

# https://ggplot2-book.org/maps.html

## Polygon maps ----
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

## Simple features maps ----

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

## China ----

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

## Read shape file ----

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

## Projections Polygons ----

us_states <- map_data("state")
polygon_us <- ggplot(us_states,
                     aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "grey60")

polygon_us +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


## Projections sf ----

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

## Choropleth milk ----

# https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

# US States, continental data
us_states <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii')) %>% 
  left_join(milk_2017, by = 'name')

# Milk fill
sf_us_milk_2017 <- ggplot(us_states) +
  geom_sf(aes(fill = milk_produced)) +
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
  geom_sf(aes(fill = milk_produced)) +
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

## Choropleth internet ----

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


## Bubble map - England ----

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

## Bubble map squirrels ----

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

## Coffee maps ----

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

## State centroid map ----

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


## Hexbin map ----

# https://www.r-graph-gallery.com/hexbin-map.html
# Also statebins: http://socviz.co/maps.html#maps



## OTHER ----



## sf & spatial ----
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html


## andrew - world projections ----
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

## internet animation ----
# https://ourworldindata.org/internet

## Map insets ----
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

## Polygon election ----
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

## Polygon population ----
# http://socviz.co/maps.html#maps

county_full <- county_map %>%
  left_join(county_data, by = "id") %>%
  mutate(
    pop_dens = fct_recode(
      pop_dens,
      "0-10"        = "[    0,   10)",
      "10-50"       = "[   10,   50)",
      "50-100"      = "[   50,  100)",
      "100-500"     = "[  100,  500)",
      "500-1,000"   = "[  500, 1000)",
      "1,000-5,000" = "[ 1000, 5000)",
      ">5,000"      = "[ 5000,71672]"
    )
  )

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


## Animation reprex ----

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

mpg_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point(
        aes(color = as.factor(cyl)),
        alpha = 0.8, size = 3) +
    theme_half_open(font_size = 16) +
    labs(x = "Fuel efficiency (mpg)",
         y = "Power (hp)",
         color = '# cylinders',
         title = "Vehicle fuel efficiency vs. power",
         caption = "Source: 1974 Motor Trend U.S. magazine.")
mpg_plot_mycolors <- mpg_plot +
    theme_half_open(font_size = 10) +
    scale_color_manual(values = c(
        '#a0522d', '#522da0', '#2da052'))
mpg_plot_mycolors <- cvd_grid(mpg_plot_mycolors)

ggsave(
    here::here('figs', 'mpg_plot_mycolors.png'),
    mpg_plot_mycolors, width = 8, height = 7
)

# anim ----

# interact  ----

