### --------------------------------------------------
### Population of England and Wales, 1961-2014
###------------------------------------------------------------

library(tidyverse)
library(gganimate)

## Colors
bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
        "#009E73", "#F0E442", "#D55E00", "#CC79A7")

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)


###--------------------------------------------------
### Data
###
### From the UK ONS.  https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/004358englandandwalespopulationestimates1838to2014
###--------------------------------------------------


ages <- read_csv("data/england_wales_mf_age_1961_2014.csv")

## Tidy
ages_lon <- gather(ages, year, count, `Mid-1961`:`Mid-2014`)

## Extract the totals
ages_all <- ages_lon %>% filter(Age == "All Ages") %>%
    rename(total = count) %>%
    select(-Age)

## Remove the totals from the main table
ages_lon <- ages_lon %>% filter(Age != "All Ages") %>%
    mutate(Age = as.integer(Age))

## Clean the names
ages_all <- janitor::clean_names(ages_all)
ages_lon <- janitor::clean_names(ages_lon)

## Put the totals back in as their own colmn, so we can calculate the
## percentages
ages_lon <- left_join(ages_lon, ages_all)

ages_lon <- ages_lon %>%
    mutate(pct = (count/total) * 100,
           yr = as.integer(stringr::str_extract(year, "\\d{4}")))

###--------------------------------------------------
### Approximate median ages by sex
### (Not used below, just having a look)
###--------------------------------------------------
ages_med <- ages_lon %>% group_by(group, yr) %>%
    mutate(cum_pct = cumsum(pct)) %>%
    filter(abs(cum_pct - 50) == min(abs(cum_pct - 50))) %>%
    rename(med_age = age) %>%
    select(group, med_age, yr)


###--------------------------------------------------
### Plots
###--------------------------------------------------

## Quick look at the males post 1970 as a small multiple
p <- ggplot(data = subset(ages_lon, yr > 1970 & group == "Males"),
            mapping = aes(x = age,
                          y = pct,
                          color = group,
                          fill = group))

p_out <- p + geom_area(alpha = 0.3) +
    scale_color_manual(values = bly_palette[3]) +
    scale_fill_manual(values = bly_palette[3]) +
    scale_x_continuous(breaks = seq(10, 80, 10)) +
    guides(fill = FALSE, color = FALSE) +
    facet_wrap(~ yr) + theme_minimal()


pdf(file = "figures/eng_wal_age.pdf", height = 10, width = 20)
print(p_out)
dev.off()

###--------------------------------------------------
### Population Pyramids
###--------------------------------------------------

ages_lon$base <-  0 # For some reason geom_area doesn't work, so we'll
                    # use geom_ribbon instead

ages_pyr <- ages_lon

## Manually make the male scores negative
ages_pyr$pct[ages_pyr$group == "Males"] <- -ages_lon$pct[ages_lon$group == "Males"]


## First just do one. E.g., 1968
p <- ggplot(data = subset(ages_pyr, yr == 1968),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

p_pyr <- p + geom_ribbon(alpha = 0.5) +
    scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
    scale_x_continuous(breaks = seq(10, 80, 10)) +
    scale_fill_manual(values = bly_palette) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(x = "Age", y = "Percent of Population",
         title = "Age Distribution of the Population of England and Wales: 1968",
         subtitle = "Age is top-coded at 85.",
         caption = "Kieran Healy / kieranhealy.org / Data: UK ONS.",
         fill = "Group") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = rel(0.8), face = "bold"),
          plot.subtitle = element_text(size = rel(0.8)),
          plot.caption = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.9)),
          axis.text.x = element_text(size = rel(0.9))) +
    coord_flip()

pdf(file = "figures/eng-wal-pyr-1968.pdf", width = 7, height = 10)
print(p_pyr)
dev.off()

## Now the whole animation
p <- ggplot(data = ages_pyr,
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group,
                          frame = yr))

p_pyr_ani <- p + geom_ribbon(alpha = 0.5) +
    scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
    scale_x_continuous(breaks = seq(10, 80, 10)) +
    scale_fill_manual(values = bly_palette) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(x = "Age", y = "Percent of Population",
         title = "Age Distribution of the Population of England and Wales:",
         subtitle = "Age is top-coded at 85 before 1971 and 90 thereafter.",
         caption = "Kieran Healy / kieranhealy.org / Data: UK ONS.",
         fill = "Group") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = rel(0.8), face = "bold"),
          plot.subtitle = element_text(size = rel(0.8)),
          plot.caption = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.9)),
          axis.text.x = element_text(size = rel(0.9))) +
    coord_flip()

## This will take a while to run.
## ani.res option needs a relatively recent version of the animate library
gganimate(p_pyr_ani, filename = "figures/eng-wa-pop-pyr.gif",
          ani.width = 1000, ani.height = 1600, ani.res = 200)




### Marching labels

ww1m_labs <- data.frame(yr = 1961:2008, age = 43:90,
                        lab = "Born 1918", base = 0,
                        group = "Males", gen = "ww1m")

ww1m_labs <- left_join(ww1m_labs, ages_pyr)

ww1m_labs <- ww1m_labs %>% rename(y = pct) %>%
    mutate(y = y - 0.05,
           yend = y - 0.025)


ww2m_labs <- data.frame(yr = 1961:2014, age = 14:67,
                       lab = "Born 1947",
                       base = 0, group = "Males", gen = "ww2m")
ww2m_labs <- left_join(ww2m_labs, ages_pyr)


ww2m_labs <- ww2m_labs %>% rename(y = pct) %>%
    mutate(y = y - 0.05,
           yend = y - 0.025)



xm_labs <- data.frame(yr = 1977:2014, age = 0:37,
                       lab = "Born 1977",
                       base = 0, group = "Males", gen = "x70m")

xm_labs <- left_join(xm_labs, ages_pyr)


xm_labs <- xm_labs %>% rename(y = pct) %>%
    mutate(y = y - 0.05,
           yend = y - 0.025)

ww1f_labs <- data.frame(yr = 1961:2008, age = 41:88,
                       lab = "Born 1920",
                       base = 0, group = "Females", gen = "ww1f")


ww1f_labs <- left_join(ww1f_labs, ages_pyr)

ww1f_labs <- ww1f_labs %>% rename(y = pct) %>%
    mutate(y = y + 0.3,
           yend = y + 0.3)



x64f_labs <- data.frame(yr = 1964:2014, age = 0:50,
                       lab = "Born 1964",
                       base = 0, group = "Females", gen = "ww2")


x64f_labs <- left_join(x64f_labs, ages_pyr)

x64f_labs <- x64f_labs %>% rename(y = pct) %>%
    mutate(y = y + 0.3,
           yend = y + 0.3)


gen_labs <- rbind(ww1m_labs, ww2m_labs, xm_labs, ww1f_labs, x64f_labs)


p <- ggplot(data = ages_pyr,
            mapping = aes(x = age,
                          frame = yr))

p_pyr_ani <- p + geom_ribbon(alpha = 0.5, mapping = aes(ymin = base, ymax = pct, fill = group)) +
    geom_text(data = gen_labs,
              mapping = aes(x = age, y = y, label = lab),
              size = rel(1.8), hjust = 1) +
    scale_y_continuous(labels = abs, limits = max(ages_lon$pct + 0.1, na.rm = TRUE) * c(-1,1)) +
    scale_x_continuous(breaks = seq(10, 80, 10)) +
    scale_fill_manual(values = bly_palette) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(x = "Age", y = "Percent of Population",
         title = "Age Distribution of the Population of England and Wales:",
         subtitle = "Age is top-coded at 85 before 1971 and 90 thereafter.",
         caption = "Kieran Healy / kieranhealy.org / Data: UK ONS.",
         fill = "Group") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = rel(0.8), face = "bold"),
          plot.subtitle = element_text(size = rel(0.8)),
          plot.caption = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.9)),
          axis.text.x = element_text(size = rel(0.9))) +
    coord_flip()

## This will take a while to run.
## ani.res option needs a relatively recent version of the animate library
gganimate(p_pyr_ani, filename = "figures/eng-wa-pop-pyr-labs.gif",
          ani.width = 1000, ani.height = 1600, ani.res = 200)
