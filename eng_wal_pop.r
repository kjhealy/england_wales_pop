### --------------------------------------------------
### Population of England and Wales, 1961-2014
###------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(gtable)
library(viridis)

## Colors
bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
        "#009E73", "#F0E442", "#D55E00", "#CC79A7")

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

## Nice alignment solution for the STL plots, from Baptiste Auguié
## http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot/22984913
## (Probably don't really need this approach anymore, but I know it works.)
rbind_gtable_max <- function(...) {

    gtl <- list(...)
    stopifnot(all(sapply(gtl, gtable::is.gtable)))
    bind2 <- function(x, y) {
        stopifnot(ncol(x) == ncol(y))
        if (nrow(x) == 0)
            return(y)
        if (nrow(y) == 0)
            return(x)
        y$layout$t <- y$layout$t + nrow(x)
        y$layout$b <- y$layout$b + nrow(x)
        x$layout <- rbind(x$layout, y$layout)
        x$heights <- gtable:::insert.unit(x$heights, y$heights)
        x$rownames <- c(x$rownames, y$rownames)
        x$widths <- grid::unit.pmax(x$widths, y$widths)
        x$grobs <- append(x$grobs, y$grobs)
        x
    }

    Reduce(bind2, gtl)
}

draw_stl <- function(data_stl = data,
                     bar.width = 0.5,
                     p_col = "gray30") {
    theme_set(theme_minimal())

    break_vec <- seq(from=as.Date("1938-01-01"), to=as.Date("1991-12-01"), by="year")
    break_vec <- break_vec[seq(1, 80, 5)]

    p <- ggplot(data_stl, aes(x = date, y = births_pct_day))
    p1 <- p + geom_line(color = p_col) + ylab("Data") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)), plot.title = element_text(size = rel(1))) +
        ggtitle("England and Wales, Births per month per million people, 1938-1991")

    p <- ggplot(data_stl, aes(x = date, y = trend))
    p2 <- p + geom_line(color = p_col) + ylab("Trend") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data_stl, aes(x = date, y = seasonal))
    p3 <- p + geom_line(color = p_col) + ylab("Seasonal") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data_stl, aes(x = date, ymax = remainder, ymin = 0))
    p4 <- p + geom_linerange(size = bar.width, color = p_col) +
        scale_x_date(breaks = break_vec, date_labels = "%Y") +
        ylab("Remainder") + xlab("Year") +
        theme(axis.title.y = element_text(size = rel(0.8)))

    g1 <- ggplotGrob(p1)
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    g4 <- ggplotGrob(p4)
    out <- rbind_gtable_max(g1, g2, g3, g4)
    out
}



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
### Data on live births
###--------------------------------------------------

pop <- read_csv("data/england_wales_population_1838_2014.csv")
pop <- janitor::clean_names(pop)

births <- read_csv("data/england_wales_live_births_by_month_1938_1991.csv")

births_lon <- births %>% gather(month, count, -Year) %>%
    mutate(tmp_dt = paste(Year, month, "01", sep = "-"),
           date = lubridate::ymd(tmp_dt)) %>%
    select(-tmp_dt, -month) %>%
    arrange(date)

births_lon <- janitor::clean_names(births_lon)


days <- read_csv("data/days_of_month.csv")
days_lon <- gather(days, year, n_days, -Month)
days_lon <- janitor::clean_names(days_lon)
days_lon$year <- as.integer(days_lon$year)
days_lon <- days_lon %>%
    mutate(tmp_dt = paste(year, month, "01", sep = "-"),
           date = lubridate::ymd(tmp_dt)) %>%
    select(-tmp_dt)

births_lon <- left_join(births_lon, days_lon)
births_lon <- left_join(births_lon, pop)

births_lon <- births_lon %>%
    mutate(births_pct = count / persons,
           births_pct_day = (births_pct / n_days) * 1e6)

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

## use case_when to change to negative for males only
ages_pyr <- ages_pyr %>% 
  mutate(pct = case_when(
    group == "Males" ~ -pct,
    TRUE ~ pct
  ))

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



###--------------------------------------------------
### Birth plots
###--------------------------------------------------


data_stl <- stl(ts(births_lon$births_pct_day, start = c(1938, 1), frequency = 12), s.window = "periodic")
stl_df <- data.frame(data_stl$time.series)

births_lon <- cbind(births_lon, stl_df)


###--------------------------------------------------
### Plots
###--------------------------------------------------

theme_set(theme_minimal())

break_vec <- seq(from = as.Date("1938-01-01"), to = as.Date("1991-12-01"), by = "year")
break_vec <- break_vec[seq(1, 55, 5)]


## Trend line plot
p <- ggplot(births_lon, aes(x = date, y = births_pct_day))
p_line <- p + geom_line(size = 0.6, color = "gray30") +
    scale_x_date(breaks = break_vec, date_labels = "%Y") +
    labs(x = "Date", y = "Monthly Births per Million Population",
         title = "England & Wales, Average Monthly Births, 1938-1991.",
                       caption = "Data: UK ONS")

png("figures/births_monthly_line.png", width = 1600, height = 480, res = 100)
print(p_line)
dev.off()

pdf("figures/births_monthly_line.pdf", width = 9, height = 4)
print(p_line)
dev.off()

## Time series decomposition plot
pdf("figures/births_monthly_stl.pdf", width = 12, height = 7)
out <- draw_stl(data = births_lon, bar.width = 0.5)
grid.draw(out)
dev.off()

png("figures/births_monthly_stl.png", width = 1600, height = 600)
out <- draw_stl(data = births_lon, bar.width = 0.5)
grid.draw(out)
dev.off()


## Tiled monthly plot
p <- ggplot(births_lon,
            aes(y = factor(month,
                           levels = c(12:1),
                           labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           ordered = TRUE),
                x = factor(year)))
p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") + labs(x = "", y = "") +
    scale_x_discrete(breaks = seq(1938, 1991, 5)) +
    scale_fill_viridis(option = "inferno") +
    theme(legend.position = "right", plot.caption = element_text(size = 6)) +
    labs(x = "Year", fill = "", title = "England & Wales, Monthly Birth Rate 1938-1991",
         subtitle = "Average births per day per million people.",
         caption = "Kieran Healy (kieranhealy.org). Data: UK ONS.")


png("figures/births_monthly_tile.png", width = 1600, height = 320, res = 100)
print(p_tile)
dev.off()

pdf("figures/births_monthly_tile.pdf", width = 12, height = 3.5)
print(p_tile)
dev.off()



## Tiled monthly plot, vertical
p <- ggplot(births_lon,
            aes(x = factor(month,
                           levels = c(12:1),
                           labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           ordered = TRUE),
                y = factor(year, levels = c(1991:1938), ordered = TRUE)))

p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") + labs(x = "", y = "") +
    scale_x_discrete(breaks = seq(1938, 1991, 5), position = "top") +
    scale_fill_viridis(option = "inferno") +
    theme(legend.position = "top",
          plot.title = element_text(size = rel(0.8)),
          plot.subtitle = element_text(size = rel(0.6)),
          plot.caption = element_text(size = rel(0.4)),
          axis.text.y = element_text(size = rel(1)),
          axis.text.x = element_text(size = rel(0.7))) +
    labs(x = "", fill = "", title = "England & Wales, Monthly Births 1938-1991",
         subtitle = "Average births per day per million people.",
         caption = "Kieran Healy (kieranhealy.org). Data: UK ONS.")


pdf("figures/births_monthly_tile_vert.pdf", height = 15, width = 3.75)
print(p_tile)
dev.off()


png("figures/births_monthly_tile_vert.png", height = 1300, width = 320, res = 100)
print(p_tile)
dev.off()


uk_births_lon <- births_lon %>%
    select(year, month, n_days, count, persons, births_pct,
           births_pct_day, date, seasonal, trend, remainder) %>%
    mutate(country = "England and Wales") %>%
    rename(births = count, total_pop = persons)

save(uk_births_lon, file = "data/uk_births_lon.rda")
