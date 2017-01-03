######################################################
# 0. loading data, setting up ########################
######################################################

install.packages('tidyverse') 
install.packages('abind') # also need to install abind
install.packages('MASS') # same
install.packages('readr') # last one - this one reads CSV files faster and has some useful defaults, though it is not necessary

library(tidyverse) # after you can use library(tidyverse) when you want to use it

setwd('~/dropbox/1_research/generality') # change this to the folder with the data in it

data <- readr::read_csv("all_observations.csv")
data # need `` symbols around the names since can't start a variable name with a #

# this is because `7C_8-3` and `7C_12-2` have no 3.1 obs - will remove later
data <- rbind(data, c(NA, '3.1', '3.1', NA))
data

data[, 2:4] <- sapply(data[, 2:4], function(x) floor(as.numeric(x)))
data

##################################################################
# 1. look at likelihood of profile (using all data & six codes) #
##################################################################

to_prep <- tidyr::gather(data, key, val, -id)

to_plot <- to_prep %>% 
    group_by(key, val) %>% 
    summarize(n = n()) %>% 
    filter(!is.na(val)) %>% 
    tidyr::spread(val, n, fill = 0) %>% 
    tidyr::gather(Code, the_val, -key) %>% 
    group_by(key) %>% 
    mutate(the_val = as.numeric(the_val),
           the_val = the_val / sum(the_val))

to_plot$key <- factor(to_plot$key,
                      levels = c("7C_8-3", "7C_12-2", "8B_6-2"))

to_plot$the_key <- factor(to_plot$Code)

to_plot <- to_plot %>% arrange(key, Code)

t1_freq <- as.vector(table(data$`7C_8-3`))
t2_freq <- as.vector(table(data$`7C_12-2`))
t3_freq <- as.vector(table(data$`8B_6-2`))
the_mat <- cbind(t1_freq, t2_freq, t3_freq)
row.names(the_mat) <- sort(unique(data$`8B_6-2`))
colnames(the_mat) <- names(data)[2:4]
chi_sq_output <- chisq.test(the_mat) # this throws a warning (not an error) because some cells have few (< 5) cases; it's probably okay as long as we interpret those cells with caution

round(chi_sq_output$stdres, 3)  # here are the z-scores (> 1.96 means cell is more likely than expected; < 1.96 means cell is less likely than expected)


to_plot$sig <- ifelse(as.vector(chi_sq_output$stdres) >= 1.96, "+",
                      ifelse(as.vector(chi_sq_output$stdres) <= -1.96, "-",
                             "=")) # replace "=" w/ NA

ggplot(to_plot, aes(x = key, y = the_val, color = Code, group = Code, label = sig)) +
    geom_point(size = 1.5) +
    geom_line() +
    ggrepel::geom_label_repel(show.legend = F) +
    #ggrepel::geom_text_repel() +
    ylab("Proportion of Responses") +
    xlab(NULL) +
    theme(text = element_text(size = 14)) +
    theme(legend.title = element_blank()) +
    labs(
        title = "Number of Responses by Code for All Observations",
        caption = "Note. +, -, and = labels indicate code is more likely than expected evaluated using a chi-square test of proportions."
    ) +
    theme(plot.caption = element_text(size = 10, family = "Times"))

ggsave("code.png", width = 6, height = 6)

######################################################
# 2. look at likelihood of shifts ####################
######################################################

tab1 <- table(data$`7C_8-3`, data$`7C_12-2`)
tab2 <- table(data$`7C_12-2`, data$`8B_6-2`)

arr <- abind::abind(tab1, tab2, along = 3)
names(dimnames(arr)) <- c("first_code", "second_code", "shift")
dimnames(arr)[[3]] = c("shift_1", "shift_2")

m.sat <- MASS::loglm( ~ first_code + second_code + shift, arr)
m.sat_out <- as.data.frame(resid(m.sat))

df1 <- data.frame(tab1)
df2 <- data.frame(tab2)

df1 <- mutate(df1, shift = "shift_1")
df2 <- mutate(df2, shift = "shift_2")

df <- bind_rows(df1, df2)
names(df) <- c('first_code', 'second_code', "n", "shift")
df_out <- df %>% arrange(shift, second_code)

out_out <- m.sat_out %>% 
    gather(key, val) %>% 
    mutate(code_1 = rep(c(0:5), 12)) %>% 
    unite(united, code_1, key)

df <- bind_cols(df_out, out_out)
df <- select(df, first_code, second_code, val, shift)
df <- unite(df, code, first_code, second_code, sep = "-")

out <- df %>% 
    filter(val >= 1.96 | val <= -1.96) %>% 
    mutate(sig = ifelse(val > 1.96, "+",
                        ifelse(val < -1.96, "-", NA))) %>% 
    select(-val) %>% 
    arrange(shift, code)

to_plot <- data %>% 
    gather(key, val, -id) %>% 
    mutate(val = factor(val)) %>% 
    filter(!is.na(val)) %>% 
    count(key, val) %>% 
    mutate(prop = round(n / sum(n), 3))

to_plot$key <- factor(to_plot$key,
                       levels = c("7C_8-3", "7C_12-2", "8B_6-2"))

to_plot$key <- ifelse(to_plot$key == "7C_8-3", "Time 1",
                      ifelse(to_plot$key == "7C_12-2", "Time 2", "Time 3"))

to_plot$val <- factor(to_plot$val,
                      levels = 
                      c("5", "4", "3", "2", "1", "0"))

# Found likelihoods using this (very clunky):
# to_plot %>% 
#     arrange(key) %>% 
#     group_by(key) %>% 
#     mutate(new_prop = cumsum(prop),
#            new_loc = (new_prop - (prop / 2)))

ggplot(to_plot, aes(x = key, y = prop, fill = val, width = .625)) +
    geom_col(position = 'stack') +
    xlab(NULL) +
    ylab("Proportion of Responses") +
    xlab(NULL) +
    theme(text = element_text(size = 15)) +
    theme(legend.title = element_blank()) +
    theme(plot.caption = element_text(size = 11, family = "Times")) +
    annotate("segment", x = 1, xend = 2, y = .049, yend = .015, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", linetype = "dashed", x = 1, xend = 2, y = .207, yend = .505, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", linetype = "dashed", x = 1, xend = 2, y = .207, yend = .626, arrow=arrow(ends = "last", length=unit(.2,"cm"))) + 
    annotate("segment", x = 1, xend = 2, y = .478, yend = .505, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", x = 1, xend = 2, y = .701, yend = .626, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", linetype = "dashed", x = 1, xend = 2, y = .705, yend = .819, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", linetype = "dashed", x = 1, xend = 2, y = .873, yend = .221, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", x = 2, xend = 3, y = .221, yend = .180, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", x = 2, xend = 3, y = .221, yend = .553, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment",  x = 2, xend = 3, y = .221, yend = .805, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", linetype = "dashed", x = 2, xend = 3, y = .505, yend = .180, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", linetype = "dashed", x = 2, xend = 3, y = .505, yend = .398, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", x = 2, xend = 3, y = .626, yend = .553, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    annotate("segment", x = 2, xend = 3, y = .819, yend = .805, arrow=arrow(ends = "last", length=unit(.2,"cm"))) +
    labs(
        title = "Shifts Between Codes for All Observations",
        caption = "Note. Solid lines indicate shift is more likely than (and dashed lines indicate shift is less likely) than expected as evaluated using log linear models"
    )
ggsave("shift.png", width = 9, height = 9.25)