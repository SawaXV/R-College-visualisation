library(ggplot2)
library(dplyr)
library(scales)
library(fmsb)
library(cowplot)

# Dataset
dataset <- read.csv("CollegeDistance.csv")
dataset <- na.omit(dataset) # clean any empties  
dataset <- subset(dataset, select = -c(1)) # rownames column has an issue with with random increments mid data

# Format all data -> Appropriate formatting for labels for increased readability
## gender
dataset$gender[dataset$gender == "male"] <- "Male"
dataset$gender[dataset$gender == "female"] <- "Female"
## ethnicity
dataset$ethnicity[dataset$ethnicity == "other"] <- "Other"
dataset$ethnicity[dataset$ethnicity == "afam"] <- "African American"
dataset$ethnicity[dataset$ethnicity == "hispanic"] <- "Hispanic"
## parent college
dataset$mcollege[dataset$mcollege == "yes"] <- "Yes"
dataset$fcollege[dataset$fcollege == "yes"] <- "Yes"
dataset$mcollege[dataset$mcollege == "no"] <- "No"
dataset$fcollege[dataset$fcollege == "no"] <- "No"
## home
dataset$home[dataset$home == "yes"] <- "Yes"
dataset$home[dataset$home == "no"] <- "No"
## urban
dataset$urban[dataset$urban == "yes"] <- "Yes"
dataset$urban[dataset$urban == "no"] <- "No"
## income
dataset$income[dataset$income == "high"] <- ">$25k" # value based on variables.txt
dataset$income[dataset$income == "low"] <- "<$25k"
## region
dataset$mcollege[dataset$mcollege == "other"] <- "Other"
dataset$fcollege[dataset$fcollege == "west"] <- "West"

# COLOUR PALETTES
## GRADUATES (#4874bf) / NON-GRADUATES (#f44242) - Provides good constrasts, individualises
## WEALTHY (#eda323) / PARTIALLY WEALTHY (#f4cd2a) / NOT WEALTHY (#f7d551) - Scaling colours, gold represents wealth
## AFRICAN-AMERICAN (#4c1a4c) / HISPANIC (#1a1a4c) / OTHER (#4c4c1a) - Good contrasts against each other, made darker to constrast with scatter points used later

# --- Q1. Parent graduates ---
## new column to establish families with and without graduates
dataset <- dataset %>% mutate(
        pgraduate = case_when(
            mcollege == "No" & fcollege == "No" ~ "No graduate parents",
            TRUE ~ "Graduate parents"
        )
)

graduateGraph <- ggplot(dataset, aes(x = pgraduate, y = after_stat(count)/sum(after_stat(count)), fill=pgraduate)) + 
geom_bar(width = 0.5) +
theme_bw() +
labs(title = "Students with college graduate parents", 
    x = "Parent graduate", 
    y = "Student total (%)",
    fill = "Parent graduate") +
scale_fill_manual(values = c("#4874bf", "#f44242")) +
theme(plot.title = element_text(size = 20)) +
geom_text(aes(label = scales::percent(after_stat(count)/sum(after_stat(count))),
y = after_stat(count)/sum(after_stat(count))), stat = "count", vjust = -.5) +
scale_y_continuous(labels = scales::percent)


# --- Q2. Parent graduates and their wealth ---
## create new columns for wealth groups
dataset <- dataset %>% mutate(
        wealth = case_when(
            home == "Yes" & income == ">$25k" ~ "Wealthy",
            (home == "Yes" & income == "<$25k") | (home == "No" & income == ">$25k")  ~ "Partially wealthy",
            home == "No" & income == "<$25k" ~ "Not wealthy"
        )
)

## wealth statuses
graduateOnlyDataset <- subset(dataset, pgraduate == "Graduate parents")
graduateWealthGraph <- ggplot(graduateOnlyDataset, aes(x = wealth, y = after_stat(count)/sum(after_stat(count)), fill = wealth)) + 
geom_bar(width = 0.5) +
theme_bw() +
labs(title = "Wealth of graduate parents based on home ownership and income", 
    x = "Wealth", 
    y = "Parent total (%)",
    fill = "Wealth") +
scale_fill_manual(values = c("#f7d551", "#f4cd2a", "#eda323")) +
theme(plot.title = element_text(size = 20)) +
geom_text(aes(label = scales::percent(after_stat(count)/sum(after_stat(count)))),
            stat = "count",
            colour = "white",
            position = position_stack(vjust = 0.5)) +
scale_y_continuous(labels = scales::percent, limits = c(0,1))


# --- Q3. Parent non-graduates and their wealth ---
## wealth statuses
ngraduateOnlyDataset <- subset(dataset, pgraduate == "No graduate parents")
ngraduateWealthGraph <- ggplot(ngraduateOnlyDataset, aes(x = wealth, y = after_stat(count)/sum(after_stat(count)), fill = wealth)) + 
geom_bar(width = 0.5) +
theme_bw() +
labs(title = "Wealth of non-graduate parents based on home ownership and income", 
    x = "Wealth", 
    y = "Parent total (%)",
    fill = "Wealth") +
scale_fill_manual(values = c("#f7d551", "#f4cd2a", "#eda323")) +
theme(plot.title = element_text(size = 20)) +
geom_text(aes(label = scales::percent(after_stat(count)/sum(after_stat(count)))),
            stat = "count",
            colour = "white",
            position = position_stack(vjust = 0.5)) +
scale_y_continuous(labels = scales::percent, limits = c(0,1))

# faceted version
fgraduateWealthGraph <- ggplot(dataset, aes(x = wealth, y = after_stat(count)/sum(after_stat(count)), fill = wealth)) + 
geom_bar(width = 0.5) +
theme_bw() +
labs(title = "Wealth of non-graduate vs. graduate parents \nbased on home ownership and income", 
    x = "Wealth", 
    y = "Parent total (%)",
    fill = "Wealth") +
scale_fill_manual(values = c("#f7d551", "#f4cd2a", "#eda323")) +
theme(plot.title = element_text(size = 20)) +
geom_text(aes(label = scales::percent(after_stat(count)/sum(after_stat(count)))),
            stat = "count",
            colour = "white",
            position = position_stack(vjust = 0.5)) +
facet_grid(rows = vars(pgraduate)) +
scale_y_continuous(labels = scales::percent_format(), limits = c(0,1))


# --- Q4. Parent graduate and wealth on composite scores ---
## how scores compare against wealth
wealthScoreGraph <- ggplot(dataset, aes(x=wealth, y=score, fill=wealth)) + 
geom_boxplot()+
theme_bw() + 
labs(
    title = "Composite test score distribution based on family wealth", 
    x = "Wealth", 
    y = "Composite test score",
    fill = "Wealth") +
scale_fill_manual(values = c("#f7d551", "#f4cd2a", "#eda323")) +
stat_summary(fun = "mean",
    geom = "point",
    shape = 20,
    color = "green",
    size = 5,
    show.legend = FALSE) +
geom_point(aes(shape = "Average"), alpha = 0, color="green") +
guides(shape=guide_legend(title=NULL, shape=21, color = "green", override.aes = list(alpha = 1))) +
theme(plot.title = element_text(size = 20))

## how scores compare against grad parents
gradScoreGraph <- ggplot(dataset, aes(x=pgraduate, y=score, fill=pgraduate)) + 
geom_boxplot()+
theme_bw() + 
labs(
    title = "Composite test score distribution based on families with graduate parents", 
    x = "Parent graduate", 
    y = "Composite test score",
    fill = "Parent graduate") +
scale_fill_manual(values = c("#4874bf", "#f44242")) +
stat_summary(fun = "mean",
    geom = "point",
    shape = 20,
    color = "green",
    size = 5,
    show.legend = FALSE) +
geom_point(aes(shape = "Average"), alpha = 0, color="green") +
guides(shape=guide_legend(title=NULL, shape=21, color = "green", override.aes = list(alpha = 1))) +
theme(plot.title = element_text(size = 20))


# --- Q5. Ethnicity on composite score ---
ethnicityGraph <- ggplot(dataset, aes(x=ethnicity ,y=score, fill=ethnicity)) + 
geom_boxplot(varwidth = TRUE)+
theme_bw() + 
labs(
    title = "Composite test score distribution based on ethnicity", 
    x = "Ethnicity", 
    y = "Composite test score",
    fill = "Ethnicity") +
theme(plot.title = element_text(size = 20)) +
scale_fill_manual(values = c("#4c1a4c", "#1a1a4c", "#4c4c1a")) +
stat_summary(fun = "mean",
    geom = "point",
    shape = 20,
    color = "green",
    size = 5,
    show.legend = FALSE) +
geom_point(aes(shape = "Average"), alpha = 0, color="green") +
guides(shape=guide_legend(title=NULL, shape=21, color = "green", override.aes = list(alpha = 1))) +
theme(plot.title = element_text(size = 20))


# --- Q6. Ethnicity disadvantages ---
## how many ethnic families have graduate parents
ethnicGradGraph <- ggplot(dataset, aes(x = ethnicity, y = after_stat(count)/sum(after_stat(count)), fill = pgraduate)) + 
geom_bar(width = 0.5) +
theme_bw() +
labs(title = "Graduate parents based on ethnicity", 
    x = "Parent Graduate", 
    y = "Parent Total (%)",
    fill = "Parent Graduate") +
scale_fill_manual(values = c("#4874bf", "#f44242")) +
theme(plot.title = element_text(size = 20)) +
geom_text(aes(label = scales::percent(after_stat(count)/sum(after_stat(count)))),
            stat = "count",
            colour = "white",
            position = position_stack(vjust = 0.5)) +
scale_y_continuous(labels = scales::percent, limits = c(0,1))

## score/ethnicity box plot with enabled points of parent graduates
ethnicGradPoints <- ggplot(dataset, aes(x=ethnicity ,y=score, fill=ethnicity)) + 
geom_boxplot(varwidth = TRUE)+
theme_bw() + 
labs(
    title = "Composite test score distribution based on ethnicity and parent graduates", 
    x = "Ethnicity", 
    y = "Composite test score",
    fill = "Ethnicity",
    color = "Parent graduate") +
theme(plot.title = element_text(size = 20)) +
scale_fill_manual(values = c("#4c1a4c", "#1a1a4c", "#4c4c1a")) +
stat_summary(fun = "mean",
    geom = "point",
    shape = 20,
    color = "green",
    size = 5,
    show.legend = FALSE) +
geom_point(aes(shape = "Average"), alpha = 0, color="green") +
guides(shape=guide_legend(title=NULL, shape=21, color = "green", override.aes = list(alpha = 1))) +
theme(plot.title = element_text(size = 20)) + 
geom_jitter(aes(color=pgraduate), alpha=0.5) +
scale_color_manual(values = c("#4874bf", "#f44242")) 

## how many ethnic families are wealthy
ethnicWealthGraph <- ggplot(dataset, aes(x = ethnicity, y = after_stat(count)/sum(after_stat(count)), fill = wealth)) + 
geom_bar(width = 0.5) +
theme_bw() +
labs(title = "Wealthy families based on ethnicity", 
    x = "Wealth", 
    y = "Parent total (%)",
    fill = "Ethnicity") +
scale_fill_manual(values = c("#f7d551", "#f4cd2a", "#eda323")) +
theme(plot.title = element_text(size = 20)) +
geom_text(aes(label = scales::percent(after_stat(count)/sum(after_stat(count)))),
            stat = "count",
            colour = "white",
            position = position_stack(vjust = 0.5)) +
scale_y_continuous(labels = scales::percent, limits = c(0,1))

## wealth/ethnicity box plot with enabled points of wealth
ethnicWealthPoints <- ggplot(dataset, aes(x=ethnicity ,y=score, fill=ethnicity)) + 
geom_boxplot(varwidth = TRUE)+
theme_bw() + 
labs(
    title = "Composite test score distribution based on ethnicity and wealth", 
    x = "Ethnicity", 
    y = "Composite test score",
    fill = "Ethnicity",
    color = "Wealth") +
theme(plot.title = element_text(size = 20)) +
scale_fill_manual(values = c("#4c1a4c", "#1a1a4c", "#4c4c1a")) +
stat_summary(fun = "mean",
    geom = "point",
    shape = 20,
    color = "green",
    size = 5,
    show.legend = FALSE) +
geom_point(aes(shape = "Average"), alpha = 0, color="green") +
guides(shape=guide_legend(title=NULL, shape=21, color = "green", override.aes = list(alpha = 1))) +
theme(plot.title = element_text(size = 20)) + 
geom_jitter(aes(color=wealth), alpha=0.5) +
scale_color_manual(values = c("#f7d551", "#f4cd2a", "#eda323")) 


# --- Q7. Economical impact on college distance ---

## distance by unemployment
dataset <- subset(dataset, distance <=16) # outliers
distanceUnempGraph <- ggplot(dataset, aes(x = unemp, y = distance)) + 
geom_point(
    shape = 20,
    size = 2,
    position = position_jitter(h = 0.5, w = 0.5),
    alpha = 0.5) + 
theme_bw() + 
labs(
    title = "Correlation between college distance and county unemployment rates", 
    x = "Unemployment rate (%)", 
    y = "College distance (in 10 miles)") + 
stat_smooth(method = "lm",
        col = "red",
        se = FALSE,
        linewidth = 1) +
theme(plot.title = element_text(size = 20))





