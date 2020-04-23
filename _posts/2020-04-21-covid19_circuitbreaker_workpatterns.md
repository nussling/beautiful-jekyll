``` r
library(tidyverse)
library(glue)
library(here)
library(ggalluvial)
```

``` r
load(here::here("data", "fb_wave4", "dat2.rda"))
load(here::here("data", "fb_wave4", "dict.rda"))

wave4_df <- dat2 %>%
  filter(live_sg == "Yes") %>% 
  filter(between(2020 - year, 21, 120)) %>% 
  select(record_id, start_ncov, contains("work"),
         year, gender, ethnicity, education, housing, marital, income)

wave4_dict <- dict %>%
  filter(str_detect(`Variable / Field Name`, "^cb_work")) %>%
  select(`Variable / Field Name`,
         `Field Label`,
         `Choices, Calculations, OR Slider Labels`,
         `Branching Logic (Show field only if...)`)
```

We [previously reported](bit.ly/3ckkKME) the impact of the ‘circuit
breaker’ on contact patterns and visits to public places in Singapore
based on our online survey through facebook advertisement between 9 and
14 April 2020. The circuit breaker is a month-long period of tightened
distancing measures instituted by Singapore government to curb the
spread of COVID-19 starting from 3 April 2020. You can find out more
about the background, survey methodology, and demographic
characteristics of respondents in the [previous post](bit.ly/3ckkKME).

Here, we analysed how the circuit breaker measures impacted work
patterns and wage. We asked the following questions:

1.  Before 3 April 2020, when the government announced the new circuit
    breaker measures, were you doing paid work? Paid by the hour/by job;
    Paid by the day; Paid by the week; Salaried (paid a fixed salary
    each month); Self-employed
2.  Have you been able to work since 3rd April 2020? Yes, I have worked
    the same amount as usual; Yes, but less than usual; No
3.  If no, why not? I chose not to; I was not going to be paid; I was
    laid off from my job; My work closed/suspended
4.  Can your job be done remotely (e.g. working from home)? Yes, most of
    it can be done remotely; Yes, some of it can be done remotely; No
5.  Have you worked remotely since 3 April 2020? Yes, all the time; Yes,
    most of the time; Yes, some of the time; No
6.  Will you still get paid full wages? Yes; No

``` r
# only people who work and merge ad-hoc workers
df1 <- wave4_df %>%
  filter(cb_work_1 == "Yes") %>% 
  mutate(
    worktype = case_when(
      str_detect(cb_work_2, "^Paid by") ~ 1,
      cb_work_2 == "Salaried (paid a fixed salary each month)" ~ 2,
      cb_work_2 == "Self-employed" ~ 3,
      TRUE ~ NA_real_),
    income2 = case_when(
      income %in% c("< $2,000", "$2,000 - $3,999") ~ 1,
      income %in% c("$4,000 - $5,999", "$6,000 - $10,000") ~ 2,
      income == "> $10,000" ~ 3,
      income == "Prefer not to say" ~ 4,
      TRUE ~ NA_real_)
  )

worktype <- count(df1, worktype) %>% filter(!is.na(worktype))
worktype_n <- worktype$n
worktype_pct <- round(worktype_n/sum(worktype_n)*100, 1)
```

Ability to work since 3 April 2020
==================================

Prior to announcement of circuit breaker measures, 719 out of 1032
(69.7%) respondents were doing paid work. Of these respondents, NA
(10.7%) were paid on an ad-hoc basis (i.e. paid by the hour, job, day,
week), NA (77.5%) were salaried employees who were paid a fixed salary
each month, and NA (11.8%) were self-employed.

After the circuit breaker measures started, more than half of ad-hoc
employees and self-employed reported that they have not been able to
work while the work of the majority of salaried employees were not
affected.

``` r
worktype <- df1 %>%
  group_by(worktype, cb_work_4) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)*100) %>%
  na.omit()
```

``` r
cols <- c("#546880", "#C0CAC9", "#DEDFD7")

titles <- c(
  '1' = glue("Ad-Hoc (n = {worktype_n[1]})"), 
  '2' = glue("Salaried (n = {worktype_n[2]})"), 
  '3' = glue("Self-Employed (n = {worktype_n[3]})")
)

levels(worktype$cb_work_4) <- gsub(" ", "\n", levels(worktype$cb_work_4))

tick <- c(
  "Yes, same",
  "Yes, less",
  "No"
)

ggplot(worktype, aes(y = p, x = cb_work_4, fill = cb_work_4)) + 
  geom_col(width = 0.4) + 
  geom_text(aes(label = n), size = 3,
            position = position_dodge(width = 0.1), vjust = 0.5, hjust = -0.25) +
  facet_wrap(worktype ~., labeller = as_labeller(titles)) +
  coord_flip() + 
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = tick) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Responses", y = "Percentage",
       title = "Have you been able to work since 3 April 2020?") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        text = element_text(size = 11, face = "bold"))
```

![](2020-04-21-covid19_circuitbreaker_workpatterns_files/figure-markdown_github/graph1-1.png)

``` r
able_to_work <- count(df1, cb_work_4) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

wardtype_not_work <- df1 %>% 
  filter(cb_work_4 == "No") %>% 
  count(worktype)
```

When circuit breaker measures were announced, 127 (17.7%) respondents
reported that they have not been able to work. Among them, 44, 40, and
43 were ad-hoc, salaried employees, and self-employed, respectively. In
all three employment types, most cited work closure or suspension as the
reason.

``` r
df_nowork <- df1 %>%
  filter(cb_work_4 == "No") %>% 
  count(worktype, cb_work_7) %>% 
  group_by(worktype) %>% 
  mutate(p = n/sum(n)*100) %>%
  na.omit()
```

``` r
cols <- c("#546880", "#89A0AE", "#C0CAC9", "#DEDFD7")

titles <- c(
  '1' = glue("Ad-Hoc (n = {wardtype_not_work$n[1]})"), 
  '2' = glue("Salaried (n = {wardtype_not_work$n[2]})"), 
  '3' = glue("Self-Employed (n = {wardtype_not_work$n[3]})")
) 

ggplot(df_nowork, aes(y = p, x = cb_work_7, fill = cb_work_7)) + 
  geom_col(width = 0.5) + 
  geom_text(aes(label = n), size = 3, 
            position = position_dodge(width = 0.1), vjust = 0.5, hjust = -0.25) +
  facet_wrap(worktype ~., labeller = as_labeller(titles)) +
  coord_flip() + 
  scale_fill_manual(values = cols) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Responses", y = "Percentage",
       title = "Why have respondents not been working since 3 April 2020?") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        text = element_text(size = 11, face = "bold"))
```

![](2020-04-21-covid19_circuitbreaker_workpatterns_files/figure-markdown_github/graph2-1.png)

``` r
cb_work_6_levels <- c(
  levels(df1$cb_work_6)[1:3],
  "I have not been working remotely",
  "My job cannot be done remotely"
)

yeswork <- df1 %>%
  filter(str_detect(cb_work_4, "^Yes")) %>% 
  mutate(
    work_remotely = case_when(
      cb_work_6 == "No" ~ "I have not been working remotely",
      is.na(cb_work_6) ~ "My job cannot be done remotely",
      TRUE ~ as.character(cb_work_6)),
    work_remotely = factor(work_remotely, levels = cb_work_6_levels)
  )
  

yeswork %>% count(work_remotely)
```

    ## # A tibble: 5 x 2
    ##   work_remotely                        n
    ##   <fct>                            <int>
    ## 1 Yes, all the time                  272
    ## 2 Yes, most of the time               82
    ## 3 Yes, some of the time               63
    ## 4 I have not been working remotely    27
    ## 5 My job cannot be done remotely     141

``` r
df_yeswork <- yeswork %>% 
  group_by(worktype, work_remotely) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)*100)

worktype_remote <- count(yeswork, worktype)
```

Among the 585 respondents who have been able to work either as usual or
less than usual, most have been able to work remotely either all or some
of the time. Higher proportion of ad-hoc employess reported that their
work could not be done remotely, compared to salaried and self-employed.

``` r
cols <- c("#2D3A4C", "#546880", "#89A0AE", "#C0CAC9", "#DEDFD7")

titles <- c(
  '1' = glue("Ad-Hoc (n = {worktype_remote$n[1]})"), 
  '2' = glue("Salaried (n = {worktype_remote$n[2]})"), 
  '3' = glue("Self-Employed (n = {worktype_remote$n[3]})")
) 

ggplot(df_yeswork, aes(y = p, x = work_remotely, fill = work_remotely)) + 
  geom_col(width = 0.5) + 
  geom_text(aes(label = n), size = 3,
            position = position_dodge(width = 0.1), vjust = 0.5, hjust = -0.25) +
  facet_wrap(worktype ~., labeller = as_labeller(titles)) +
  coord_flip() + 
  scale_fill_manual(values = cols) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Responses", y = "Percentage",
       title = "Have you worked remotely since 3 April 2020?") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        text = element_text(size = 11, face = "bold"))
```

![](2020-04-21-covid19_circuitbreaker_workpatterns_files/figure-markdown_github/graph3-1.png)

``` r
full_wage <- yeswork %>% 
  count(cb_work_8) %>% 
  mutate(p = round(n/sum(n)*100, 1))
```

Receiving wage by income category
=================================

Among the 585 respondents who have been able to work, the majority
(88.7%) reported that they will receive full wage for their work.

However, when we assess wage paid by income categories after including
respondents who have not been able to work since 3 April 2020, it paints
a less rosy picture: higher proportion of respondents in lower income
categories had to stop working and were not paid their full wage. The
figure below shows the distribution of respondents across three
questions:

1.  Have you been able to work since 3 April 2020?
2.  Can your job be done remotely?
3.  Will you get paid full wage?

``` r
cb_work_4_fct <- tibble(
  level = c("No",
            "Yes, but less than usual",
            "Yes, I have worked the same amount as usual"),
  label = c("I haven't been working throughout",
            "Yes, but less\nthan usual",
            "Yes, I have worked\nthe same amount as usual")
)

cb_work_5_fct <- tibble(
  level = c("(Missing)",
            "No",
            "Yes, some of it can be done remotely",
            "Yes, most of it can be done remotely"),
  label = c("I haven't been working throughout",
            "My job cannot be\ndone remotely",
            "Yes, some of it\ncan be done remotely",
            "Yes, most of it\ncan be done remotely")
)

cb_work_89_level <- c("No", "Yes", "I haven't been working throughout")

df2 <- df1 %>%
  filter(!is.na(cb_work_4)) %>%
  filter(!is.na(income) & income != "Prefer not to say") %>%
  mutate(
    cb_work_4 = factor(cb_work_4, levels = cb_work_4_fct$level, labels = cb_work_4_fct$label),
    cb_work_5 = factor(fct_explicit_na(cb_work_5), levels = cb_work_5_fct$level, labels = cb_work_5_fct$label),
    cb_work_89 = ifelse(is.na(cb_work_8), as.character(cb_work_9), as.character(cb_work_8)),
    cb_work_89 = ifelse(is.na(cb_work_89), "I haven't been working throughout", cb_work_89),
    cb_work_89 = factor(cb_work_89, levels = cb_work_89_level),
    income = fct_recode(income,
                        `>$6,000` = "$6,000 - $10,000",
                        `>$6,000` = "> $10,000"),
    income = fct_drop(income)
  )
```

``` r
income <- table(df2$income)
income_labels <- paste0(names(income), " (n = ", income, ")")
df2$income <- factor(df2$income, levels = names(income), labels = income_labels)

df2 %>%
  count(income, cb_work_4, cb_work_5, cb_work_89) %>% 
  ggplot(aes(y = n,
             axis1 = cb_work_4,
             axis2 = cb_work_5,
             axis3 = cb_work_89)) +
    geom_alluvium(aes(fill = cb_work_4), alpha = 0.6, width = 0.4) +
    geom_stratum(width = 0.4, alpha = 0.75, size = .2, colour = "grey50") +
    geom_text(stat = "stratum", infer.label = TRUE, size = 2) +
    scale_x_discrete(limits = c("Have you been able to work\nsince 3 April 2020?",
                                "Can your job be done\nremotely?",
                                "Will you get paid full wage?")) +
    scale_fill_manual(values = c("#734858", "#F2B6A0", "#68A694")) +
    facet_wrap(~ income, scales = "free", ncol = 1) +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.spacing.y = unit(1, "lines"),
          strip.background = element_rect(fill = "transparent"),
          strip.text = element_text(face = "bold"),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          # axis.text.x = element_text(size = 22),
          axis.title = element_blank())
```

![](2020-04-21-covid19_circuitbreaker_workpatterns_files/figure-markdown_github/unnamed-chunk-6-1.png)
