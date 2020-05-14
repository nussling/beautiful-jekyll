---
layout: post
title: "Singapore COVID-19"
subtitle: "TraceTogether mobile app"
author: Zaw Myo Tun
date: 2020-05-14
categories: COVID-19
tags: Singapore, circuit breaker, coronavirus, pandemic, contact tracing, TraceTogether
---

``` r
library(tidyverse)
library(here)
library(glue)
library(arsenal)
```

``` r
load(here::here("data", "fb_wave5", "dat1.rda"))
load(here::here("data", "fb_wave5", "dat2.rda"))
load(here::here("data", "fb_wave5", "dict.rda"))
```

``` r
# Data set
trace <- dat2 %>% 
  mutate(age = 2020 - year) %>% 
  filter(between(age, 21, 120)) %>% 
  select(start_ncov, starts_with("cb_tt"), age, gender:income, -housing_others_ncov)

# Labels
trace_dict <- dict %>% 
  select(new_varname,
         `Field Label`,
         `Choices, Calculations, OR Slider Labels`,
         `Branching Logic (Show field only if...)`) %>% 
  filter(grepl("^cb_tt", new_varname))

# Checkbox labels
my_str <- function(df) {
  s <- str_split(df[[1]], " \\| ", simplify = TRUE)
  s <- as.character(s)
  
  str_split(s, ", ") %>% 
    unlist() %>% 
    matrix(nrow = length(s), byrow = TRUE) %>% 
    data.frame() %>% 
    setNames(c("code", "label"))
}

checkbox_label <- dict %>% 
  filter(grepl("^cb_tt", new_varname), `Field Type` == "checkbox") %>% 
  select(new_varname, `Choices, Calculations, OR Slider Labels`) %>% 
  nest(data = `Choices, Calculations, OR Slider Labels`) %>% 
  mutate(data = map(data, my_str))

names(checkbox_label$data) <- checkbox_label$new_varname
```

On March 20, Singapore government launched TraceTogether, a mobile phone
app for improving contact tracing. This is a collaborative effort of the
Government Technology Agency of Singapore (GovTech) and the Ministry of
Health. The app works by exchanging the bluetooth signals of nearby
phones with the app installed. As of May 8, the mobile app has
[reportedly](https://www.todayonline.com/singapore/given-low-adoption-rate-tracetogether-experts-suggest-merging-safeentry-or-other-apps)
been downloaded by about 25% of Singapore residents. However, this
figure is far from a minimum of 75% of active users in the population,
[as suggested by the multi-ministry task force on
COVID-19](https://www.straitstimes.com/singapore/no-other-way-but-to-make-use-of-tracetogether-mandatory?login=true&close=true),
for effctive contact tracing.

In this analysis, we report the feedback from people who downloaded the
app and possible reasons for the low number of downloads.

TraceTogether Usage
-------------------

``` r
dl <- count(trace, cb_tt_1) %>% 
  mutate(pct = round(n/sum(n)*100)) %>% 
  filter(cb_tt_1 == "Yes")
```

Among 807 eligible respondents, 572 (71%) reported that they downloaded
the app. This percentage is higher than the reported figure of 25% in
the population. Demographic characteristics were similar among
respondents who downloaded the app and those who did not (results not
shown).

When asked the reasons for not downloading the app, the respondents
cited concerns about how the collected data will be used as the top
reason.

``` r
not_dl_reason <- trace %>% 
  filter(cb_tt_1 == "No") %>% 
  pivot_longer(starts_with("cb_tt_2")) %>% 
  group_by(name) %>% 
  mutate(value = as.integer(as.character(value))) %>% 
  summarise(total = n(),
            n = sum(value),
            prop = mean(value)) %>% 
  separate(name, c("var", "code"), sep = "___") %>% 
  left_join(checkbox_label$data[["cb_tt_2"]], by = "code") %>% 
  arrange(desc(prop))

not_dl_reason$label <- factor(not_dl_reason$label, levels = not_dl_reason$label)

ggplot(not_dl_reason, aes(prop, fct_rev(label))) +
  geom_col(fill = "#265F99", width = 0.8) +
  geom_text(aes(label = n), hjust = -0.8) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(x = "", y = "",
       title = glue("Reasons for not downloading TraceTogether app (n = {nrow(trace) - pull(dl, n)})")) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 1))
```

<p><img src="/assets/2020-05-14-covid19_tracetogether_files/figure-markdown_github/unnamed-chunk-4-1.png" /></p>

``` r
onphone <- trace %>% 
  filter(cb_tt_1 == "Yes") %>% 
  count(cb_tt_3) %>% 
  mutate(pct = round(n/sum(n)*100)) %>% 
  filter(cb_tt_3 == "Yes")
```

Among 572 respondents who downloaded the app, the majority (93%) have it
installed on their phones at the time of the survey. A small number of
people (n = 42) removed the app from their phone. Battery drainage
appears to be the top concern as reported by almost half of these
respondents. Other commonly reported reasons are the app not running
properly and concerns over how the collected data will be used.

``` r
not_onphone_reason <- trace %>% 
  filter(cb_tt_3 == "No") %>% 
  pivot_longer(starts_with("cb_tt_4")) %>% 
  group_by(name) %>% 
  mutate(value = as.integer(as.character(value))) %>% 
  summarise(total = n(),
            n = sum(value),
            prop = mean(value)) %>% 
  separate(name, c("var", "code"), sep = "___") %>% 
  left_join(checkbox_label$data[["cb_tt_4"]], by = "code") %>% 
  mutate(
    label_count = sapply(strsplit(label, " "), length),
    label = ifelse(label_count > 10,
                   paste(word(label, 1, 10), word(label, 10, label_count), sep = "\n"),
                   label)
  ) %>% 
  select(-label_count) %>% 
  arrange(desc(prop))

not_onphone_reason$label <- factor(not_onphone_reason$label, levels = not_onphone_reason$label)

ggplot(not_onphone_reason, aes(prop, fct_rev(label))) +
  geom_col(fill = "#265F99", width = 0.8) +
  geom_text(aes(label = n), hjust = -0.8) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(x = "", y = "",
       title = glue("Reasons for removing TraceTogether app (n = {pull(dl, n) - pull(onphone, n)})")) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 1))
```

<p><img src="/assets/2020-05-14-covid19_tracetogether_files/figure-markdown_github/unnamed-chunk-6-1.png" /></p>

Opinions on TraceTogether
-------------------------

``` r
moh <- trace %>%
  filter(cb_tt_1 == "Yes") %>%
  count(cb_tt_8) %>% 
  filter(cb_tt_8 == "Yes")

likert_vars <- dict %>% 
  select(new_varname, `Field Label`) %>% 
  filter(grepl("cb_tt_[5-7|9]$", new_varname)) %>% 
  mutate(
    `Field Label` = str_remove(`Field Label`, "On a scale of 1 to 5, "),
    `Field Label` = str_replace(`Field Label`, "^how", "How"),
    `Field Label` = str_replace(`Field Label`, "5 =", ", 5 =")
  ) %>% 
  separate(`Field Label`, c("label1", "label2"), sep = "\\?") %>% 
  mutate(
    label2 = str_trim(label2),
    label1_count = sapply(strsplit(label1, " "), length),
    label1 = ifelse(label1_count > 8,
                    paste(word(label1, 1, 8), word(label1, 9, label1_count), sep = "\n"),
                    label1),
    label = paste(label1, label2, sep = "?\n"),
    label = ifelse(new_varname == "cb_tt_9",
                   str_replace(label, "\\?", "?*"),
                   label)
  ) %>% 
  select(new_varname, label)

trace_likert <- trace %>% 
  filter(cb_tt_1 == "Yes") %>% 
  # Remove rows with all missing data
  filter(!(is.na(cb_tt_5) & is.na(cb_tt_6) & is.na(cb_tt_7) & is.na(cb_tt_9))) %>% 
  mutate(
    cb_tt_9 = ifelse(start_ncov < lubridate::ymd_hms("2020-04-27 20:00:00"),
                     NA,
                     cb_tt_9)
  ) %>% 
  select(matches("cb_tt_[5-7|9]$"))

likert_no_answer <- trace %>% 
  filter(cb_tt_1 == "Yes") %>% 
  filter(is.na(cb_tt_5) & is.na(cb_tt_6) & is.na(cb_tt_7) & is.na(cb_tt_9))
```

Among 572 repondents who downloaded TraceTogether, only 1 has been
contacted by the Ministry of Health because of close contact with a
COVID-19 case. These respondents were also asked their opinion on
different aspects of the app (on a scale of 1 to 5 with 3 considered as
neutral) ranging from ease of use to data privacy. After excluding 42
respondents who did not answer the questions, we found that more than
half of these respondents think that TraceTogether app is easy to use,
useful for contact tracing, and practical. Interestingly, 36% of
respondents reported that they were concerned about data privacy in
using the app while 42% reported otherwise.

``` r
trace_likert_0 <- trace_likert %>% 
  map_dfr(~ data.frame(table(.x)), .id = "var") %>% 
  group_by(var) %>% 
  mutate(prop = round(Freq/sum(Freq), 2)) %>% 
  ungroup() %>% 
  left_join(likert_vars, by = c("var" = "new_varname")) %>% 
  select(ques = label, code = .x, freq = Freq, prop) %>% 
  mutate(prop = ifelse(code %in% 1:2, prop*(-1), prop),
         ques = factor(ques, levels = likert_vars$label),
         code = factor(code, levels = c(1, 2, 3, 5, 4)))

trace_likert_1 <- trace_likert_0 %>% 
  mutate(prop = ifelse(code == "3", 0, prop))

trace_likert_2 <- trace_likert_0 %>% 
  filter(code == "3")

pct_labels <- paste0(c(seq(75, 0, -25), seq(25, 75, 25)), "%")

likert_colours <- c(
  "#BD4332",
  "#F2AEAE",
  "#656565",
  "#80B0E0",
  "#265F99"
)

p1 <- trace_likert_1 %>% 
  ggplot(aes(x = prop, y = fct_rev(ques))) +
  geom_col(aes(fill = code), width = 0.5) +
  labs(x = "", y = "") +
  scale_x_continuous(
    label = pct_labels,
    limits = c(-.75, .75),
    breaks = seq(-.75, .75, .25)
  ) +
  scale_fill_manual(values = likert_colours, breaks = c("1", "2", "3", "4", "5")) +
  theme(
    axis.ticks = element_blank(),
    legend.position = c("bottom"),
    legend.title = element_blank(),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm")
  )

p2 <- trace_likert_2 %>% 
  ggplot(aes(x = prop, y = fct_rev(ques))) +
  geom_col(width = 0.5, alpha = 0.85, fill = likert_colours[3]) +
  labs(x = "", y = "") +
  scale_x_continuous(
    label = pct_labels[4:7],
    limits = c(0, .75),
    breaks = seq(0, .75, .25)
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(0.5, 1, 0, 0), "cm")
  )

library(cowplot)

p <- plot_grid(p1, p2, rel_widths = c(1, 0.4), align = "h", axis = "b", nrow = 1)

# Add a title (source: https://wilkelab.org/cowplot/articles/plot_grid.html)
title <- ggdraw() + 
  draw_label(
    glue("Opinions on TraceTogether as reported by people who downloaded the app (n = {nrow(trace_likert)})"),
    # fontface = 'bold',
    x = 0,
    hjust = -0.1
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(
  title, p,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
```

<p><img src="/assets/2020-05-14-covid19_tracetogether_files/figure-markdown_github/unnamed-chunk-8-1.png" alt="Location visits" /><figcaption>*The first 80 responses were excluded from this question because of the change in response options in early period of the survey.</figcaption></p>

Participation
-------------

``` r
complete <- dat1 %>% 
  filter(!is.na(end_ncov))

complete_pct <- round(nrow(complete)/nrow(dat1)*100)

not_live_sg <- dat1 %>% 
  filter(live_sg == "No")

age <- dat1 %>% 
  filter(live_sg == "Yes" | is.na(live_sg)) %>% 
  mutate(age = 2020-year) %>% 
  filter(!between(age, 21, 120))
```

We repeated a [previous
survey](https://nussling.github.io/2020-04-14-covid19_circuitbreaker_contactpatterns/)
between April 26 and May 13 using an updated questionnaire. Survey
completion was 55% (based on the number of completed surveys out of
users who initiated the survey).

Respondents who reported not currently residing in Singapore (n = 5) and
individuals aged \<21 years or missing age were excluded from the
analysis (n = 19). Demographic characteristics of eligible participants
are as below:

``` r
var_labels <- tribble(
  ~var,        ~label,
  "age",       "Age in years",
  "gender",    "Gender",
  "ethnicity", "Ethnicity",
  "education", "Education attainment",
  "housing",   "Housing type",
  "marital",   "Marital status",
  "income",    "Household income",
  "cb_tt_1",   "Have you ever downloaded the TraceTogether app?"
)

attach_label <- function(x, lab) {
  attr(x, "label") <- lab
  x
}

demo_df <- trace %>% 
  select(all_of(var_labels$var), cb_tt_1) %>% 
  map2_dfc(var_labels$label, attach_label) %>% 
  mutate_if(is.factor, fct_drop)

demo_table <- tableby(~ ., data = demo_df,
                control = tableby.control(test = FALSE, digits = 1L))
summary(demo_table)
```

|                                                     | Overall (N=807) |
|:----------------------------------------------------|:---------------:|
| **Age in years**                                    |                 |
|    Mean (SD)                                        |   48.3 (11.6)   |
|    Range                                            |   21.0 - 78.0   |
| **Gender**                                          |                 |
|    Male                                             |   335 (41.5%)   |
|    Female                                           |   454 (56.3%)   |
|    Non-binary                                       |     2 (0.2%)    |
|    Prefer not to say                                |    16 (2.0%)    |
| **Ethnicity**                                       |                 |
|    Chinese                                          |   581 (72.0%)   |
|    Malay                                            |    39 (4.8%)    |
|    Indian                                           |    41 (5.1%)    |
|    Other                                            |   123 (15.2%)   |
|    Prefer not to say                                |    23 (2.9%)    |
| **Education attainment**                            |                 |
|    Primary (PSLE)                                   |     4 (0.5%)    |
|    Secondary (O/N Level)                            |    72 (8.9%)    |
|    A Level/Polytechnic/Diploma                      |   140 (17.3%)   |
|    ITE/NTC                                          |    19 (2.4%)    |
|    University                                       |   548 (67.9%)   |
|    Prefer not to say                                |    24 (3.0%)    |
| **Housing type**                                    |                 |
|    1-2 room HDB                                     |    19 (2.4%)    |
|    3-4 room HDB                                     |   243 (30.1%)   |
|    5-room HDB/executive flat                        |   210 (26.0%)   |
|    Private housing                                  |   288 (35.7%)   |
|    Others                                           |    17 (2.1%)    |
|    Prefer not to say                                |    30 (3.7%)    |
| **Marital status**                                  |                 |
|    Currently married                                |   478 (59.2%)   |
|    Divorced                                         |    37 (4.6%)    |
|    Never married                                    |   221 (27.4%)   |
|    Separated but not divorced                       |     6 (0.7%)    |
|    Widowed                                          |     9 (1.1%)    |
|    Prefer not to say                                |    56 (6.9%)    |
| **Household income**                                |                 |
|    \< $2,000                                        |    56 (6.9%)    |
|    $2,000 - $3,999                                  |    84 (10.4%)   |
|    $4,000 - $5,999                                  |   110 (13.6%)   |
|    $6,000 - $10,000                                 |   161 (20.0%)   |
|    \> $10,000                                       |   247 (30.6%)   |
|    Prefer not to say                                |   149 (18.5%)   |
| **Have you ever downloaded the TraceTogether app?** |                 |
|    No                                               |   235 (29.1%)   |
|    Yes                                              |   572 (70.9%)   |
