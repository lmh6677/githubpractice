rm(list = ls())

library(googlesheets4)
library(tidyverse)
library(readr)
library(stringr)

gs4_deauth()
galtonRaw <- read_sheet(
  ss = 'https://docs.google.com/spreadsheets/d/1ZA83iqkojBVX0bvtXQ-wuCyR6oswZwJMORRb-VK2ktM/edit?usp=sharing'
)

glimpse(galtonRaw)

library(tidyverse)

galtonClean <- galtonRaw %>%
  mutate(
    Mother = parse_number(as.character(Mother)),
    mother_height = Mother + 60,
    Father = as.numeric(Father),
    father_height = Father + 60
  )

galtonLong <- galtonClean %>%
  pivot_longer(
    cols = c("Sons in order of height", "Daughters in order of height"),
    names_to = "sex",
    values_to = "child_heights_raw"
  )

galtonLong <- galtonLong %>%
  mutate(
    child_heights_raw = as.character(child_heights_raw)
  ) %>%
  separate_rows(child_heights_raw, sep = ",") %>%
  mutate(
    child_heights_raw = parse_number(as.character(child_heights_raw))
  )

galtonLong <- galtonLong %>%
  mutate(
    child_height = child_heights_raw + 60
  )

galtonLong <- galtonLong %>%
  group_by(family, sex) %>%
  mutate(
    birth_order = row_number()
  ) %>%
  ungroup()

galtonTidy <- galtonLong %>%
  select(
    family = family,
    father_height,
    mother_height,
    sex,
    birth_order,
    child_height
  ) %>%
  arrange(family, sex, birth_order)





