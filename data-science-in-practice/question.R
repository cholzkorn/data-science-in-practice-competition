library(tidyverse)

df <- tibble(x1 = c(13, NA, 16, 17, 16, 12), x2 = c(1, 4, 3, 5, NA, 4),
             month = c(1, 1, 1, 2, 2, 2))


new_df <- df %>%  group_by(month) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))