library(tidyverse)

major = "1"
minor = "0"
choices = read_csv(glue('data/v{major}/choices.csv'))
identifiers = read_csv(glue('data/v{major}/identifiers.csv'))
lottery_values = read_csv(glue('data/v{major}/lottery_values.csv'))
ids = read_csv(glue('prolific/v{major}.{minor}.csv')) %>% 
    transmute(prolific_id = participant_id) %>% 
    left_join(identifiers)

# %% --------

selection = choices %>% 
    right_join(ids) %>% 
    filter(!(question %in% c("Example", "areyousure"))) %>% 
    group_by(participant,question) %>% 
    slice_sample(n=1) %>% 
    group_by(participant) %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    slice_sample(prop=1/4)

# %% --------

selection %>% 
    left_join(lottery_values) %>% 
    rowwise() %>% 
    mutate(bonus = if_else(choose_lottery, 
        sample(c(heads, tails), 1),
        value
    )) %>% 
    filter(choose_lottery) %>% 
    select(prolific_id, bonus) %>% 
    write_csv("bonus.csv", col_names=FALSE)

system("cat bonus.csv | pbcopy")





    

