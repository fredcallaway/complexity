library(tidyverse)
library(qualtRics)
library(digest)
library(glue)


# this script will only work if you have the api_key
# qualtrics_api_credentials(api_key = "REDACTED", 
#                           base_url = "princetonsurvey.az1.qualtrics.com", 
#                           install = TRUE)

survey_id = "SV_a65mkrGgAbYz6US"
version = "v1"
dir.create(glue("data/{version}"), recursive=TRUE)

write_data = function(data, name) {
    path = glue("data/{version}/{name}.csv")
    write_csv(data, path)
    print(glue("Wrote {path}"))
    data
}

raw_data = fetch_survey(survey_id, force_request=TRUE, col_types=cols())

data = raw_data %>% 
    drop_na(ID) %>% 
    filter(str_length(ID) == 24) %>% 
    filter(!startsWith(ID, "5e14f")) %>% # previewing
    rowwise() %>% 
    mutate(participant = substr(digest(ID), 1, 10))

identifiers = data %>% 
    transmute(prolific_id=ID, participant) %>% 
    write_data("identifiers")

anonymized = data %>% 
    select(!ID) %>% 
    write_data("anonymized_raw")

# %% ==================== extract stimuli values ====================

lottery_desc = extract_colmap(raw_data) %>% 
    filter(str_detect(qname, "^Q\\d.*_")) %>% 
    mutate(question = str_extract(qname, "[^_]+")) %>% 
    distinct(question, .keep_all=TRUE) %>% 
    group_by(question)

lottery_values = lottery_desc %>% 
    filter(!str_detect(question, "complex")) %>% 
    summarise(
        data.frame(
            str_match(main, "If Heads, then you receive \\$(.*)\n\n.*If Tails, then you receive \\$(.*)")
        ) %>% 
        transmute(heads=parse_double(X2), tails=parse_double(X3))
    )


lottery_values = bind_rows(lottery_values, tribble(
  ~question, ~heads, ~tails,
  "Q7complex", 3, 7,
  "Q7'complex", 3, 7,
))

lottery_values %>% write_data("lottery_values")

# %% --------
question_values = extract_colmap(raw_data) %>% 
    filter(str_detect(qname, "_")) %>% 
    mutate(value=parse_double(str_match(sub, "Lottery:\\$(.*)")[,2])) %>% 
    select(qname, value) %>% 
    write_data("question_values")

# %% ==================== save processed human data ====================

choices = anonymized %>% 
    select(participant,  contains("_")) %>% 
    select(!Q_LastModified) %>% 
    pivot_longer(!participant, names_to="qname", values_to="response") %>% 
    drop_na(response) %>% 
    mutate(question = str_extract(qname, "[^_]+")) %>% 
    left_join(question_values) %>% 
    mutate(choose_lottery=response == "1") %>% 
    # filter(startsWith(question, "Q")) %>% 
    select(!c(response, qname)) %>% 
    write_data("choices")
