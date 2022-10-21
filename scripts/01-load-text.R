
# Setup ----------------------------------------------------------------------------------------------------

librarian::shelf(tidyverse, tidytext, quanteda, spacyr, reticulate, glue)

quanteda_options("threads" = 10)
source('scripts/utils.R')
# Load data -------------------------------------------------------------------------------------------------------

# Read agency plans, convert text to a corpus, and tokenize
plans_txt <- readtext::readtext('processed-data/txt/*', docvarsfrom = "filenames")

plans <-
  fs::dir_ls('processed-data/txt') |>
  fs::path_file() |>
  str_extract('^[^.]*') |>
  str_to_upper()

as_tibble_col(plans, 'docname') |>
  separate(docname, into = c('agency', 'plan'), extra = 'merge')

corpus <-
  corpus(plans_txt)|>
  set_names(plans) |>
  corpus_reshape(to = 'sentences')

toks <- tokens(corpus, remove_punct = FALSE)



# Underserved + Focus Groups --------------------------------------------------------------------------------------



dict_underserved <-
  dictionary(
    list(
      native_american = c('tribal*', 'tribes*', 'indigenous*','native*'),
      alaskan = 'alaskan*',
      pacific_islander = c('pacific*', 'pacific island*'),
      asian = c('asian'),
      aapi = c('aapi', 'asian-pacific'),
      black = c('black*', 'african-american*'),
      bipoc = 'BIPOC',
      poc = c('people of color', 'persons of color',  'students of color', 'Communities of color'),
      latinx = c('latino*', 'hispanic*', 'latinx*'),
      women = c('women'),
      lgbtqplus = c('LGBT*'),
      disabled = c('disabled*', 'disabilities*', 'disability*'),
      rural = c('rural*'),
      immigrant = c('immigrant*', 'newcomer*', 'migrant*')
    )
  )

dict_underserved_tidy <-
  tidy(dict_underserved) |>
  mutate(word = str_flatten(word, ', '))

context_list$underserved_global <-
  kwic(toks, dict_underserved, window = 1000) |>
  tidy_kwic() |>
  left_join(dict_underserved_tidy, by = c('pattern' = 'category'))


context_list$underserved_window <-
  tokens_select(toks, 'underserved*|disadvantaged|communities of concern|minority|overburdened|vulnerable|marginalized|underrepresented', window = 1000, selection = 'keep') |>
  kwic(dict_underserved, window = 1000) |>
  tidy_kwic() |>
  left_join(dict_underserved_tidy, by = c('pattern' = 'category'))



# Barriers + Enrollment -------------------------------------------------------------------------------------------


dict_barriers <- dictionary(list(barriers = c('completion', 'enroll*', 'participat*')))
context_list$barriers_window <-
  tokens_select(toks, 'barrier*', window = 1000, selection = 'keep') |>
  kwic(dict_barriers, window = 1000) |>
  tidy_kwic() |>
  mutate(search_terms = str_flatten(dict_barriers$barriers, ', ')) |>
  distinct(agency, plan, sentence_number, .keep_all = TRUE)

