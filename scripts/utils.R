tidy_kwic <- function(txt){
  as_tibble(txt) |>
    separate(docname, into = c('docname', 'sentence_number'), sep = '\\.') |>
    separate(docname, into = c('agency', 'plan'), extra = 'merge') |>
    mutate(plan = str_replace_all(plan, '-', ' ')) |>
    select(-c(from, to)) |>
    relocate(pattern, .after = 'plan') |>
    mutate(sentence = glue("{pre} {keyword} {post}") |> str_trim() |> str_to_sentence()) |>
    mutate(sentence = str_replace_all(sentence, ' ,', ','),
           sentence = str_replace_all(sentence, ' :', ':'))
}

tidy_dict <- function(dict){
  tidy(dict) |>
    group_by(category) |>
    mutate(search_terms = str_flatten(string = word, collapse = ', ')) |>
    select(pattern = category, search_terms) |>
    distinct(pattern, .keep_all = TRUE)
}

count_matches <- function(txt, ...){
  txt |>
    group_by(..., pattern, .drop = FALSE) |>
    count() |>
    ungroup() |>
    complete(..., pattern, fill = list(n = 0)) |>
    pivot_wider(names_from = pattern, values_from = n)
}


my_theme <- function(data, ...){
  data %>%
    opt_row_striping() %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        gt::google_font("Lato"),
        default_fonts()))  %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    #Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    tab_options(
      column_labels.border.top.width = px(5),
      column_labels.border.top.color = "#FFFFFF",
      table.border.top.color = "#FFFFFF",
      table.border.bottom.color = "#FFFFFF",
      heading.background.color = '#1696d2',
      data_row.padding = px(5),
      source_notes.font.size = 16 ,
      heading.align = "center",
      row_group.background.color = '#D0D3D4',
      ...)
}

check <- function(plan){
  if (plan == TRUE) {
    logo_out <- fontawesome::fa("check", fill = "#55b748")
  } else if (plan == FALSE){
    logo_out <- fontawesome::fa("times", fill = "#db2b27")
  }

  logo_out %>%
    as.character() %>%
    gt::html()

}




reactable_example <- function(pattern){
  sentences_filtered <- filter(sentences, pattern == {{pattern}}) |> select(-pattern)

  sentences_shared <- SharedData$new(sentences_filtered)

  examples <-
    bscols(
      widths = c(1.5, 10.5),
      list(
        filter_select("agency", "Agency", sentences_shared, ~agency, multiple = TRUE),
        filter_select("plan", "Plan", sentences_shared, ~plan, multiple = TRUE)
      ),
      reactable(
        sentences_shared,
        # compact = TRUE,
        defaultPageSize = 5,
        showSortIcon = FALSE,
        theme = flatly(),
        columns = list(
          agency = colDef(
            name = 'Agency', maxWidth = 95
          ),
          plan = colDef(name = 'Plan', maxWidth = 130),
          sentence = colDef(name = 'Sentence', html = TRUE)
        )
      )
    )

  examples

}




