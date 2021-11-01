librarian::shelf(
  dplyr, DT, readr, tidyr)
options(readr.show_col_types = F)

# edit: [EDS 211 Team Science - Google Sheets](https://docs.google.com/spreadsheets/d/1hQGPNBotVV2KfouaQE8U2tSTZBuIuflsMtvVWQwKcx8/edit#gid=0)
sched_csv <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSbjLIAlor277KKU5tUys5Rb290KmO8SGMquPXErF9ELPifCJhbFYNKOVvQIv6OR7iQT9OKXoO4zgQw/pub?gid=0&single=true&output=csv"

get_sched <- function(){
  d_sched <- readr::read_csv(sched_csv) %>%
    tidyr::fill(Module)
}

dt_sched <- function(d_sched){
  i_mod <- which(names(d_sched) == "Module") - 1

  d_sched %>%
    mutate(
      Lecture = ifelse(
        is.na(Lecture),
        "",
        ifelse(
          is.na(Lecture_link),
          Lecture,
          glue::glue("<a href='{Lecture_link}' target='_blank'>{Lecture}</a>"))),
      Lab     = ifelse(
        is.na(Lab),
        "",
        ifelse(
          is.na(Lab_link),
          Lab,
          glue::glue("<a href='{Lab_link}'     target='_blank'>{Lab}</a>"))),
      Reading = ifelse(
        is.na(Reading),
        "",
        ifelse(
          is.na(Reading_link),
          Reading,
          glue::glue("<a href='{Reading_link}'     target='_blank'>{Reading}</a>")))) %>%
    select(-Lecture_link, -Lab_link, -Reading_link, -`Lab\nTechnology`) %>%
    DT::datatable(
      rownames = F,
      extensions = 'RowGroup',
      options = list(
        dom = 't',
        rowGroup = list(
          dataSrc=c(i_mod)),
        columnDefs = list(list(visible=F, targets=c(i_mod)))),
      escape = F)
}
