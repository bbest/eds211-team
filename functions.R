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

  DT::datatable(
    d_sched,
    rownames = F,
    extensions = 'RowGroup',
    options = list(
      dom = 't',
      rowGroup = list(
        dataSrc=c(i_mod)),
      columnDefs = list(list(visible=F, targets=c(i_mod)))))
}
