##load the 2019 spreadsheet
{
library(tidyverse)
library(readxl)
library(xlsx)
library(altools)
}

####--------------------bring in the data----------------------------------------------
path <- paste0(dirname(dirname(getwd())), "/bookeeping/2019/NewGraph_bookkeeping_2019_20191113.xls") #looks 2 levels up

prefix <- "Month "
suffix <- 1:12
search_list <- paste0(prefix, suffix)

books_raw <- search_list %>% 
  set_names() %>% 
  map(read_excel, 
      path = path, 
      .name_repair = ~ make.names(.x, unique = TRUE),
      col_types = "text")


##clean up the sheets
at_trim_xlsheet <- function(df, column_last = ncol(df), row_first = 1) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    dplyr::slice(row_first:nrow(.)) %>%
    purrr::set_names(nm = tolower(unlist(slice(.,1)))) %>%
    dplyr::slice(., -1) %>% 
    # tidyr::drop_na(1) %>% 
    dplyr::mutate(date = at_time_excel_number(as.numeric(date)))
}


books <- books_raw %>%
  map(at_trim_xlsheet, row_first = 8) %>% 
  bind_rows() %>% 
  dplyr::mutate(date = at_time_excel_number(as.numeric(date))) %>% 
  as_tibble(., .name_repair = ~ make.names(.x, unique = TRUE)) %>% 
  mutate_at(vars(-date, -description, -`reference..`, -bank.rec), as.numeric) ##change the class

vehicle_expenses <- books %>% 
  select(date, description, reference.., vehicle.expense, gst.paid.out) %>% 
  filter(!is.na(vehicle.expense))


##burn to excel
write.xlsx2(vehicle_expenses, file = paste0(dirname(dirname(getwd())), '/bookeeping/2019/vehicle_expenses.xlsx'), sheetName="export",
            col.names=TRUE, row.names=TRUE, append=FALSE, password=NULL)
