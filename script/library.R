library(tidyverse)

f_read_file <- function(file, cols, n_max=Inf) {
  file_extension <- tools::file_ext(file)
  if (file_extension == "csv") {
    df <- read_csv(file, col_select = any_of(cols), locale = locale(encoding = "CP949"), n_max=n_max)
  } else if (file_extension == "tsv") {
    df <- read_tsv(file, col_select = any_of(cols), locale = locale(encoding = "CP949"), n_max=n_max)
  } else {
    stop("Unsupported file extension")
  }
  return(df)
}