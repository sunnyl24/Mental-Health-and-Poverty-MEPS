modify_column_names <- function(tbl, labels) {
  # Use modify_header() to rename columns in a gtsummary table
  tbl %>%
    modify_header(update = labels)
}