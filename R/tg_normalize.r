#' Coerce the data.frame validated data.frame vectors
#'
#' `.coerce_df` is a lambda that coerces the types of the vectors in a
#'    data.frame supplied by `tg_normalize`.
#'
#' @param x A data.frame object with columns that need to be coerced
#' @param vec A reference data.frame object with target data types.
#'
#' @return A data.frame object with the same structure as `x`, but with columns
#'  coerced to match those in `vec`.
#' @noRd
.coerce_df <- function(df, vec) {
  df[] <- lapply(names(df), function(colname) {
    column_data <- df[[colname]]

    if (colname %in% names(vec)) {
      type <- class(vec[[colname]])
      column_data <- type.convert(column_data, as.is=TRUE, mode=type)
    }

    return(column_data)
  })

  return(df)
}

#' Validate column names based on string
#'
#' `.validate_cols` validates the given columns supplied the tg_list parameter in
#'  `tg_normalize`.
#'
#' @param df A data frame whose columns are to be validated.
#' @param valid_cols A vector of strings representing the name of valid columns.
#'  in `tg_normalize`.
#' @noRd
.validate_cols <- function(df, valid_cols) {
  all(names(df) %in% valid_cols)
}


#' Initialize the columns based on type
#'
#' This is a lambda to initialize the column based on the type, each being
#'  constructed with a data.frame of every column name per vector type.
#' @param col_names A vector of strings representing the names of the columns to
#'  be coerced to the same vector type.
#' @param type_func A function that specifies the data type to which each each
#'  column should be initalized.
#' @noRd
.initialize_cols <- function(col_names, type_func) {

  lapply(setNames(nm = col_names), function(x) type_func(NA))

}


#' Process URLs in flat data frame if lib = "telescrape"
#'
#' @param table A data.frame containing the column 'URLs'. Each row in the table
#'  may be empty, a single element, or a list of elements. `.vector_logic` coerces
#'  to a list via `split_str(table$column[[i]], pattern = ",")` to allow simple
#'  conditional access to each element. This case is currently concerned with the
#'  case of removing an unmatched parenthesis.
#' @noRd
.fix_url_vec <-  function(table = table) {

  for (i in 1:nrow(table)) {

    # This uses table$URLs -- if you needed, any other field could be modified
    # here to modify any data with flattened, but nestable data

    # creating the split_string list:
    split_string <- str_split(table$URLs[[i]], pattern=",")

    # access vector
    for (j in seq_along(split_string)) {

      # anything needing done to a vector happens here
      # conditional ... e.g.:
      # if (str_count(split_string[[j]]))


      # access elements
      for (k in seq_along(split_string[[j]])) {

        # anything needing done in elements happens here
        # single element:
        k_elem <- trimws(split_string[[j]][k])

        if (str_count(k_elem, pattern = "\\)") > 0) {

          k_elem <- str_remove(k_elem, pattern = "\\)")
          split_string[[j]][k]

        }

        # reassigning the output of k_elem transformations:
        split_string[[j]][k] <- k_elem

      }

    }

    # flatten it out for the data.frame
    table$URLs[i] <- paste(unlist(split_string[j]), collapse = ",")

  }

  return(table)

}


#' Validate Telegram Data
#'
#' `tg_normalize` normalizes the input data from a list of data.frames or a data.frame
#' object with the option of binding them to a single data.frame.
#'
#' @param tg_list A list of data.frame objects or a data.frame object. All data
#' in each data.frame object have their vector types normalized.
#'
#' @param lib A string with the name of the library API that constructed the
#' supplied data.frame object.
#'
#' @param bind A Boolean argument to determine if the list of data.frame objects
#' should be bound into a single data.frame object. If TRUE, a single data.frame
#' is returned. If FALSE, a list of data.frames is returned.
#'
#' @return A data.frame (or list of data.frames if bind = FALSE)
#' @export
tg_normalize <- function(tg_list, lib = "telescrape", bind = FALSE, datecol=FALSE) {

  if (lib == "telescrape") {

      # telescrape telegram message scraper ("github.com/PeterWalchhofer/Telescrape")

      # valid column names
      # future .telescrape_cols?
      logical_cols <- c("member_count", "broadcast", "isDeleted",
                      "hasComments", "isComment")
      numeric_cols <- c("id", "user_id", "views", "replyToId")
      character_cols <- c("channel", "content", "first_and_last_name", "username",
                        "bot_url", "parent", "forward",
                        "forward_id", "forward_msg_id", "media", "URLs")
      datetime_cols <- c("timestamp", "edit.date", "forward_date")
      date_cols <- c("date")

      # cbind valid columns
      telescrape_header <- cbind.data.frame(
        .initialize_cols(logical_cols, as.logical),
        .initialize_cols(numeric_cols, as.numeric),
        .initialize_cols(character_cols, as.character),
        .initialize_cols(datetime_cols, as.character),
        .initialize_cols(date_cols, as.character)
      )

      # define header given header library
      header.defined <- telescrape_header

      # process data.frame
      process_data_frame <- function(df) {
        if (!.validate_cols(df, names(header.defined))) {
          stop("Data frame contains invalid columns. Is the data source invalid or modified?")
        }
        .coerce_df(df, header.defined)
      }

      if (is.data.frame(tg_list)) {

        # this is where `tg_list` is passed to the `df` args in utils:
        table <- process_data_frame(tg_list)

        # else if list of data.frames:
      } else if (is.list(tg_list)) {
        table <- lapply(tg_list, process_data_frame)

        # this is where to put logic for handling .vector_logic by looping over
        # each data.frame in the list of `tg_list`

        # if bind, do bind_rows:
        if (bind) {
          table <- bind_rows(table)
        }

        # if invalid input:
      } else {

        stop("Invalid input type")

      }

        table$edit.date <- as_datetime(table$edit.date)
        table$forward_date <- as_datetime(table$forward_date)
        table$timestamp <- as_datetime(table$timestamp)

    }

  # future .json export definition will go here:

  # telescrape library has a weird regex mistake? it's deprecated, so this is necessary:
  # Weird scoping thing, we have to put this here:

  if (lib == "telescrape") {
    table <- .fix_url_vec(table = table)
    return(table)
  }

}
