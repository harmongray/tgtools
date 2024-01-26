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
#' This is a lambda function to initialize the column based on the type, each being
#'  constructed with a data.frame of every column name per vector type.
#' @param col_names A vector of strings representing the names of the columns to
#'  be coerced to the same vector type.
#' @param type_func A function that specifies the data type to which each each
#'  column should be initalized.
#' @noRd
.initialize_cols <- function(col_names, type_func) {

  lapply(setNames(nm = col_names), function(x) type_func(NA))

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
#' @param datecol A Boolean argument to add a "date" column, which is an as.date()
#'  coercion that makes per day statistical data easier.
#'
#' @return A data.frame (or list of data.frames if bind = FALSE)
#'
#' @export
tg_normalize <- function(tg_list, lib = "telescrape", bind = FALSE, datecol=FALSE) {

    # valid column names
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

    # future .json export definition will go here:

    # define chosen library
    header.defined <- if (lib == "telescrape") telescrape_header else api_definition

    # process data frame
    process_data_frame <- function(df) {
      if (!.validate_cols(df, names(header.defined))) {
        stop("Data frame contains invalid columns. Is the data source invalid or modified?")
      }
      .coerce_df(df, header.defined)
    }

    # if data.frame:
    if (is.data.frame(tg_list)) {
      inter <- process_data_frame(tg_list)

      # else if list of data.frames:
    } else if (is.list(tg_list)) {
      inter <- lapply(tg_list, process_data_frame)

      # if bind, do bind_rows:
      if (bind) {
        inter <- bind_rows(inter)
      }

    # if invalid input:
    } else {

      stop("Invalid input type")

    }

    # return inter (combined data.frame / or the list of data.frames if bind = FALSE):

    inter$edit.date <- as_datetime(inter$edit.date)
    inter$forward_date <- as_datetime(inter$forward_date)
    inter$timestamp <- as_datetime(inter$timestamp)

    if ("date" %in% colnames(date)) {

      inter$date <- as.Date(inter$timestamp)

    }
    if (datecol) {

      inter$date <- as.Date(inter$timestamp)

    }

    return(inter)

  }
