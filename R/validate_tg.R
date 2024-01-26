#' .coerce_df internal
#' @param x A data.frame object with columns that need to be coerced
#' @param vec A reference data.frame object with target data types.
#'
#' @return A data.frame object with the same structure as `x`, but with columns
#'  coerced to match those in `vec`.
#'
#' @keywords internal
.coerce_df <- function(x, vec) {
  y <- sapply(names(x), function(colname) {
    column_data <- x[[colname]]

    if (colname %in% names(vec)) {
      type <- class(vec[[colname]])
      column_data <- type.convert(column_data, as.is=TRUE, mode=type)
    }

    return(column_data)
  })

  return(as.data.frame(y))
}

#' Validate Telegram Data
#'
#' `validate_tg` validates the input data from a list of data.frames or a data.frame
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
#' @return A data.frame (or list of data.frames if bind=FALSE)
#'
#' @export
validate_tg <- function(tg_list, lib, bind = FALSE) {
  telescrape_header <- data.frame(
    channel = character(),
    member_count = logical(),
    broadcast = logical(),
    id = numeric(),
    timestamp = as_datetime(character()),
    content = character(),
    user_id = numeric(),
    first_and_last_name = character(),
    username = character(),
    views = numeric(),
    edit.date = as_datetime(character()),
    forward = character(),
    forward_id = character(),
    forward_msg_id = character(),
    forward_date = as_datetime(character()),
    media = character(),
    hasComments = logical(),
    isComment = logical(),
    bot_url = character(),
    parent = character(),
    isDeleted = logical()
  )

  api_definition <- data.frame(channel_name = character())

  if (lib %in% c("telescrape", "api")) {
    if (lib == "telescrape") {
      header.defined <- telescrape_header
    } else if (lib == "api") {
      header.defined <- api_definition
    }

    if (is.data.frame(tg_list)) {
      inter <- .coerce_df(tg_list, header.defined)
      inter$day <- as.Date(inter$timestamp)

    } else if (is.list(tg_list)) {
      inter <- lapply(tg_list, function(x) {
        validate_tg(x, lib = lib)
      })

      if (bind) {
        inter <- inter |> bind_rows()
      }

    } else {
      stop("Supplied data is not valid for trasnformation. Did you change any names?")
    }

  } else {
    stop("Invalid API reference string supplied")
  }

  return(inter)
}
