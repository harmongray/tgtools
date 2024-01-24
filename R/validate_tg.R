#' Processing data returned from the telethon channel.getMessages Python Telethon
#'  interface for accessing the Telegram API
#' @param tg_list a list of data.frame objects or a data.frame object. All data
#'  in each data.frame object have their vector types normalized
#' @param bind a boolean arg to coerce to data.frame object
#' @export
validate_tg <- function(tg_list, bind = FALSE) {

  if (is.data.frame(tg_list) == TRUE) {

    tg_list <- as.list(tg_list)

    return(tg_list)

  }

      tg_list <- lapply(tg_list, function(df) {

        channel_index <- match("channel", names(df))
        df[, channel_index] <- as.character(df[, channel_index])

        member_index <- match("member_count", names(df))
        df[, member_index] <- as.logical(df[, member_index])

        broadcast_index <- match("broadcast", names(df))
        df[, broadcast_index] <- as.logical(df[, broadcast_index])

        id_index <- match("id", names(df))
        df[, id_index] <- as.numeric(df[, id_index])

        timestamp_index <- match("timestamp", names(df))
        df[, timestamp_index] <- as_datetime(df[, timestamp_index])

        content_index <- match("content", names(df))
        df[, content_index] <- as.character(df[, content_index])

        user_id_index <- match("user_id", names(df))
        df[, user_id_index] <- as.numeric(df[, user_id_index])

        first_last_index <- match("first_and_last_name", names(df))
        df[, first_last_index] <- as.character(df[, first_last_index])

        username_index <- match("username", names(df))
        df[, username_index] <- as.character(df[, username_index])

        views_index <- match("views", names(df))
        df[, views_index] <- as.numeric(df[, views_index])

        edit_date_index <- match("edit.date", names(df))
        df[, edit_date_index] <- as_datetime(df[, edit_date_index])

        forward_index <- match("forward", names(df))
        df[, forward_index] <- as.character(df[, forward_index])

        forward_id_index <- match("forward_id", names(df))
        df[, forward_id_index] <- as.character(df[, forward_id_index])

        forward_msg_index <- match("forward_msg_id", names(df))
        df[, forward_msg_index] <- as.character(df[, forward_msg_index])

        forward_date_index <- match("forward_date", names(df))
        df[, forward_date_index] <- as_datetime(df[, forward_date_index])

        url_index <- match("URLs", names(df))
        df[, url_index] <- as.character(df[, url_index])

        media_index <- match("media", names(df))
        df[, media_index] <- as.character(df[, media_index])

        hasComments_index <- match("hasComments", names(df))
        df[, hasComments_index] <- as.logical(df[, hasComments_index])

        isComment_index <- match("isComment", names(df))
        df[, isComment_index] <- as.logical(df[, isComment_index])

        bot_url_index <- match("bot_url", names(df))
        df[, bot_url_index] <- as.character(df[, bot_url_index])

        parent_index <- match("parent", names(df))
        df[, parent_index] <- as.character(df[, parent_index])

        isDeleted_index <- match("isDeleted", names(df))
        df[, isDeleted_index] <- as.logical(df[, isDeleted_index])

        df$day <- as.Date(df$timestamp)

        return(df)

    })

    if (bind) {

      tg_list <- bind_rows(tg_list)

  }

  return(tg_list)

}
