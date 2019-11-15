

#' Make calls into lists
#'
#' Makes a list out of a call, returns the input otherwise
#'
#' @param element
#'
#' @return list with 2 elements
#' @export
#'
#' @examples
#' unlist_call(str2lang("print(1,2)"))
#' # $call_name
#' # print
#' #
#' # $arguments
#' # $arguments[[1]]
#' # [1] 1
#' #
#' # $arguments[[2]]
#' # [1] 2
#'
unlist_call <- function(element) {
    if (is.call(element)) {
        elem_list <- as.list(element)
        ret <- list(
            call_name = elem_list[[1]],
            arguments = elem_list[-1])
        return(ret)
    } else {
        return(element)
    }
}


#' Title
#'
#' @param my_list
#'
#' @return
#' @export
#'
#' @examples
recunlist_calls <- function(my_list) {
    tmp <- rapply(my_list, unlist_call, how = "list")

    if (identical(tmp, my_list)) {
        return(tmp)
    } else {
        recunlist_calls(tmp)
    }
}

#' Title
#'
#' @param my_list
#'
#' @return
#' @export
#'
#' @examples
parse_assign <- function(my_list) {
    assigned_var <- my_list[[1]]
    assigned_val <- my_list[-1]

    return(list(assigned_var = assigned_var,
                assigned_val = assigned_val))

}

#' Title
#'
#' @param parsed_file
#'
#' @return
#' @export
#'
#' @examples
get_global_df <- function(parsed_file) {
    tmp <- recunlist_calls(parsed_file) %>%
        purrr::transpose(.l = .) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(call_name = as.character(call_name)) %>%
        dplyr::filter(call_name == "`<-`") %>%
        dplyr::mutate(arguments = purrr::map(arguments, parse_assign))  %>%
        .[[2]] %>%
        purrr::transpose() %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(assigned_var = as.character(assigned_var))

    tmp[[2]] <- lapply(tmp[[2]], replace_singlets)
    return(tmp)
}

#' Title
#'
#' @param parsed_file
#'
#' @return
#' @export
#'
#' @examples
get_globals <- function(parsed_file) {

    ret <- get_global_df(parsed_file)

    return(ret[["assigned_var"]])
}


listify_dependency_df <- function(global_df) {
    global_df[["Dependencies"]] <- lapply(
        global_df[["assigned_val"]],
        FUN = unlist,
        recursive = FALSE,
        use.names = FALSE)

    global_df[["Dependencies"]] <- purrr::map(
        global_df[["Dependencies"]],
        as.character)
    return(global_df)
}

#' Title
#'
#' @param global_df
#'
#' @return
#' @export
#'
#' @examples
uniquify_globals <- function(global_df) {
    curr_name_map <- unique(global_df[["assigned_var"]])
    names(curr_name_map) <- curr_name_map

    global_df[["original_assigned_var"]] <- global_df[["assigned_var"]]
    global_df[["assigned_var"]] <- make.names(
        global_df[["assigned_var"]],
        unique = TRUE)

    for (row_num in seq_len(nrow(global_df))) {
        curr_row <- global_df[row_num,]

        global_deps_lgl <- global_df[["Dependencies"]][[row_num]] %in%
            names(curr_name_map)
        global_deps <- global_df[["Dependencies"]][[row_num]][global_deps_lgl]

        global_df[["Dependencies"]][[row_num]][global_deps_lgl] <-
            curr_name_map[global_deps]
        curr_name_map[[curr_row[["original_assigned_var"]]]] <-
            curr_row[["assigned_var"]]
    }
    return(global_df)
}

#' Title
#'
#' @param parsed_file
#' @param unique_names
#'
#' @return
#' @export
#'
#' @examples
get_dependencies <- function(parsed_file, unique_names = TRUE) {
    global_df <- get_global_df(parsed_file)

    global_df <- listify_dependency_df(global_df)

    if (unique_names) {
        global_df <- uniquify_globals(global_df = global_df)
    }

    var_names <- global_df[["assigned_var"]]

    global_df[["Global_Dependencies"]] <- purrr::map(
        global_df[["Dependencies"]],
        ~ unique(var_names[var_names %in% as.character(.x)]))

    global_df <- global_df[,c("Global_Dependencies", "assigned_var")]
    colnames(global_df) <- c("From", "To")


    unnested_df <- tidyr::unnest(global_df, cols = c("From"))
    return(unnested_df)
}

#' Title
#'
#' @param my_list
#'
#' @return
#' @export
#'
#' @examples
replace_singlets <- function(my_list) {
    if (is.list(my_list) & length(my_list) == 1) {
        return(my_list[[1]])
    } else {
        return(my_list)
    }
}