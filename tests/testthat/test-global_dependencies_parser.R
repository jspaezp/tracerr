test_that("Getting Dependencies from Expression With unique names work", {

    r_str <- "
    b <- 1:2
    a <- sum(b)
    c <- sum(a)
    print(c)
    "
    parsed_expr <- parse(text = r_str)

    expect_equal(
        get_dependencies(parsed_expr),
        dplyr::tibble(
            From = c("b", "a"),
            To = c("a", "c")))
})


test_that("Getting Dependencies from Expression With unique names and multiple usages", {

    r_str <- "
    b <- 1:2
    a <- sum(b)
    a2 <- sum(b)
    c <- sum(a)
    print(c)
    "
    parsed_expr <- parse(text = r_str)

    expect_equal(
        get_dependencies(parsed_expr),
        dplyr::tibble(
            From = c("b", "a", "b"),
            To = c("a", "c", "a2")))
})

test_that("Getting Dependencies from Expression With duplicate names work", {

    r_str <- "
    b <- 1:2
    a <- sum(b)
    a <- sum(a)
    c <- sum(a)
    print(c)
    "
    parsed_expr <- parse(text = r_str)

    expect_equal(
        get_dependencies(parsed_expr, unique_names = FALSE),
        dplyr::tibble(
            From = c("b", "a", "a"),
            To = c("a", "c", "a")))

    expect_equal(
        get_dependencies(parsed_expr, unique_names = TRUE),
        dplyr::tibble(
            From = c("b", "a.1", "a"),
            To = c("a", "c", "a.1")))
})

