library(pedbp)

stopifnot(class(gemelli1990) == "data.frame")

# expected names:
stopifnot(all(c("age", "male", "sbp_mean", "dbp_mean", "sbp_sd", "dbp_sd") %in% names(gemelli1990)))

# testing charateristics of the data set
stopifnot( dim(gemelli1990) == c(8, 6))

stopifnot(all(gemelli1990$male %in% c(0, 1)))
