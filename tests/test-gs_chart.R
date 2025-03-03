library(pedbp)

################################################################################
# verify errors if args are not as expected
x <- tryCatch(gs_chart(metric = "Not-A-Metric", male = 0, source = "CDC"),
              error = function(e) e)
stopifnot(inherits(x, "error"))

x <- tryCatch(gs_chart(metric = "height_for_age", male = -1, source = "CDC"),
              error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "male == 0 | male == 1 is not TRUE"))

x <- tryCatch(gs_chart(metric = "height_for_age", male = 1, source = "NOT A SOURCE"),
              error = function(e) e)
stopifnot(inherits(x, "error"))

################################################################################
# verify ggplot object is returned
x <- gs_chart(metric = "bmi_for_age")
stopifnot(isTRUE(inherits(x, "ggplot")))

################################################################################
#                                 End of File                                  #
################################################################################
