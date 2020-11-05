x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
v <- z^2

confidence_interval <- function(data) {
  n <- length(data)
  emp_mean <- mean(data)
  emp_std_dev <- sd(data)
  half_width <- 1.96 * emp_std_dev / sqrt(n)
  c(emp_mean - half_width, emp_mean + half_width)
}
