x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
v <- z^2

# Function that computes a 95% Confidence Interval given a numeric vector
confidence_interval <- function(data) {
  if (!checkmate::test_atomic_vector(data)) {
    stop("supplied data is not a vector")
  }
  if (!checkmate::test_numeric(data)) {
    stop("supplied data is not numeric")
  }
  n <- length(data)
  emp_mean <- mean(data, na.rm = TRUE)
  emp_std_dev <- sd(data, na.rm = TRUE)
  half_width <- 1.96 * emp_std_dev / sqrt(n)
  c(emp_mean - half_width, emp_mean + half_width)
}

confidence_interval(x)
confidence_interval(y)
confidence_interval(z)
confidence_interval(v)
