# Lemon-Squeezer for Bounded Liekert Sums
# Gabriel Odom and Catalina Canizares
# 2023-02-14 (Happy Valentine's Day)

# We have Likert-scaled values over [10, 40] which we would like to use as a
#   dependent variable. Because the data are bounded AND the boundaries are
#   included in the values, we must both transform to [0,1], then apply the 
#   "lemon squeezer" of Smithson and Verkuilen (2006) to transform to (0,1).
# This will enable us to analyze the response with Beta regression.

sampSize_int <- 342L
syntheticData_int <- sample(10:40, size = sampSize_int, replace = TRUE)
plot(density(syntheticData_int))

Squeeze <- function(xBdd, lower, upper, squeeze = 0.5) {
	# Squeeze a BDD r.v. to (0,1)
	# I:
	#   xBdd - realizations from a bounded random variable
	#   lower - the theoretical lower bound for xBdd
	#   upper - the theoretical upper bound for xBdd
	#   squeeze - a constant to "squeeze" values on the boundary away from it. See
	#     Details for more information
	# O: the r.v. scaled to (0,1) **NON**-inclusive (so that Beta regression can
	#   be applied)
	# Details: We first scale x to [0,1] by the transformation x1 = (x - lower) /
	#   (upper - lower); then we "squeeze" values on {0,1} by the transformation
	#   x2 = (x1 * (SampleSize - 1) + squeeze) / SampleSize.
	#   See \doi{10.1037/1082-989X.11.1.54} for more info.
	# Examples:
	#   
	
	N <- length(xBdd)
	x1 <- (xBdd - lower) / (upper - lower)
	x2 <- (x1 * (N - 1) + squeeze) / N
	x2
	
}

# Test
Squeeze(
	xBdd = syntheticData_int,
	lower = 10L, upper = 40L
)
plot(
	density(
		Squeeze(
			xBdd = syntheticData_int,
			lower = 10L, upper = 40L
		)
	)
)
