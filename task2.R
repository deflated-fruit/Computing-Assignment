# Simon Toogood
# Task 2

# ---- Function definitions ---- #
sinc <- function(x) {
  out <- sin(x) / x
  out[x == 0] <- 1
  return(out) }

# ---- Constant definitions ---- #
L <- 1000          # Distance (from apertures) to screen
a <- 0.5           # Size of each aperture
d <- 3             # Separation of the slits
lambda <- 0.05     # Wavelength of light
k <- 2*pi / lambda # Wavenumber

# Define distance along the screen, from -y.max to +y.max
y.max <- 200       # Size of the screen
screen.size <- 501 # Sets the number of pixels on the screen (ie. the resolution)
screen <- seq(-y.max, y.max, length.out=screen.size)

# Calculate the intensity at each pixel
theta <- atan(screen/L)
half.phi <- (pi*a*sin(theta)) / lambda
half.delta <- (pi*d*sin(theta)) / lambda
intensity <- (sinc(half.phi) * cos(half.delta))^2

# Plot the intensity across the screen
plot(screen, intensity, type="l", xlab="Distance from centre of screen", ylab="Intensity")
