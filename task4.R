# Simon Toogood
# Task 4

# ---- Constant definitions ---- #
L <- 1000          # Distance (from apertures) to screen
a <- 0.5           # Size of each aperture
d <- 3             # Separation of the slits
lambda <- 0.05     # Wavelength of light
k <- 2*pi / lambda # Wavenumber


# ---- Function definitions ---- #
compute.wave <- function(x.0, y.0, x.1, y.1, k, amp.0=1) {
  # Given a single source of waves at point (x.0, y.0), compute the phase
  # shift and amplitude of the wave arriving at point (x.1, y.1).
  dx <- x.1 - x.0               # x distance travelled
  dy <- y.1 - y.0               # y distance travelled
  dist <- sqrt( dx^2 + dy^2 )   # Total path length
  amp <- amp.0 * dist^(-1/2)    # Amplitude decay with path length
  phase <- (k * dist) %% (2*pi) # Phase change over path length modulo 2pi
  # Put amplitude and phase into a phasor and output this.
  phasor <- complex(modulus=amp, argument=phase)
  return(phasor) }


# ----- Main Program ----- #

# Define distance along the screen, from -y.max to +y.max
y.max <- 200       # Size of the screen
screen.size <- 501 # Sets the number of pixels on the screen (ie. the resolution)
screen <- seq(-y.max, y.max, length.out=screen.size)

y.source <- c(-d/2, d/2) # Set position of two wave sources
phasor.1 = compute.wave(0, y.source[1], L, screen, k) # Compute a phasor list for the first wave
phasor.2 = compute.wave(0, y.source[2], L, screen, k)  # Compute a phasor list for the second wave
phasor.tot = phasor.1 + phasor.2 # Sum the phasors
intensity = Mod(phasor.tot)^2 # Calculate the intesity of the resultant phasor
plot(screen, intensity, type="l", xlab="Distance from centre of screen", ylab="Intensity")
