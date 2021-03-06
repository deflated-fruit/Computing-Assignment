# Simon Toogood
# Task 5 Part 2

# ---- Constant definitions ---- #

L <- 1000          # Distance (from apertures) to screen
a <- 0.5           # Size of each aperture
d <- 3             # Separation of the slits
lambda <- 0.05     # Wavelength of light
k <- 2*pi / lambda # Wavenumber
I.0 <- 1           # Initial wave intensity
n.source <- 400    # Initial number of sources
m.source <- 1600   # Very high number of sources - good approximation
q.source <- 40     # Low number of sources - poorer approximation


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
  return(phasor)
}

interfere <- function(y.screen, y.source, L, k) {
  n.source <- length(y.source) # Number of sources in total
  n.screen <- length(y.screen) # Number of screen pixels
  # Spread the amplitude over the individual sources
  amp.0 <- sqrt(I.0) / n.source
  
  # Array to store combined phasor (amplitude, phase) at each screen pixel
  phasor <- complex(length.out = n.screen)
  
  # Loop over each source inside the aperture
  for (i in 1:n.source) {
    # Combine a phase and an amplitude into a phasor
    phasor.i <- compute.wave(x.0=0, y.0=y.source[i], x.1=L, y.1=y.screen, k, amp.0)
    # Combine phasors for each source i
    phasor <- phasor + phasor.i
  }
  return(phasor) # Output the summed phasor array
}

generate.sources = function(from, to, N) {
  # Generate a list of wave source y-coordinates from 'from' to 'to' and -'from' to -'to'
  y.source.1 <- seq(-from, -to, length.out=N/2)
  y.source.2 <- seq( from,  to, length.out=N/2)
  return(c(y.source.1, y.source.2))
}


# ---- Main Program ----- #

# Define distance along the screen, from -y.max to +y.max
y.max <- 200       # Size of the screen
screen.size <- 501 # Sets the number of pixels on the screen
screen <- seq(-y.max, y.max, length.out=screen.size)

# Create a list of wave sources for each number of sources
slit.min = d/2 - a/2
slit.max = d/2 + a/2
y.n = generate.sources(slit.min, slit.max, n.source)
y.m = generate.sources(slit.min, slit.max, m.source)
y.q = generate.sources(slit.min, slit.max, q.source)

# Compute interference for each number of sources
phasors.n <- interfere(screen, y.n, L, k)
phasors.m <- interfere(screen, y.m, L, k)
phasors.q <- interfere(screen, y.q, L, k)

# Calculate the intensity on the screen
int.n <- Mod(phasors.n)^2
int.m <- Mod(phasors.m)^2
int.q <- Mod(phasors.q)^2

# Plot each intenisty curve
plot(screen, int.n, type="l", xlab="Distance from centre of screen", ylab="Intensity")
lines(screen, int.m, col="red")
lines(screen, int.q, col="blue")
