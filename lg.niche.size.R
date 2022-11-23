lg <- read.csv("Gibbusadj.csv",stringsAsFactors = FALSE)

attach(lg)
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
n_mean=tapply(N,list(Location), mean) #group fish into means by year
n_meanD=as.data.frame(n_mean)
c_mean=tapply(C,list(Location), mean) #group fish into means by year
c_meanD=as.data.frame(c_mean)
fish=data.frame(n_meanD,c_meanD)
fish$Location=rownames(fish)#create dataframe so ggplot can read data
detach(lg)
fish <-lg
aggregate(fish[5:6], fish[1], mean)



# generate parameter draws from the 'default' posteriors of each fish
nsamples <- 10000
system.time({
  fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                        X = fish[ii, 5:6]))
})

# mu1 (del15N), mu2 (del13C), and Sigma12
clrs <- c("#FF6F00B2","#8A4198B2","#C71000B2")
par(mar = c(4, 4, 0.5, 0.1) + 0.1, mfrow = c(1, 3))
niche.par.plot(fish.par, col = clrs, plot.index = 1)
niche.par.plot(fish.par, col = clrs, plot.index = 2)
niche.par.plot(fish.par, col = clrs, plot.index = 1:2)
legend("topleft", legend = names(fish.par), fill = clrs)

# all mu (del15N, del13C)
niche.par.plot(fish.par, col = clrs, plot.mu = TRUE, plot.Sigma = FALSE)
legend("topleft", legend = names(fish.par), fill = clrs)

# all mu and Sigma
par(mar = c(4.2, 4.2, 2, 1) + 0.1)
niche.par.plot(fish.par, col = clrs, plot.mu = TRUE, plot.Sigma = TRUE)
legend("topright", legend = names(fish.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 10
fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                      X = fish[ii, 5:6]))
# format data for plotting function
fish.data <- tapply(1:nrow(fish), fish$Location, function(ii) X = fish[ii, 5:6])

niche.plot(niche.par = fish.par, niche.data = fish.data, pfrac = 0.05, iso.names = expression(delta^{
  15
} * N, delta^{
  13
} * C), col = clrs, xlab = expression("Isotope Ratio (per mil)"))

# niche overlap plots for 95% niche region sizes
nsamples <- 10000
fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                      X = fish[ii, 5:6]))
# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher
# accuracy.  the variable over.stat can be supplied directly to the
# overlap.plot function

over.stat <- overlap(fish.par, nreps = nsamples, nprob = 10000, alpha = c(0.95,0.99))

# The mean overlap metrics calculated across iteratations for both niche
# region sizes (alpha = .95 and alpha = .99) can be calculated and displayed
# in an array.
over.mean <- apply(over.stat, c(1:2, 4), mean) * 100
round(over.mean, 2)

over.cred <- apply(over.stat * 100, c(1:2, 4), quantile, prob = c(0.025, 0.975), 
                   na.rm = TRUE)
round(over.cred[, , , 1])  # display alpha = .95 niche region

# Overlap plot.Before you run this, make sure that you have chosen your
# alpha level.
over.stat <- overlap(fish.par, nreps = nsamples, nprob = 10000, alpha = 0.95)
overlap.plot(over.stat, col = clrs, mean.cred.col = "#3D3B25B2", equal.axis = TRUE, 
             xlab = "Overlap probability (%) -- Niche region size: 95%")

tiff("lg.overlap.tiff", width = 9, height = 6.5, units = 'in', res = 300)
overlap.plot(over.stat, col = clrs, mean.cred.col = "#3D3B25B2", equal.axis = TRUE,
              xlab = "Overlap probability (%) -- Niche region size: 95%")
dev.off()

# calculation of niche size
# posterior distribution of (mu, Sigma) for each species
nsamples <- 10000
fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                      X = fish[ii, 5:6]))
# https://rdrr.io/github/mlysy/nicheROVER/src/R/niche.size.R
niche.size <- function(Sigma, alpha = .95) {
  Sigma <- as.matrix(Sigma)
  n <- nrow(Sigma)
  sz <- as.numeric(determinant(Sigma, logarithm = TRUE)$modulus)
  sz <- .5 * (sz + n * (log(pi) + log(qchisq(alpha, df = n)))) - lgamma(.5*n+1)
  exp(sz)
}

# posterior distribution of niche size by species
fish.size <- sapply(fish.par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})


# point estimate and standard error
rbind(est = colMeans(fish.size),
      se = apply(fish.size, 2, sd))

# E. areolatus E. multinotatus L. punctulatus L. sebae P. maculatus
# est    4.9574689       10.864757      10.001607 6.643900     9.442132
# se     0.9335154        2.057572       1.601639 1.058486     2.602830

par(mfrow = c(1,2))

lg.niche.size <- boxplot(fish.size, col = clrs, pch = 16, cex = .5,
                         ylab = "Niche Size", xlab = "Location", ylim = c(0,30))


tiff("lg.niche.size.tiff", width = 9, height = 6.5, units = 'in', res = 300)
niche.size.box <- boxplot(fish.size, col = clrs, pch = 16, cex = .5,ylab = "Niche Size", xlab = "Location", ylim = c(0,35))
dev.off()