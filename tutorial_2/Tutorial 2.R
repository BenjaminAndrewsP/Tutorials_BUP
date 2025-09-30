getwd()
rm(list = ls())
graphics.off()

#### Discrete model ####

p <- numeric(500)
p
p[1] = 10
r = 2.4
k = 100

p[2] = r*(1-p[1]/k)*p[1]+p[1]
p[3] = r*(1-p[2]/k)*p[2]+p[2]
p[4] = r*(1-p[3]/k)*p[3]+p[3]
p[1:4]

for (i in 2:500) {
  
  p[i+1] <- r*(1-p[i]/k)*p[i]+p[i]

}
plot(p, type = "l", ylab = "Population size", xlab = "Time", main = "Population growth")

plot(x = p[1:i-1], y = p[2:i], type = "l", ylab = "P(t+1)", xlab = "P(t)", main = "P(t+1) vs P(t)",
     xlim = c(1,150), ylim = c(1,150))

pt <- seq(0, 150, 0.1)
ptt <- r*(1-pt/k)*pt+pt
lines(pt, ptt, col = "blue")
lines(c(1,150), c(1,150), col = "red")
lines(c(p[1], rep(p[2:99], each = 2)), y = c(rep(p[2:99], each = 2), p[100]), col = "green")

#### Continuous model ####
##### Setting the model #####
library(deSolve)
library(tidyverse)
library(ggplot2)

?ode()

state <- c(p=10)
times <- seq(0, 100,by = 0.01)
parameters <- c(r = 0.1, k = 1000)

lg <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dp <- r*(1-p/k)*p
    return(list(dp))
  })
}

out <- ode(y = state, times = times, func = lg, parms = parameters)

out.df <- data.frame(out)
plot(out.df, type = "l")

ggplot(data = out.df) +
  geom_line(mapping = aes(x = time, y = p), color = "lightblue") + 
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 0, color = "darkgrey") +
  labs(x = "Time", y = "P") +
  theme_light()

##### Model calibration #####
pop <- read.csv("pop_LG_simul_noise_small.csv")
head(pop)

range(pop$P)
length(pop$P)
plot(pop, type = "l", )
abline(h = 125, lty = 2, col = "red")

# Parameter K
mean(pop$P[50:300])

# Parameter r -----------------------------> LG objetc not found !!
tmax <- 300
k <- mean(pop$P[50:300])
x <- numeric(tmax+1)

for (i in 2:(tmax+1)) {
  x[i] <- r*x[i-1]*(1-x[i-1]/k)+x[i-1]+rnorm(1,0,x[i-1]/100)}

out.df.list <- list()
i <- 1
for (r in seq(0.01, 1, 0.01)) {
  parameters <- c(r=r, k=k)
  state <- c(p=pop$P[1])
  times <- seq(0, tmax, by = 1)
  out <- ode(y = state, times = times, func = LG, parms = parameters)
  out.df.list[[i]] <- data.frame(out)
  i <- i+1
}
r

# Fitting the model
fit <- numeric(length(out.df.list))
for (i in 1:length(out.df.list)) {
  fit[i] <- sum(abs(out.df.list[[i]]$P - pop$P[1:(tmax+1)]))
}
ind.est <- which.min(fit)
fit
