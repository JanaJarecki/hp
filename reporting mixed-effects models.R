library(lme4) # if package isn't existing: install.packages("lme4")
library(ggplot2); library(themejj); theme_set(themejj()); library(patchwork)

# Simulate data
# Save this later in a separate data file to be downloaded from git
set.seed(718) # 754
N <- 3
participant.df <- data.frame(unit = c(1:N), a = sample(20:50, N, rep=T))
head(participant.df)
participant.df$E.alpha.given.a <-  2 * participant.df$a / 10
participant.df$E.beta.given.a  <-  2.7 * participant.df$a / 10
head(participant.df)

# Add noise that displace slopes and intercepts from conditional participant means given alpha
# Modelling this as bivariate normal distribution
library(mvtnorm)
q = 0.15 # std.dev of [1,1]
r = 0.9 # std.dev diagonals
s = 0.5 # std.dev of [2,2] 
cov.matrix <- matrix(c(q^2, r * q * s, r * q * s, s^2), nrow = 2, byrow = TRUE)
random.effects <- rmvnorm(N, mean = c(0, 0), sigma = cov.matrix)
participant.df$alpha <- participant.df$E.alpha.given.a + random.effects[, 1]
participant.df$beta <- participant.df$E.beta.given.a + random.effects[, 2]
head(participant.df)

# We now move to the within-unit level, generating a fixed predictor variable grid, the x-grid, to be shared among units
J <- 4
M = J * N  #Total number of observations
x.grid = 1:J  - 1
within.participant.df <-  data.frame(
  unit = sort(rep(c(1:N), J)),
  j = rep(c(1:J), N),
  x = rep(x.grid, N))

# Next, we generate data from the individual linear models based on αi, βi, and within-unit level noise 
# using noise of 0.75 x normal distributioon
flat.df <- merge(participant.df, within.participant.df)
flat.df <-  within(flat.df, y <-  alpha + x * beta + 0.6 * rnorm(n = M))
simple.df <-  flat.df[, c("unit", "a", "x", "y")]
head(simple.df)

# Rename to our liking
names(simple.df) <- c("participant", "age", "question", "answer")
simple.df$participant <- paste0('p', simple.df$participant)
head(simple.df)


library(lmerTest)
lmm.model.ri.rs <-  lmer(answer ~ question + (1 + question | participant), data = simple.df)
AIC(lmm.model.ri.rs)
summary(lmm.model.ri.rs)
anova(lmm.model.ri.rs)

# random intercept only
lmm.model.ri <-  lmer(answer ~ question + (1 | participant), data = simple.df)
AIC(lmm.model.ri)
summary(lmm.model.ri)

# random slope only
lmm.model.rs <-  lmer(answer ~ question + (question - 1 | participant), data = simple.df)
AIC(lmm.model.ri)
summary(lmm.model.ri)



# Get the coefficients to plot them
slope.interc <- data.frame(coef(lmm.model.ri.rs)[["participant"]], participant = unique(simple.df$participant))
names(slope.interc)[1:2] <- c('intercept', 'slope')

interc <- data.frame(coef(lmm.model.ri)[["participant"]], participant = unique(simple.df$participant))
names(interc)[1:2] <- c('intercept', 'slope')

slope <- data.frame(coef(lmm.model.rs)[["participant"]], participant = unique(simple.df$participant))
names(slope)[1:2] <- c('intercept', 'slope')


# base plot
ymax <- max(simple.df$answer + 5)
p <- ggplot(simple.df, aes(x = question, y = answer)) +
  geom_point(alpha = .3, size = 3, aes(color = participant)) +
  scale_x_continuous(expand = c(0.06,0), limits = c(0,NA)) +
  scale_y_continuous(expand = c(0.06,0), limits = c(0,ymax)) +
  theme(plot.caption = element_text(size = rel(.6), family = "Monoid", color = "grey"))

colors.rirs <- c(p3='#6f504b', p2='#b91700',  p1='#b1b7ff')

ppa <- p +
  labs(title = "Data by participant", subtitle = "", caption = paste("N =",N)) +
  scale_color_manual(values = colors.rirs)

pri <- p +
  labs(title = "Mixed-effects interc.", subtitle = "Varying intercept, one slope", caption = paste("call:", as.character(lmm.model.ri@call)[2])) +
  geom_abline(data = interc, aes(slope = slope, intercept = intercept, color = participant), size = 1) +
  scale_color_manual(values = colors.rirs)

prs <- p +
  labs(title = "Mixed-effects slope", subtitle = "One intercept, varying slope", caption = paste("call:", as.character(lmm.model.rs@call)[2])) +
  geom_abline(data = slope, aes(slope = slope, intercept = intercept, color = participant), size = 1) +
  scale_color_manual(values = colors.rirs)

prirs <- p +
  labs(title = "Mixed-effects full", subtitle = "Varying intercept, varying slope", caption = paste("call:", as.character(lmm.model.ri.rs@call)[2])) +
  geom_abline(data = slope.interc, aes(slope = slope, intercept = intercept, color = participant), size = 1) +
  scale_color_manual(values = colors.rirs)

pre <- p +
  labs(title = "Standard Regression", subtitle = "One intercept, one slope", caption = paste("N =",N)) +  
  geom_smooth(method = 'lm', se = FALSE, color = 'black', size = 1) +
  scale_color_manual(values = rep('grey40', 5))

pre + ppa + prirs +prs +pri

pre

anova(lmm.model.ri.rs)
summary(lmm.model.ri.rs)