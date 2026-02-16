# Day 2: Learning about linear models
# 2025-02-04
# Written by Gregor Gorjanc and Gabriela Mafra Fortuna

rm(list = ls()) # cleans your working environment
# Set working directory
#dir <- "/Users/crochus/Documents/Courses/ILRI_workshop/data_training"
setwd("/Users/crochus/Documents/Courses/ILRI_workshop/data_training")

# ---- Data ----

# Read in the phenotype data
#dat <- read.csv(file = paste0(dir, "/CT_traits_724_pc_res.csv"))
dat <- read.csv("CT_traits_724_pc_res.csv")

# Look at first few lines of the data
head(dat, n = 10)

# Look at data structure and variable/column definitions
str(dat)

# Summarise live weight
summary(dat$LW)
var(dat$LW)
var(dat$LW, na.rm = TRUE)
sd(dat$LW, na.rm = TRUE)
hist(dat$LW)

# Removing records that don't have live weight data
sel <- is.na(dat$LW)
sum(sel)
nrow(dat) # 724
dat <- dat[!sel, ] # removing the live weight with NAs
nrow(dat) # 567

# Setting factors
dat$id <- factor(dat$id)
dat$sex <- factor(dat$sex)
dat$Year <- factor(dat$Year)
dat$litter <- factor(dat$litter)
dat$LS <- factor(dat$LS)
dat$DamAge <- factor(dat$DamAge)
dat$Group <- factor(dat$Group)
dat$Line_a <- factor(dat$Line_a)

# Look at data structure and variable/column definitions
str(dat)

# ---- Sex factor ----

# Data structure by sex
table(dat$sex)

# Calculate mean live weight by sex
tmp <- tapply(X = dat$LW, INDEX = dat$sex, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model with sex factor
fit <- lm(formula = LW ~ sex, data = dat)
summary(fit)

# Check model residuals
hist(resid(fit))

# Check model fit visually
plot(resid(fit) ~ dat$LW)
abline(h = mean(resid(fit)))
abline(v = mean(dat$LW))

# ---- Sex & Year factor ----

# Data structure by year
table(dat$Year)

# Calculate mean live weight by year
tmp <- tapply(X = dat$LW, INDEX = dat$Year, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model with Year factor
fit <- lm(formula = LW ~ Year, data = dat)
summary(fit)

# Fit a linear model with Sex & Year factor
fit <- lm(formula = LW ~ sex + Year, data = dat)
summary(fit)

# Check model residuals
hist(resid(fit))

# ---- Sex, Year, & OTHER factors ----

# Data structure by litter size
table(dat$LS)

# Calculate mean live weight by litter size
tmp <- tapply(X = dat$LW, INDEX = dat$LS, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model by adding LS factor
fit <- lm(formula = LW ~ sex + Year + LS, data = dat)
summary(fit)

# Data structure by age of dam (=parity)
table(dat$DamAge)

# Calculate mean live weight by age of dam
tmp <- tapply(X = dat$LW, INDEX = dat$DamAge, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model by adding DamAge factor
fit <- lm(formula = LW ~ sex + Year + LS + DamAge, data = dat)
summary(fit)
# --> very large standard errors for dam age level estimates!

# First age of dam level has very few records --> let's merge
dat$DamAge2 <- as.character(dat$DamAge)
sel <- dat$DamAge2 %in% c(1, 2)
dat[sel, "DamAge2"] <- "1-2"
dat$DamAge2 <- factor(dat$DamAge2)

# Data structure by age of dam (=parity)
table(dat$DamAge2)

# Calculate mean live weight by age of dam
tmp <- tapply(X = dat$LW, INDEX = dat$DamAge2, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model by adding DamAge factor
fit <- lm(formula = LW ~ sex + Year + LS + DamAge2, data = dat)
summary(fit)

# Data structure by management group (how ewes were fed)
table(dat$Group)

# Calculate mean live weight by group
tmp <- tapply(X = dat$LW, INDEX = dat$Group, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model by adding group
fit <- lm(formula = LW ~ sex + Year + LS + DamAge2 + Group, data = dat)
summary(fit)

# Data structure by line_a (sire lines)
table(dat$Line_a)

# Calculate mean live weight by line_a
tmp <- tapply(X = dat$LW, INDEX = dat$Line_a, FUN = mean, na.rm = TRUE)
round(tmp, digits = 1)

# Fit a linear model by adding line_a
fit <- lm(formula = LW ~ sex + Year + LS + DamAge2 + Group + Line_a,
          data = dat)
summary(fit)

# ---- DOB covariate ----

# Explore relationship between live weight and date of birth
plot(dat$LW ~ dat$dob)
tmp <- tapply(X = dat$LW, INDEX = dat$dob, FUN = mean)
points(tmp ~ names(tmp), pch = 19, col = "red", cex = 2)

# Fit a linear model with age covariate
fit <- lm(formula = LW ~ dob, data = dat)
summary(fit)

# Plot the data and regression line
plot(dat$LW ~ dat$dob)
tmp <- tapply(X = dat$LW, INDEX = dat$dob, FUN = mean)
points(tmp ~ names(tmp), pch = 19, col = "red", cex = 2)
abline(coef(fit), col = "red", lwd = 2)

# Calculate linear regression coefficient in a simple way
cov(dat$LW, dat$dob) / var(dat$dob)

# Centre the age covariate and refit the model
dat$dob2 <- dat$dob - mean(dat$dob)
fit <- lm(formula = LW ~ dob2, data = dat)
summary(fit)

# ---- Sex, Year, & OTHER factors and DOB covariate ----

# Fit a linear model with CYS2 factor and age covariate
fit <- lm(formula = LW ~ sex + Year + LS + DamAge2 + Group + Line_a + dob2,
          data = dat)
summary(fit)

# ---- Adding Litter effect ----

# Check data structure
table(dat$litter)
table(table(dat$litter))

# Fit a linear model with all effects & litter (fixed factor)
fitF <- lm(formula = LW ~ sex + Year + LS + DamAge2 + Group + Line_a + dob2 +
                          litter,
           data = dat)
summary(fitF)

anova(fit, fitF)

# Fit a linear mixed model with all effects & litter (random factor)
install.packages(pkg = "lme4")
library(package = "lme4")
fitR <- lmer(formula = LW ~ sex + Year + LS + DamAge2 + Group + Line_a + dob2 +
                            (1 | litter),
             data = dat)
summary(fitR)

# Plot random vs fixed litter estimates
coefFix <- coef(fitF)
sel <- grep(pattern = "litter", x = names(coefFix))
litterFix <- c(0, coefFix[sel]) #  0 for litter172
litterRan <- ranef(fitR)$litter[, "(Intercept)"]
plot(litterRan ~ litterFix, ylim = c(-20, 20))
abline(a = 0, b = 1, lty = 2)
abline(h = 0, v = 0, lty = 2)

# ---- Vector and matrix calculations (fixed effect model) ----

# Observation/records vector
y <- matrix(dat$LW)
dim(y)

# Design matrix for fixed effects
fixedPart <- LW ~ sex + Year + LS + DamAge2 + Group + Line_a + dob2
X <- model.matrix(object = fixedPart, data = dat)
dim(X)
X[1:10, 1:8]

# Left-Hand Side of the system of normal equations
LHS <- t(X) %*% X
dim(LHS)

# Right-Hand Side of the system of normal equations
RHS <- t(X) %*% y
dim(RHS)

# Solve the system of normal equations
solve(LHS, RHS)

# Solve the system of normal equations (another approach)
LHSInv <- solve(LHS)
sol <- LHSInv %*% RHS
sol

# ---- Vector and matrix calculations (mixed effect model) ----

# Observation/records vector
y <- matrix(dat$LW)
dim(y)

# Design matrix for fixed effects
fixedPart <- LW ~ sex + Year + LS + DamAge2 + Group + Line_a + dob2
X <- model.matrix(object = fixedPart, data = dat)
dim(X)
X[1:10, 1:5]

# Design matrix for random effect (litter)
randomPart <- LW ~ litter - 1
Z <- model.matrix(object = randomPart, data = dat)
dim(Z)
Z[1:10, 1:5]

# Left-Hand Side of the system of normal equations
XtX <- t(X) %*% X
dim(XtX)

XtZ <- t(X) %*% Z
dim(XtZ)

ZtX <- t(Z) %*% X
dim(ZtX)

# From lmer()
sigma_e2 <- 9.229
sigma_l2 <- 6.378

ZtZ <- t(Z) %*% Z + diag(nrow = ncol(Z)) * sigma_e2 / sigma_l2
dim(ZtZ)

LHS <- rbind(cbind(XtX, XtZ),
             cbind(ZtX, ZtZ))
dim(LHS)

# Right-Hand Side of the system of normal equations
Xty <- t(X) %*% y
Zty <- t(Z) %*% y
RHS <- rbind(Xty, Zty)
dim(RHS)

# Solve the system of normal equations
solve(LHS, RHS)

# Solve the system of normal equations (another approach)
LHSInv <- solve(LHS)
sol <- LHSInv %*% RHS
sol

se <- sqrt(diag(LHSInv) * sigma_e2)
se
