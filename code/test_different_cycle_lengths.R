# test if different cycle lengths give same results

n <- 100 # number of units
nk <- 5 # number of knots

# generate x on the interval allown using uniform distribution
x1 <- runif(n, min = 0, max = 1) # x on [0,1]
x365 <- x1 * 365 # x on [0, 365]

# simulation of the probabilities
x.transf.2 = x.transf = sine_function(x1) 

# # but from now on, we have to transform the X to keep it inside [0,1] using modulus division, e.g. (x %% 1)
# x <- x %% 1

# simulation of the binary events
y = ifelse(runif(n) < x.transf.2, 1, 0) # event happens if generated random number [0,1] smaller than simulated probability
# TODO: why not use rbinom() ?

# transformation on the linear predictor scale
x.transf.2.lp = log(x.transf.2 / (1 - x.transf.2))

# generation of the design matrices ####
# classic RCS
x.rcs.1 = rcspline.eval(x1, nk = nk, inclx = TRUE)
x.rcs.365 = rcspline.eval(x365, nk = nk, inclx = TRUE)
#saving the knots used for the splines, uses the default from rms package
my.knots.1 = attr(x.rcs.1, "knots")
my.knots.365 = attr(x.rcs.365, "knots")

#periodic RCS
x.rcs.per.1 = rcs.per(x1, nk = nk)
x.rcs.per.365 = rcs.per(x365, nk = nk)

# periodic cubic splines
x.cs.per.1 = cs.per(x1, nk=nk)
x.cs.per.365 = cs.per(x365, nk=nk)

# if(is.null(quantiles.cs)){ 
#   x.cs.per = cs.per(x, nk=nk, xmin=min.th.x, xmax=max.th.x)
#   my.knots.cs = my.knots
# } else {
#   my.knots.cs = as.numeric(quantile(ecdf(x), quantiles.cs))
#   x.cs.per = cs.per(x, knots=my.knots.cs, nk=NULL, xmin=min.th.x, xmax=max.th.x)
# }

# estimate models
mod.rcs.per.1 = glm(y~x.rcs.per.1, family="binomial") # model binary responses with different versions of splines
mod.rcs.per.365 = glm(y~x.rcs.per.365, family="binomial")
mod.rcs.1 = glm(y~x.rcs.1, family="binomial")
mod.rcs.365 = glm(y~x.rcs.365, family="binomial")
mod.cs.per.1 = glm(y~x.cs.per.1, family="binomial")
mod.cs.per.365 = glm(y~x.cs.per.365, family="binomial")

################### predicted probabilities, training ####

# estimated linear predictor with se
# for rcs.per
pred.rcs.per.train.1 = predict(mod.rcs.per.1, se.fit = TRUE) # on the scale of linear predictors
pred.rcs.per.train.365 = predict(mod.rcs.per.365, se.fit = TRUE)
# compare predictions
pred.rcs.per.train.1$fit - pred.rcs.per.train.365$fit
all.equal(pred.rcs.per.train.1$fit, pred.rcs.per.train.365$fit)

# for rcs
pred.rcs.train.1 = predict(mod.rcs.1, se.fit=TRUE)
pred.rcs.train.365 = predict(mod.rcs.365, se.fit=TRUE)
pred.rcs.train.1$fit - pred.rcs.train.365$fit
all.equal(pred.rcs.train.1$fit, pred.rcs.train.365$fit)

# for cs.per
pred.cs.per.train.1 = predict(mod.cs.per.1, se.fit=TRUE)
pred.cs.per.train.365 = predict(mod.cs.per.365, se.fit=TRUE)
pred.cs.per.train.1$fit - pred.cs.per.train.365$fit
all.equal(pred.cs.per.train.1$fit, pred.cs.per.train.365$fit)

##########################################333
# saving the linear predictors
lp.rcs.per.train = pred.rcs.per.train$fit
lp.rcs.train = pred.rcs.train$fit
lp.cs.per.train = pred.cs.per.train$fit

# deriving the estimated probabilities
p.rcs.per.train = 1/(1+exp(-lp.rcs.per.train))
p.rcs.train = 1/(1+exp(-lp.rcs.train))
p.cs.per.train = 1/(1+exp(-lp.cs.per.train))

#estimated probabilities predictor with se
p.pred.rcs.per.train = predict(mod.rcs.per, se.fit = TRUE, type="response")
p.pred.rcs.train = predict(mod.rcs, se.fit=TRUE, type="response")
p.pred.cs.per.train = predict(mod.cs.per, se.fit=TRUE, type="response")

