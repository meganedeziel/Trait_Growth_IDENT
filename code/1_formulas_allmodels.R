### Below I present all the code used to compute the seven alternative models tested in the paper.
### as well as the model presented in Supplementary Materials investigating trait-growth relationships
### when the species Betula papyrifera is removed from the analysis

### These models were run seperately on Compute Canada clusters. The main model (most complex) takes about 5 days to compute.


### Package loading
library(here)
library(tidyverse)
library(brms)
### Data importation 
ident_3periods <- readRDS(here("data", "ident_3periods.RDS")) 
dir.create(here("results_models"), recursive = TRUE)

### Original model 

priors.main.model<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape"),
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

main.model.shape <- as.formula("shape ~ 1 + CE*time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)")

main.model.mu <-"growth ~ 1 + time * (CE + LDMC_s + SLA_s + SM_s + WD_s + LDMC_s:CE + SLA_s:CE + SM_s:CE + WD_s:CE) + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)" %>%
  as.formula()

main.model.form <- bf(main.model.mu, main.model.shape)

main.model<-brm(main.model.form,
                  data=ident_3periods,
                  prior=priors.main.model,
                  cores=4,
                  chains=4,
                  iter=10000, 
                  warmup=7500, 
                  family=Gamma(link="log"),
                  control=list(adapt_delta=0.99,
                               max_treedepth=12),
                  save_pars = save_pars(all=TRUE))

main.model.loo<-add_criterion(main.model, "loo", moment_match=TRUE)
saveRDS(main.model.loo, here("results_models", "main.model.loo.RDS"))

gc()


### Mod MB1

priors.mb1<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    #Linear predictor for location (mu), population-level effects:
    #Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    #set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    #set_prior("student_t(3, 0, 2.5)", class = "sd"),
    #set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape")
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    # set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

mb1.shape <- as.formula("shape ~ 1")

mb1.mu <-"growth ~ 1" |>
  as.formula()

mb1.form <- bf(mod.1.mu, mod.1.shape)


mb1<-brm(mod.1.form,
           data=ident_3periods,
           prior=priors.mb1,
           cores=4,
           chains=4,
           iter=10000, 
           warmup=7500, 
           family=Gamma(link="log"),
           control=list(adapt_delta=0.99,
                        max_treedepth=12),
           save_pars = save_pars(all=TRUE))



mb1.loo<-add_criterion(mb1, "loo", moment_match=TRUE)
saveRDS(mb1.loo, here("results_models", "mb1.loo.RDS"))

gc()


### Mod MB2

priors.mb2<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    #set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape")
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    #set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

mb2.shape <- as.formula("shape ~ 1 + (1 |p| ProjectName_T) + (1 |b| Block:ProjectName_T)")

mb2.mu <-"growth ~ 1 + (1 |p| ProjectName_T) + (1 |b| Block:ProjectName_T)" |>
  as.formula()

mb2.form <- bf(mb2.mu, mb2.shape)


mb2<-brm(mod.2.form,
           data=ident_3periods,
           prior=priors.mb2,
           cores=4,
           chains=4,
           iter=10000, 
           warmup=7500, 
           family=Gamma(link="log"),
           control=list(adapt_delta=0.99,
                        max_treedepth=12),
           save_pars = save_pars(all=TRUE))


mb2.loo<-add_criterion(mb2, "loo", moment_match=TRUE)
saveRDS(mb2.loo, here("results_models", "mb2.loo.RDS"))

gc()


### Mod MB3

priors.mb3<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape"),
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

mb3.shape <- as.formula("shape ~ 1 + time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)")

mb3.mu <-"growth ~ 1 + time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)" |>
  as.formula()

mb3.form <- bf(mb3.mu, mb3.shape)


mb3<-brm(mb3.form,
           data=ident_3periods,
           prior=priors.mb3,
           cores=4,
           chains=4,
           iter=10000, 
           warmup=7500, 
           family=Gamma(link="log"),
           control=list(adapt_delta=0.99,
                        max_treedepth=12),
           save_pars = save_pars(all=TRUE))


mb3.loo<-add_criterion(mb3, "loo", moment_match=TRUE)
saveRDS(mb3.loo, here("results_models", "mb3.loo.RDS"))

gc()


### Mod MH2 - no period

priors.mh2<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape"),
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

mh2.shape <- as.formula("shape ~ 1 + CE*time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)")

mh2.mu <-"growth ~ 1 + time * CE + CE * (LDMC_s + SLA_s + SM_s + WD_s) + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)" |> as.formula()

mh2.form <- bf(mh2.mu, mh2.shape)


mh2<-brm(mh2.form,
           data=ident_3periods,
           prior=priors.mh2,
           cores=4,
           chains=4,
           iter=10000, 
           warmup=7500, 
           family=Gamma(link="log"),
           control=list(adapt_delta=0.99,
                        max_treedepth=12),
           save_pars = save_pars(all=TRUE))


mh2.loo<-add_criterion(mh2, "loo", moment_match=TRUE)
saveRDS(mh2.loo, here("results_models", "mh2.loo.RDS"))

gc()

### Mod MH3 - no leaf habit

priors.mh3<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape"),
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

mh3.shape <- as.formula("shape ~ 1 + CE*time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)")

mh3.mu <-"growth ~ 1 + time * (CE + LDMC_s + SLA_s + SM_s + WD_s) + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)" |>
  as.formula()

mh3.form <- bf(mh3.mu, mh3.shape)

mh3<-brm(mh3.form,
         data=ident_3periods,
         prior=priors.mh3,
         cores=4,
         chains=4,
         iter=10000, 
         warmup=7500, 
         family=Gamma(link="log"),
         control=list(adapt_delta=0.99,
                      max_treedepth=12),
         save_pars = save_pars(all=TRUE))


mh3.loo<-add_criterion(mh3, "loo", moment_match=TRUE)
saveRDS(mh3.loo, here("results_models", "mh3.loo.RDS"))

gc()

### Mod MH4 - influence of traits only 

priors.mh4<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape"),
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

mh4.shape <- as.formula("shape ~ 1 + CE*time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)")

mh4.mu <-"growth ~ 1 + time*CE + LDMC_s + SLA_s + SM_s + WD_s + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)" |>
  as.formula()

mh4.form <- bf(mh4.mu, mh4.shape)


mh4<-brm(mh4.form,
           data=ident_3periods,
           prior=priors.mh4,
           cores=4,
           chains=4,
           iter=10000, 
           warmup=7500, 
           family=Gamma(link="log"),
           control=list(adapt_delta=0.99,
                        max_treedepth=12),
           save_pars = save_pars(all=TRUE))


mh4.loo<-add_criterion(mh4, "loo", moment_match=TRUE)
saveRDS(mh4.loo, here("results_models", "mh4.loo.RDS"))


### Main model without Betula papyrifera

ident_3periods <- ident_3periods[ident_3periods$CodeSp!="BEPA", ] 

priors.main.model<-
  c(
    # Linear predictor for location (mu), intercept: Weakly informative student-t prior centered on the (rounded) global mean of the response:
    set_prior(paste0("student_t(3, ",
                     round(log(median(ident_3periods$growth))),
                     ", 2.5)"), class = "Intercept"),
    # Linear predictor for location (mu), population-level effects:
    # Weakly informative student-t priors scaled to 2.5 * standard deviation of
    # the (centred) predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b"),
    # Group-level effects: Weakly informative half-t prior
    set_prior("student_t(3, 0, 2.5)", class = "sd"),
    set_prior("student_t(3, 0, 2.5)", class = "sd", dpar = "shape"),
    # Linear predictor for shape, intercept: Weakly informative student-t prior centered on 0
    set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "shape"),
    # Linear predictor for shape, population-level effects: Weakly informative
    # student-t priors scaled to 2.5 * standard deviation of the predictors:
    set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "shape") 
  )

main.model.shape <- as.formula("shape ~ 1 + CE*time + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)")

main.model.mu <-"growth ~ 1 + time * (CE + LDMC_s + SLA_s + SM_s + WD_s + LDMC_s:CE + SLA_s:CE + SM_s:CE + WD_s:CE) + (1 + time |p| ProjectName_T) + (1 + time |b| Block:ProjectName_T)" %>%
  as.formula()

main.model.form <- bf(main.model.mu, main.model.shape)

main.model<-brm(main.model.form,
                data=ident_3periods,
                prior=priors.main.model,
                cores=4,
                chains=4,
                iter=10000, 
                warmup=7500, 
                family=Gamma(link="log"),
                control=list(adapt_delta=0.99,
                             max_treedepth=12),
                save_pars = save_pars(all=TRUE))

main.model.loo<-add_criterion(main.model, "loo", moment_match=TRUE)
saveRDS(main.model.loo, here("results_models", "main.model.nobepa.loo.RDS"))

