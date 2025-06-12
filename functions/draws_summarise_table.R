draws_summarise_table<-function(ident_3periods, model) {
  
  LDMC_sd<-sd(ident_3periods$LDMC)
  SLA_sd<-sd(ident_3periods$SLA)
  SM_sd<-sd(ident_3periods$SMlog)
  WD_sd<-sd(ident_3periods$WD)
  LDMC_m<-mean(ident_3periods$LDMC)
  SLA_m<-mean(ident_3periods$SLA)
  SM_m<-mean(ident_3periods$SMlog)
  WD_m<-mean(ident_3periods$WD)
  
  draws<-brms::as_draws_df(model,
                           variable=c("b_LDMC_s",
                                      "b_SLA_s",
                                      "b_SM_s",
                                      "b_WD_s",
                                      "b_CEE:LDMC_s",
                                      "b_CEE:SLA_s",
                                      "b_CEE:SM_s",
                                      "b_CEE:WD_s",
                                      "b_time3to6years:CEE",
                                      "b_time6to9years:CEE",
                                      "b_time3to6years:LDMC_s",
                                      "b_time6to9years:LDMC_s",
                                      "b_time3to6years:SLA_s",
                                      "b_time6to9years:SLA_s",
                                      "b_time3to6years:SM_s",
                                      "b_time6to9years:SM_s",
                                      "b_time3to6years:WD_s",
                                      "b_time6to9years:WD_s",
                                      "b_time3to6years:CEE:LDMC_s",
                                      "b_time6to9years:CEE:LDMC_s",
                                      "b_time3to6years:CEE:SLA_s",
                                      "b_time6to9years:CEE:SLA_s",
                                      "b_time3to6years:CEE:SM_s",
                                      "b_time6to9years:CEE:SM_s",
                                      "b_time3to6years:CEE:WD_s",
                                      "b_time6to9years:CEE:WD_s"
                           )) %>%
    mutate(LDMC_slope_03_d = exp((`b_LDMC_s`/LDMC_sd)),
           LDMC_slope_03_e = exp(((`b_LDMC_s` + `b_CEE:LDMC_s`)/LDMC_sd)),
           LDMC_slope_46_d = exp(((`b_LDMC_s` + `b_time3to6years:LDMC_s`)/LDMC_sd)),
           LDMC_slope_46_e = exp(((`b_LDMC_s` + `b_CEE:LDMC_s` + `b_time3to6years:LDMC_s` + `b_time3to6years:CEE:LDMC_s`)/LDMC_sd)),
           LDMC_slope_79_d = exp(((`b_LDMC_s` + `b_time6to9years:LDMC_s`)/LDMC_sd)),
           LDMC_slope_79_e = exp(((`b_LDMC_s` + `b_CEE:LDMC_s` + `b_time6to9years:LDMC_s` + `b_time6to9years:CEE:LDMC_s`)/LDMC_sd)),
           SLA_slope_03_d = exp((`b_SLA_s`/SLA_sd)),
           SLA_slope_03_e = exp(((`b_SLA_s` + `b_CEE:SLA_s`)/SLA_sd)),
           SLA_slope_46_d = exp(((`b_SLA_s` + `b_time3to6years:SLA_s`)/SLA_sd)),
           SLA_slope_46_e = exp(((`b_SLA_s` + `b_CEE:SLA_s` + `b_time3to6years:SLA_s` + `b_time3to6years:CEE:SLA_s`)/SLA_sd)),
           SLA_slope_79_d = exp(((`b_SLA_s` + `b_time6to9years:SLA_s`)/SLA_sd)),
           SLA_slope_79_e = exp(((`b_SLA_s` + `b_CEE:SLA_s` + `b_time6to9years:SLA_s` + `b_time6to9years:CEE:SLA_s`)/SLA_sd)),
           SM_slope_03_d = exp((`b_SM_s`/SM_sd)),
           SM_slope_03_e = exp(((`b_SM_s` + `b_CEE:SM_s`)/SM_sd)),
           SM_slope_46_d = exp(((`b_SM_s` + `b_time3to6years:SM_s`)/SM_sd)),
           SM_slope_46_e = exp(((`b_SM_s` + `b_CEE:SM_s` + `b_time3to6years:SM_s` + `b_time3to6years:CEE:SM_s`)/SM_sd)),
           SM_slope_79_d = exp(((`b_SM_s` + `b_time6to9years:SM_s`)/SM_sd)),
           SM_slope_79_e = exp(((`b_SM_s` + `b_CEE:SM_s` + `b_time6to9years:SM_s` + `b_time6to9years:CEE:SM_s`)/SM_sd)),
           WD_slope_03_d = exp((`b_WD_s`/WD_sd)),
           WD_slope_03_e = exp(((`b_WD_s` + `b_CEE:WD_s`)/WD_sd)),
           WD_slope_46_d = exp(((`b_WD_s` + `b_time3to6years:WD_s`)/WD_sd)),
           WD_slope_46_e = exp(((`b_WD_s` + `b_CEE:WD_s` + `b_time3to6years:WD_s` + `b_time3to6years:CEE:WD_s`)/WD_sd)),
           WD_slope_79_d = exp(((`b_WD_s` + `b_time6to9years:WD_s`)/WD_sd)),
           WD_slope_79_e = exp(((`b_WD_s` + `b_CEE:WD_s` + `b_time6to9years:WD_s` + `b_time6to9years:CEE:WD_s`)/WD_sd))
    ) %>%
    mutate(LDMC_slope_raw_03_d = `b_LDMC_s`,
           LDMC_slope_raw_03_e = `b_LDMC_s` + `b_CEE:LDMC_s`,
           LDMC_slope_raw_46_d = `b_LDMC_s` + `b_time3to6years:LDMC_s`,
           LDMC_slope_raw_46_e = `b_LDMC_s` + `b_CEE:LDMC_s` + `b_time3to6years:LDMC_s` + `b_time3to6years:CEE:LDMC_s`,
           LDMC_slope_raw_79_d = `b_LDMC_s` + `b_time6to9years:LDMC_s`,
           LDMC_slope_raw_79_e = `b_LDMC_s` + `b_CEE:LDMC_s` + `b_time6to9years:LDMC_s` + `b_time6to9years:CEE:LDMC_s`,
           SLA_slope_raw_03_d = `b_SLA_s`,
           SLA_slope_raw_03_e = `b_SLA_s` + `b_CEE:SLA_s`,
           SLA_slope_raw_46_d = `b_SLA_s` + `b_time3to6years:SLA_s`,
           SLA_slope_raw_46_e = `b_SLA_s` + `b_CEE:SLA_s` + `b_time3to6years:SLA_s` + `b_time3to6years:CEE:SLA_s`,
           SLA_slope_raw_79_d = `b_SLA_s` + `b_time6to9years:SLA_s`,
           SLA_slope_raw_79_e = `b_SLA_s` + `b_CEE:SLA_s` + `b_time6to9years:SLA_s` + `b_time6to9years:CEE:SLA_s`,
           SM_slope_raw_03_d = `b_SM_s`,
           SM_slope_raw_03_e = `b_SM_s` + `b_CEE:SM_s`,
           SM_slope_raw_46_d = `b_SM_s` + `b_time3to6years:SM_s`,
           SM_slope_raw_46_e = `b_SM_s` + `b_CEE:SM_s` + `b_time3to6years:SM_s` + `b_time3to6years:CEE:SM_s`,
           SM_slope_raw_79_d = `b_SM_s` + `b_time6to9years:SM_s`,
           SM_slope_raw_79_e = `b_SM_s` + `b_CEE:SM_s` + `b_time6to9years:SM_s` + `b_time6to9years:CEE:SM_s`,
           WD_slope_raw_03_d = `b_WD_s`/WD_sd,
           WD_slope_raw_03_e = `b_WD_s` + `b_CEE:WD_s`,
           WD_slope_raw_46_d = `b_WD_s` + `b_time3to6years:WD_s`,
           WD_slope_raw_46_e = `b_WD_s` + `b_CEE:WD_s` + `b_time3to6years:WD_s` + `b_time3to6years:CEE:WD_s`,
           WD_slope_raw_79_d = `b_WD_s` + `b_time6to9years:WD_s`,
           WD_slope_raw_79_e = `b_WD_s` + `b_CEE:WD_s` + `b_time6to9years:WD_s` + `b_time6to9years:CEE:WD_s`
    ) %>%
    mutate(LDMC_slope_diff_03_ed=LDMC_slope_03_e-LDMC_slope_03_d,
           LDMC_slope_diff_36_ed=LDMC_slope_46_e-LDMC_slope_46_d,
           LDMC_slope_diff_79_ed=LDMC_slope_79_e-LDMC_slope_79_d,
           LDMC_slope_diff_e_0346=LDMC_slope_46_e-LDMC_slope_03_e,
           LDMC_slope_diff_e_0379=LDMC_slope_79_e-LDMC_slope_03_e,
           LDMC_slope_diff_e_4679=LDMC_slope_79_e-LDMC_slope_46_e,
           LDMC_slope_diff_d_0346=LDMC_slope_46_d-LDMC_slope_03_d,
           LDMC_slope_diff_d_0379=LDMC_slope_79_d-LDMC_slope_03_d,
           LDMC_slope_diff_d_4679=LDMC_slope_79_d-LDMC_slope_46_d,
           SLA_slope_diff_03_ed=SLA_slope_03_e-SLA_slope_03_d,
           SLA_slope_diff_46_ed=SLA_slope_46_e-SLA_slope_46_d,
           SLA_slope_diff_79_ed=SLA_slope_79_e-SLA_slope_79_d,
           SLA_slope_diff_e_0346=SLA_slope_46_e-SLA_slope_03_e,
           SLA_slope_diff_e_0379=SLA_slope_79_e-SLA_slope_03_e,
           SLA_slope_diff_e_4679=SLA_slope_79_e-SLA_slope_46_e,
           SLA_slope_diff_d_0346=SLA_slope_46_d-SLA_slope_03_d,
           SLA_slope_diff_d_0379=SLA_slope_79_d-SLA_slope_03_d,
           SLA_slope_diff_d_4679=SLA_slope_79_d-SLA_slope_46_d,
           SM_slope_diff_03_ed=SM_slope_03_e-SM_slope_03_d,
           SM_slope_diff_46_ed=SM_slope_46_e-SM_slope_46_d,
           SM_slope_diff_79_ed=SM_slope_79_e-SM_slope_79_d,
           SM_slope_diff_e_0346=SM_slope_46_e-SM_slope_03_e,
           SM_slope_diff_e_0379=SM_slope_79_e-SM_slope_03_e,
           SM_slope_diff_e_4679=SM_slope_79_e-SM_slope_46_e,
           SM_slope_diff_d_0346=SM_slope_46_d-SM_slope_03_d,
           SM_slope_diff_d_0379=SM_slope_79_d-SM_slope_03_d,
           SM_slope_diff_d_4679=SM_slope_79_d-SM_slope_46_d,
           WD_slope_diff_03_ed=WD_slope_03_e-WD_slope_03_d,
           WD_slope_diff_46_ed=WD_slope_46_e-WD_slope_46_d,
           WD_slope_diff_79_ed=WD_slope_79_e-WD_slope_79_d,
           WD_slope_diff_e_0346=WD_slope_46_e-WD_slope_03_e,
           WD_slope_diff_e_0379=WD_slope_79_e-WD_slope_03_e,
           WD_slope_diff_e_4679=WD_slope_79_e-WD_slope_46_e,
           WD_slope_diff_d_0346=WD_slope_46_d-WD_slope_03_d,
           WD_slope_diff_d_0379=WD_slope_79_d-WD_slope_03_d,
           WD_slope_diff_d_4679=WD_slope_79_d-WD_slope_46_d
    )
  draws_summarise<-summarise_draws(draws, mean, median, ~quantile(.x, probs=c(0.05, 0.95)))
  draws_summarise$round_mean<-round(draws_summarise$mean, 4)
  draws_summarise$round_5<-round(draws_summarise$`5%`, 4)
  draws_summarise$round_95<-round(draws_summarise$`95%`, 4)
  
  draws_summarise
}
