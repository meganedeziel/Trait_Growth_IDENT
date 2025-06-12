growthpreds_nobepa<-function(timeper, model) {
  
  
  mycolors<-c("E - 0 to 3 years"="white", "E - 0 to 3 years - real"="olivedrab4", "D - 0 to 3 years"="white", 
              "D - 0 to 3 years - real"="maroon4", "E - 3 to 5 years"="white", "E - 3 to 5 years - real"="olivedrab4", 
              "D - 3 to 5 years"="white", "D - 3 to 5 years - real"="maroon4", "E - 3 to 6 years"="white", 
              "E - 3 to 6 years - real"="olivedrab4", "D - 3 to 6 years"="white", "D - 3 to 6 years - real"="maroon4", 
              "E - 5 to 7 years"="white", "E - 5 to 7 years - real"="olivedrab4", "D - 5 to 7 years"="white", 
              "D - 5 to 7 years - real"="maroon4", "E - 5 to 8 years"="white", "E - 5 to 8 years - real"="olivedrab4", 
              "D - 5 to 8 years"="white", "D - 5 to 8 years - real"="maroon4", "D - 6 to 9 years"="white", 
              "D - 6 to 9 years - real"="maroon4", "E - 6 to 9 years"="white", "E - 6 to 9 years - real"="olivedrab4")
  
  
  legend_title<-""
  
  #SLA 
  min_sla_d<-min(timeper[timeper$CE=="D", 17])
  max_sla_d<-max(timeper[timeper$CE=="D", 17])
  min_sla_e<-min(timeper[timeper$CE=="E", 17])
  max_sla_e<-max(timeper[timeper$CE=="E", 17])
  
  df_sla<-expand.grid(CE=c("E", "D"), time=c(unique(timeper$time)), SLA_s=seq(min(timeper$SLA_s), max(timeper$SLA_s), length.out=1000), LDMC_s=0, WD_s=0, SM_s=0)
  post_sla<-brms::posterior_epred(model, df_sla, re_formula=NA)
  df_sla$postmean<-apply(post_sla, 2, mean) #2 indicates column
  df_sla$q95<-apply(post_sla, 2, quantile, 0.95)
  df_sla$q05<-apply(post_sla, 2, quantile, 0.05)
  df_sla$SLA<-df_sla$SLA_s*sd(timeper$SLA)+mean(timeper$SLA)
  
  df_sla$real_d<-ifelse(df_sla$CE=="D" & df_sla$WD_s >= min_sla_d & df_sla$SLA_s <= max_sla_d, "real", NA)
  df_sla$unreal_d_before<-ifelse(df_sla$CE=="D" & df_sla$SLA_s < min_sla_d, "before", NA)
  df_sla$unreal_d_after<-ifelse(df_sla$CE=="D" & df_sla$SLA_s > max_sla_d, "after", NA)
  df_sla$real_e<-ifelse(df_sla$CE=="E" & df_sla$SLA_s >= min_sla_e & df_sla$SLA_s <= max_sla_e, "real", NA)
  df_sla<-df_sla %>%
    unite(., col="CEtime", CE, time, real_d, real_e, unreal_d_before, unreal_d_after, na.rm=TRUE, sep = " - ", remove=FALSE)
  df_sla$CEtime<-as.factor(df_sla$CEtime)
  
  df_sla<-subset(df_sla, df_sla$CEtime %in% c("E - 0 to 3 years - real", "E - 3 to 6 years - real", "E - 6 to 9 years - real", "D - 0 to 3 years - real", "D - 3 to 6 years - real", "D - 6 to 9 years - real"))
  
  plot_sla1<-ggplot(df_sla[df_sla$time==unique(timeper$time)[1],]) +
    ylim(0, 200) +
    geom_line(aes(x=SLA, y=postmean, color=CEtime)) +
    geom_ribbon(aes(x=SLA,
                    ymin=q05,
                    ymax=q95,
                    fill=CEtime),
                alpha=0.3) +
    labs(title="period 0-3", 
         x=expression("SLA (mm"^2*"/mg)"),
         y="Growth (cm/year)") +
    scale_color_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[1], " - real"), paste0("E - ", unique(timeper$time)[1], " - real")), labels = c("Deciduous", "Evergreen")) +
    scale_fill_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[1], " - real"), paste0("E - ", unique(timeper$time)[1], " - real")), labels = c("Deciduous", "Evergreen")) +
    theme_classic()
  
  plot_sla2<-ggplot(df_sla[df_sla$time==unique(timeper$time)[2],]) +
    ylim(0, 225) +
    geom_line(aes(x=SLA, y=postmean, color=CEtime)) +
    geom_ribbon(aes(x=SLA,
                    ymin=q05,
                    ymax=q95,
                    fill=CEtime),
                alpha=0.3) +
    labs(title="period 4-6", 
         x=expression("SLA (mm"^2*"/mg)"),
         y=element_blank()) +
    scale_color_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[2], " - real"), paste0("E - ", unique(timeper$time)[2], " - real")), labels = c("Deciduous", "Evergreen")) +
    scale_fill_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[2], " - real"), paste0("E - ", unique(timeper$time)[2], " - real")), labels = c("Deciduous", "Evergreen")) +
    theme_classic()
  
  plot_sla3<-ggplot(df_sla[df_sla$time==unique(timeper$time)[3],]) +
    ylim(0, 225) +
    geom_line(aes(x=SLA, y=postmean, color=CEtime)) +
    geom_ribbon(aes(x=SLA,
                    ymin=q05,
                    ymax=q95,
                    fill=CEtime),
                alpha=0.3) +
    labs(title="period 7-9", 
         x=expression("SLA (mm"^2*"/mg)"),
         y=element_blank()) +
    scale_color_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[3], " - real"), paste0("E - ", unique(timeper$time)[3], " - real")), labels = c("Deciduous", "Evergreen")) +
    scale_fill_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[3], " - real"), paste0("E - ", unique(timeper$time)[3], " - real")), labels = c("Deciduous", "Evergreen")) +
    theme_classic()
  
  sla<-ggarrange(plot_sla1, plot_sla2, plot_sla3, ncol=3, nrow=1, common.legend = TRUE)
  
 
  #LDMC
  min_ldmc_d<-min(timeper[timeper$CE=="D", 20])
  max_ldmc_d<-max(timeper[timeper$CE=="D", 20])
  min_ldmc_e<-min(timeper[timeper$CE=="E", 20])
  max_ldmc_e<-max(timeper[timeper$CE=="E", 20])
  
  df_ldmc<-expand.grid(CE=c("E", "D"), time=c(unique(timeper$time)), LDMC_s=seq(min(timeper$LDMC_s), max(timeper$LDMC_s), length.out=1000), SLA_s=0, WD_s=0, SM_s=0)
  post_ldmc<-brms::posterior_epred(model, df_ldmc, re_formula=NA)
  df_ldmc$postmean<-apply(post_ldmc, 2, mean) #2 indicates column
  df_ldmc$q95<-apply(post_ldmc, 2, quantile, 0.95)
  df_ldmc$q05<-apply(post_ldmc, 2, quantile, 0.05)
  df_ldmc$LDMC<-df_ldmc$LDMC_s*sd(timeper$LDMC)+mean(timeper$LDMC)
  
  df_ldmc$real_d<-ifelse(df_ldmc$CE=="D" & df_ldmc$LDMC_s >= min_ldmc_d & df_ldmc$LDMC_s <= max_ldmc_d, "real", NA)
  df_ldmc$unreal_d_before<-ifelse(df_ldmc$CE=="D" & df_ldmc$LDMC_s < min_ldmc_d, "before", NA)
  df_ldmc$unreal_d_after<-ifelse(df_ldmc$CE=="D" & df_ldmc$LDMC_s > max_ldmc_d, "after", NA)
  df_ldmc$real_e<-ifelse(df_ldmc$CE=="E" & df_ldmc$LDMC_s >= min_ldmc_e & df_ldmc$LDMC_s <= max_ldmc_e, "real", NA)
  df_ldmc<-df_ldmc %>%
    unite(., col="CEtime", CE, time, real_d, real_e, unreal_d_before, unreal_d_after, na.rm=TRUE, sep = " - ", remove=FALSE)
  df_ldmc$CEtime<-as.factor(df_ldmc$CEtime)
  
  df_ldmc<-subset(df_ldmc, df_ldmc$CEtime %in% c("E - 0 to 3 years - real", "E - 3 to 6 years - real", "E - 6 to 9 years - real", "D - 0 to 3 years - real", "D - 3 to 6 years - real", "D - 6 to 9 years - real"))
  
  plot_ldmc1<-ggplot(df_ldmc[df_ldmc$time==unique(timeper$time)[1],]) +
    ylim(0, 50) +
    geom_line(aes(x=LDMC, y=postmean, color=CEtime, show.legend=FALSE)) +
    geom_ribbon(aes(x=LDMC,
                    ymin=q05,
                    ymax=q95,
                    fill=CEtime, show.legend=FALSE),
                alpha=0.3) +
    labs(
      x=expression("LDMC (g/g)"),
      y="Growth (cm/year)") +
    scale_color_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[1], " - real"), paste0("E - ", unique(timeper$time)[1], " - real")), labels = c("Deciduous", "Evergreen")) +
    scale_fill_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[1], " - real"), paste0("E - ", unique(timeper$time)[1], " - real")), labels = c("Deciduous", "Evergreen")) +
    theme_classic()
  
  plot_ldmc2<-ggplot(df_ldmc[df_ldmc$time==unique(timeper$time)[2],]) +
    ylim(0, 225) +
    geom_line(aes(x=LDMC, y=postmean, color=CEtime, show.legend=FALSE)) +
    geom_ribbon(aes(x=LDMC,
                    ymin=q05,
                    ymax=q95,
                    fill=CEtime, show.legend=FALSE),
                alpha=0.3) +
    labs(
      x=expression("LDMC (g/g)"),
      y=element_blank()) +
    scale_color_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[2], " - real"), paste0("E - ", unique(timeper$time)[2], " - real")), labels = c("Deciduous", "Evergreen")) +
    scale_fill_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[2], " - real"), paste0("E - ", unique(timeper$time)[2], " - real")), labels = c("Deciduous", "Evergreen")) +
    theme_classic()
  
  plot_ldmc3<-ggplot(df_ldmc[df_ldmc$time==unique(timeper$time)[3],]) +
    ylim(0, 400) +
    geom_line(aes(x=LDMC, y=postmean, color=CEtime, show.legend=FALSE)) +
    geom_ribbon(aes(x=LDMC,
                    ymin=q05,
                    ymax=q95,
                    fill=CEtime, show.legend=FALSE),
                alpha=0.3) +
    labs(
      x=expression("LDMC (g/g)"),
      y=element_blank()) +
    scale_color_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[3], " - real"), paste0("E - ", unique(timeper$time)[3], " - real")), labels = c("Deciduous", "Evergreen")) +
    scale_fill_manual(legend_title, values=mycolors, breaks=c(paste0("D - ", unique(timeper$time)[3], " - real"), paste0("E - ", unique(timeper$time)[3], " - real")), labels = c("Deciduous", "Evergreen")) +
    theme_classic()
  
  
  ldmc<-ggarrange(plot_ldmc1, plot_ldmc2, plot_ldmc3, ncol=3, nrow=1, common.legend = TRUE)
  
  
  ggarrange(plot_sla1, plot_sla2, plot_sla3, plot_ldmc1, plot_ldmc2, plot_ldmc3, ncol=3, nrow=2, common.legend = TRUE)
  
}