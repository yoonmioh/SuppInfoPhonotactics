# Snippets of code used to generate the figures for the paper. The relevant data is assumed to already have been loaded from the Technical Supplement.

score_plot = ggplot(stimuliExp1, aes(x=score, y=mean.rating, color=type, shape=type, label=word)) + 
  geom_point(size=4*0.6, alpha=0.2) + 
  geom_smooth(aes(fill=type), method="lm", formula="y~x", alpha=0.6, size=2*0.6) +
  # geom_text(hjust=0.2, vjust=-0.7, check_overlap=TRUE, size=4) + 
  scale_shape_manual(name="Stimulus type",
                     values = c("Nonword" = 1, "Word" = 2),
                     guide = "none") +   
  scale_color_manual(name="Stimulus type",
                     values = c("Nonword" = "black", "Word" = "blue"),
                     guide = "none") +
  scale_fill_manual(name="Stimulus type",
                    values = c("Nonword" = "gray70", "Word" = "dodgerblue1"),
                    guide = "none") +  
  facet_grid( ~ type) + 
  labs(x= "Phonotactic score", y="Wordhood confidence\nMean rating (per stimulus)") + 
  scale_x_continuous(expand=expansion(mult=0.05)) +
  ylim(1, 5.1) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size=16*0.6, color="black"),
    axis.text = element_text(size=14*0.6, color="black"),
    legend.title = element_text(size=16*0.6, color="black"),
    legend.text = element_text(size=15*0.6, color="black"),
    strip.text = element_text(size=15*0.6, color="black")
  )

ggsave(plot=score_plot, "paper-figures/Fig2.tif", device="tiff", width=600*300/96*0.6, height=400*300/96*0.6, units="px", compression="lzw+p")


exp1d_mean_1 <- clm_plotdat(mExp1Dprime, c("type", "score", "dprime"), xlevels=list(score=25, dprime=dprime_cuts), type="mean") %>%
  mutate(
    type = fct_recode(type, "Nonword"="non", "Word"="real") %>% fct_relevel("Word"),
    dprime = factor(dprime, levels=rev(dprime_cuts), labels=c("High d-prime", "Low d-prime"))
  ) %>%
  ggplot(., aes(x=score, y=pred, color=type, fill=type)) +
  geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.2, color=NA) +
  geom_line(size=2*0.6) +
  facet_grid(dprime ~ ., switch="y") +
  xlab("Phonotactic score") +
  ylab("Predicted mean rating") + 
  ylim(1, 5) +
  scale_color_manual(
    labels = c("Word" = "Word ", "Nonword" = "Nonword"),
    values = c("Nonword" = "black", "Word" = "blue"), 
    guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    labels = c("Word" = "Word ", "Nonword" = "Nonword"),
    values = c("Nonword" = "black", "Word" = "blue"), 
    guide = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme(
    axis.title = element_text(size=16*0.6, color="black"),
    axis.text = element_text(size=14*0.6, color="black"),
    legend.title = element_blank(),
    legend.text = element_text(size=15*0.6, color="black"),
    legend.position = "bottom",
    strip.text = element_text(size=15*0.6, color="black"),
    plot.margin = margin(10,5,0,5, "pt")
  )

exp1d_mean_2 <- clm_plotdat(mExp1Dprime, c("macron", "dprime"), xlevels=list(dprime=dprime_cuts), type="mean") %>%
  mutate(
    type="Word & Nonword",
    macron = fct_recode(macron, "Yes"="TRUE", "No"="FALSE"),
    dprime = factor(dprime, levels=rev(dprime_cuts), labels=c("High d-prime", "Low d-prime"))
  ) %>%
  ggplot(., aes(x=macron, y=pred, color=type, fill=type)) +
  geom_point(size=3*0.6, position=position_dodge(width=0.2)) +
  geom_line(aes(x=as.integer(macron)), size=1*0.6, alpha=0.4, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=lci, ymax=uci), size=1*0.6, width=0.3, position=position_dodge(width=0.2)) +
  facet_grid(dprime ~ ., switch="y") +
  xlab("Presence of macron") +
  ylab("Predicted mean rating") + 
  ylim(1, 5) +
  scale_color_manual(values = c("Word & Nonword" = "purple")) +
  scale_fill_manual(values = c("Word & Nonword" = "purple")) +    
  theme_bw() + 
  theme(
    axis.title = element_text(size=16*0.6, color="black"),
    axis.text = element_text(size=14*0.6, color="black"),
    legend.title = element_blank(),
    legend.text = element_text(size=15*0.6, color="black"),
    legend.position = "bottom",
    strip.text = element_text(size=15*0.6, color="black"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y = element_blank(),
    strip.background.y = element_blank(),
    plot.margin = margin(10,5,0,5, "pt")
  )

exp1d_mean_3 <- clm_plotdat(mExp1Dprime, c("n.neighbors", "dprime"), xlevels=list(n.neighbors=25, dprime=dprime_cuts), type="mean") %>%
  mutate(
    type = "Word & Nonword",
    dprime = factor(dprime, levels=rev(dprime_cuts), labels=c("High d-prime", "Low d-prime"))
  ) %>%
  ggplot(., aes(x=n.neighbors, y=pred, color=type, fill=type)) +
  geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.2, color=NA) +
  geom_line(size=2*0.6) +
  facet_grid(dprime ~ ., switch="y") +
  xlab("Number of neighbors") +
  ylab("Predicted mean rating") + 
  ylim(1, 5) +
  scale_color_manual(values = c("Word & Nonword" = "purple")) +
  scale_fill_manual(values = c("Word & Nonword" = "purple")) +  
  theme_bw() + 
  theme(
    axis.title = element_text(size=16*0.6, color="black"),
    axis.text = element_text(size=14*0.6, color="black"),
    legend.title = element_blank(),
    legend.text = element_text(size=15*0.6, color="black"),
    legend.position = "bottom",
    strip.text = element_text(size=15*0.6, color="black"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y = element_blank(),
    strip.background.y = element_blank(),
    plot.margin = margin(10,5,0,5, "pt")
  )

effects_plots <- ggarrange(exp1d_mean_1, exp1d_mean_2, exp1d_mean_3, ncol=3,
          labels=c("             a.","b.","c."),
          label.args = list(gp=grid::gpar(font=4, cex=0.8), x=unit(1,"line"), hjust=0))

ggsave(plot=effects_plots, "paper-figures/Fig3.tif", device="tiff", width=900*300/96*0.6, height=600*300/96*0.6, units="px", compression="lzw+p")


familiarity_plot = ggplot(stimuliExp2, aes(x=familiarity.latent, y=prop.correct, label=word)) + 
  geom_point(size=3*0.6, alpha=0.1, shape=17, color="blue") + 
  geom_text(hjust=0.2, vjust=-0.7, check_overlap=TRUE, size=5*0.6, color="blue") + 
  labs(x= "Word familiarity (Exp. 1)", y="Proportion correct definitions") + 
  ylim(0, 1.05)+
  scale_x_continuous(expand=expansion(mult=0.2)) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size=16*0.6, color="black"),
    axis.text = element_text(size=14*0.6, color="black"),
    legend.title = element_text(size=16*0.6, color="black"),
    legend.text = element_text(size=15*0.6, color="black"),
    strip.text = element_text(size=15*0.6, color="black")
  )

ggsave(plot=familiarity_plot, "paper-figures/Fig4.tif", device="tiff", width=700*300/96*0.6, height=600*300/96*0.6, units="px", compression="lzw+p")
