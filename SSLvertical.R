Depth <- df$Depth

# ?ӵ?2?п?ʼ?? Sv(dB)
X <- C[-1, , drop = FALSE]

# ת????ֵ???????ַ?/??ֵ???ɣ?????ת?ı? NA??
SV <- apply(X, 2, function(z) suppressWarnings(as.numeric(z)))

# ????????ֵ 10^(Sv/10)??NA ???? NA
LIN <- 10^(SV/10)

Depth <- suppressWarnings(as.numeric(Depth))

LIN_mat <- as.matrix(LIN)

n_i <- rowSums(!is.na(LIN_mat))
LIN_mean <- rowMeans(LIN_mat, na.rm = TRUE)

LIN_sd <- apply(LIN_mat, 1, sd, na.rm = TRUE)
LIN_se <- LIN_sd / sqrt(n_i)

tcrit <- rep(NA_real_, length(n_i))
ok <- n_i >= 2
tcrit[ok] <- qt(0.975, df = n_i[ok] - 1)

LIN_lower <- LIN_mean - tcrit * LIN_se
LIN_upper <- LIN_mean + tcrit * LIN_se

Sv_mean  <- 10 * log10(LIN_mean)
Sv_lower <- 10 * log10(LIN_lower)
Sv_upper <- 10 * log10(LIN_upper)
minus <- "\u2212"
m <- min(length(Depth), length(Sv_mean))
df_plot <- data.frame(
  Depth   = Depth[1:m],
  Sv_mean = Sv_mean[1:m],
  Sv_lower = Sv_lower[1:m],
  Sv_upper = Sv_upper[1:m]
)

# ֻ?? 800?C990 m ?? movmedian ƽ???????? 10
idx <- which(df_plot$Depth >= 800 & df_plot$Depth <= 990)

# Ϊ?˱?֤?????ǰ?????˳?򻬶????????���?????ڸ??????ڰ? Depth ??????ƽ??
ord_local <- order(df_plot$Depth[idx])
idx_ord <- idx[ord_local]

movmedian_10 <- function(x) {
  zoo::rollapply(
    x,
    width = 10,
    FUN = function(v) median(v, na.rm = TRUE),
    align = "center",
    partial = TRUE,   # ?˵??ý?С???ڣ????? Matlab movmedian ?ı?Ե???���
    fill = NA
  )
}

# ?? Sv ?????????????ֱ???ƽ?????? 800?C990 m??
df_plot$Sv_mean[idx_ord]  <- movmedian_10(df_plot$Sv_mean[idx_ord])
df_plot$Sv_lower[idx_ord] <- movmedian_10(df_plot$Sv_lower[idx_ord])
df_plot$Sv_upper[idx_ord] <- movmedian_10(df_plot$Sv_upper[idx_ord])

# -------- 4) ??ͼ??X=Sv(dB)??Y=Depth???????????? --------
p <- ggplot(df_plot, aes(x = Depth, y = Sv_mean)) +
  geom_ribbon(aes(ymin = Sv_lower, ymax = Sv_upper),
              fill = "#DC7369", alpha = 0.25) +
  geom_line(color = "#DC7369", linewidth = 1.0) +
  coord_flip() +  # ??ת????x=Sv_mean, y=Depth
  labs(x = "LIN", y = "Depth") +  # ??һ?к????ᱻ?????? labs ????
  theme_classic(base_family = "serif", base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = margin(t = 6), color = "black"),
    axis.text.y = element_text(margin = margin(r = 6), color = "black")
  )

p <- p + scale_y_continuous(
  position = "right",
  limits = c(-95, -60),
  breaks = seq(-95, -60, by = 5),
  labels = function(x) {
    lab <- as.character(x)
    gsub("^-", minus, lab)
  }
)

p <- p + scale_x_reverse(
  limits = c(1000, 0),
  breaks = seq(0, 1000, by = 100),
  expand = expansion(mult = c(0, 0))
)

# =================== ?ؼ??Ķ????????Ƹ?Ϊ Sv(dB) ===================
p <- p + labs(
  x = "Depth (m)",       # flip ??ͼ??????��??ԭ x
  y = "Sv (dB)"      # flip ??ͼ?Ϻ???��??ԭ y
)

print(p)

# ================