 <- SVdata[[6]]
MVBS <- MVBS[is.finite(MVBS)]  # ȥ?? NA/NaN/Inf

df <- data.frame(MVBS = MVBS)

# xtick labels?????????ġ????š? U+2212?????? Matlab ?? ???90??
minus <- "\u2212"

# x ?᣺??Χ -85 ?? -60
x_breaks <- seq(-85, -60, by = 5)
x_labels <- paste0(minus, abs(x_breaks))

# y ?᣺????��λС?????? 0 ??ʾΪ 0??????С????
y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.2f", x))

p <- ggplot(df, aes(x = MVBS)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2,
                 fill = "#f57c6e", alpha = 1, color = "#f57c6e", linewidth = 0.2) +
  geom_density(color = "#297270", linewidth = 1.0, adjust = 1) +
  scale_x_continuous(limits = c(-85, -60),
                     breaks = seq(-85, -60, 5),
                     labels = paste0("\u2212", abs(seq(-85, -60, 5)))) +
  scale_y_continuous(
    limits = c(0, NA),                 # y ???? 0 ??ʼ
    expand = expansion(mult = c(0, 0.05)),  # ?¶˲????ף??϶???һ???㣨?ɸ? 0??
    labels = y_lab_fun
  ) +
  labs(x = "MVBS (dB)", y = "Probability density") +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))
p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),           # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)), # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )
p <- p + theme(
  axis.text.x = element_text(color = "black"),   # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")   # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣- SSLdata[[7]]
MVBS_1 <- MVBS_1[is.finite(MVBS_1)]  # ȥ?? NA/NaN/Inf

df <- data.frame(MVBS_1 = MVBS_1)

# xtick labels?????????ġ????š? U+2212?????? Matlab ?? ???90??
minus <- "\u2212"

# x ?᣺
x_breaks <- seq(-85, -60, by = 5)
x_labels <- paste0(minus, abs(x_breaks))

# y ?᣺????��λС?????? 0 ??ʾΪ 0??????С????
y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.2f", x))

p <- ggplot(df, aes(x = MVBS_1)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2,
                 fill = "#f57c6e", alpha = 1, color = "#f57c6e") +
  geom_density(color = "#297270", linewidth = 1.0, adjust = 1) +
  scale_x_continuous(limits = c(-85, -60),
                     breaks = seq(-85, -60, 5),
                     labels = paste0("\u2212", abs(seq(-85, -60, 5)))) +
  scale_y_continuous(
    limits = c(0, NA),                 # y ???? 0 ??ʼ
    expand = expansion(mult = c(0, 0.05)),  # ?¶˲????ף??϶???һ???㣨?ɸ? 0??
    labels = y_lab_fun
  ) +
  labs(x = expression(MVBS[1]~"(dB)"), y = "Probability density") +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))
p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),           # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)), # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )
p <- p + theme(
  axis.text.x = element_text(color = "black"),   # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")    # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣??ΪSSLdata[[4]]
MVBS_all <- MVBS_all[is.finite(MVBS_all)]  # ȥ?? NA/NaN/Inf

df <- data.frame(MVBS_all = MVBS_all)

# xtick labels?????????ġ????š? U+2212?????? Matlab ?? ???90??
minus <- "\u2212"

# x ?᣺
x_breaks <- seq(-85, -60, by = 5)
x_labels <- paste0(minus, abs(x_breaks))

# y ?᣺????��λС?????? 0 ??ʾΪ 0??????С????
y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.2f", x))

p <- ggplot(df, aes(x = MVBS_all)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2,
                 fill = "#f57c6e", alpha = 1, color = "#f57c6e") +
  geom_density(color = "#297270", linewidth = 1.0, adjust = 1) +
  scale_x_continuous(limits = c(-85, -60),
                     breaks = seq(-85, -60, 5),
                     labels = paste0("\u2212", abs(seq(-85, -60, 5)))) +
  scale_y_continuous(
    limits = c(0, NA),                 # y ???? 0 ??ʼ
    expand = expansion(mult = c(0, 0.05)),  # ?¶˲????ף??϶???һ???㣨?ɸ? 0??
    labels = y_lab_fun
  ) +
  labs(x = expression(MVBS[all]~"(dB)"), y = "Probability density") +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))
p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),           # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)), # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )
p <- p + theme(
  axis.text.x = element_text(color = "black"),   # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")    # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣??ΪͼƬ]
N <- N[is.finite(N) & N >= 0]   # ȥ NA/NaN/Inf/??ֵ
N <- round(N)                   # ???????뵽????

# ???????䣺???????߽? -> ÿ??????һ????
lo <- floor(min(N))
hi <- ceiling(max(N))
edges <- seq(lo - 0.5, hi + 0.5, by = 1)
centers <- lo:hi

# ???? PMF???? Matlab histcounts(...,'Normalization','probability') ?ȼۣ?
counts <- as.numeric(table(factor(N, levels = centers))) / length(N)

# ??ÿ??????һ?????飨?? centers ???????ӣ?
df <- data.frame(centers = centers, counts = counts)
df$centers_f <- factor(df$centers)

# ????һ????ɫ????Ҳ???Ի????Լ???Ҫ????ɫ??��??
cols <- c("#f57c6e", "#f57c6e", "#f57c6e", "#f57c6e", "#f57c6e")  # ʾ??????��Ҫƥ??

y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.2f", x))

p <- ggplot(df, aes(x = centers, y = counts)) +
  geom_col(aes(fill = centers_f),
           width = 0.8,
           alpha = 1,
           color = NA, linewidth = 1) +
  scale_x_continuous(breaks = centers) +
  scale_fill_manual(values = cols, guide = "none") +  # ????ʾͼ??
  labs(x = "N", y = "Probability") +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))

p <- p + scale_y_continuous(
  limits = c(0, NA),                      # y ???? 0 ??ʼ
  expand = expansion(mult = c(0, 0.05)),  # ?¶˲????ף??϶??? 0.05
  breaks = seq(0, 1, by = 0.1)            # Y?????? 0.1?????޲?ȷ?????? 0~1??
)

p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),            # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)),  # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )

p <- p + theme(
  axis.text.x = element_text(color = "black"),   # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")    # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣??ΪͼƬ
# Ŀ???
NASC_all <- NASC_all[is.finite(NASC_all) & SSLdata[[5]] >= 0]  # ?Ǹ? + ȥ NA/NaN/Inf

df <- data.frame(NASC_all = NASC_all)

# ???? ֱ??ͼ???䣺Freedman?CDiaconis ?????? BinWidth ???? 
n <- length(NASC_all)
iqrX <- IQR(NASC_all, na.rm = TRUE)
bw_hist <- 2 * iqrX / (max(n, 1)^(1/3))     # FD ????
if (!is.finite(bw_hist) || bw_hist <= 0) {
  bw_hist <- max(1e-3, 0.02 * diff(range(NASC_all, na.rm = TRUE)))  # ??·????Χ?? 2%
}

# KDE???Ǹ???�� ?? ??????֧?? + ?߽練?䣨reflection??
# ?÷??䷨???? 0 ?????񣬹��ƺ?ֻȡ x>=0???????ܶȳ? 2????֤??????????Ϊ 1??
d0 <- density(
  c(NASC_all, -NASC_all),
  bw = "nrd0",
  from = 0,
  to = max(NASC_all, na.rm = TRUE),
  na.rm = TRUE
)
kde_df <- data.frame(x = d0$x, y = 2 * d0$y)

# y ?᣺????0λС?????? 0 ??ʾΪ 0??????С????
y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.0f", x))

p <- ggplot(df, aes(x = NASC_all)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = bw_hist,
                 fill = "#f57c6e", alpha = 1, color = "#f57c6e") +
  geom_line(data = kde_df, aes(x = x, y = y),
            color = "#297270", linewidth = 1.0) +
  scale_x_continuous(limits = c(0, 8000),                        # x ???? 0 ??ʼ
                     expand = expansion(mult = c(0, 0.05)),    # ?¶˲????ף??϶??? 0.05
                     ) +   # xlim([0, 8000])
  scale_y_continuous(
    limits = c(0, NA),                        # y ???? 0 ??ʼ
    expand = expansion(mult = c(0, 0.05)),    # ?¶˲????ף??϶??? 0.05
    labels = label_number(scale = 1e4, accuracy = 1)
  ) +
  labs(x = expression(NASC[all]~"(m"^2*"/nmi"^2*")"),
       y = expression(paste("Probability density (", "\u00D7", 10^{-4}, ")"))) +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))

p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),              # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)), # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )

p <- p + theme(
  axis.text.x = element_text(color = "black"),  # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")   # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣??ΪͼƬ
# Ŀ???ļ??????nDepth_1 <- MeanDepth_1[is.finite(MeanDepth_1) & SSLdata[[12]] >= 0]  # ?Ǹ? + ȥ NA/NaN/Inf

df <- data.frame(MeanDepth_1 = MeanDepth_1)

# ???? ֱ??ͼ???䣺Freedman?CDiaconis ?????? BinWidth ???? 
n <- length(MeanDepth_1)
iqrX <- IQR(MeanDepth_1, na.rm = TRUE)
bw_hist <- 2 * iqrX / (max(n, 1)^(1/3))      # FD ????
if (!is.finite(bw_hist) || bw_hist <= 0) {
  bw_hist <- max(1e-3, 0.02 * diff(range(MeanDepth_1, na.rm = TRUE)))  # ??·????Χ?? 2%
}

# KDE???Ǹ???�� ?? ??????֧?? + ?߽練?䣨reflection??
d0 <- density(
  c(MeanDepth_1, -MeanDepth_1),
  bw = "nrd0",
  from = 0,
  to = max(MeanDepth_1, na.rm = TRUE),
  na.rm = TRUE
)
kde_df <- data.frame(x = d0$x, y = 2 * d0$y)

# y ?᣺????��λС?????? 0 ??ʾΪ 0??????С????
y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.3f", x))

p <- ggplot(df, aes(x = MeanDepth_1)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = bw_hist,
                 fill = "#f57c6e", alpha = 1, color = "#f57c6e") +
  geom_line(data = kde_df, aes(x = x, y = y),
            color = "#297270", linewidth = 1.0) +
  scale_x_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, by = 100),
                     expand = expansion(mult = c(0, 0.05))) +   # xlim([0, 600])
  scale_y_continuous(
    limits = c(0, NA),                       # y ???? 0 ??ʼ
    expand = expansion(mult = c(0, 0.05)),   # ?¶˲????ף??϶???һ???㣨?ɸ? 0??
    labels = y_lab_fun
  ) +
  labs(x = expression(MeanDepth[1]~"(m)"), y = "Probability density") +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))

p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),           # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)), # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )

p <- p + theme(
  axis.text.x = element_text(color = "black"),   # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")    # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣??ΪͼƬ
# Ŀ???ļ???????·??OK?h_1[is.finite(Width_1) & SSLdata[[11]] >= 0]  # ?Ǹ? + ȥ NA/NaN/Inf

df <- data.frame(Width_1 = Width_1)

# ???? ֱ??ͼ???䣺Freedman?CDiaconis ?????? BinWidth ???? 
n <- length(Width_1)
iqrX <- IQR(Width_1, na.rm = TRUE)
bw_hist <- 2 * iqrX / (max(n, 1)^(1/3))      # FD ????
if (!is.finite(bw_hist) || bw_hist <= 0) {
  bw_hist <- max(1e-3, 0.02 * diff(range(Width_1, na.rm = TRUE)))  # ??·????Χ?? 2%
}

# KDE???Ǹ???�� ?? ??????֧?? + ?߽練?䣨reflection??
d0 <- density(
  c(Width_1, -Width_1),
  bw = "nrd0",
  from = 0,
  to = max(Width_1, na.rm = TRUE),
  na.rm = TRUE
)
kde_df <- data.frame(x = d0$x, y = 2 * d0$y)

# y ?᣺????��λС?????? 0 ??ʾΪ 0??????С????
y_lab_fun <- function(x) ifelse(abs(x) < 1e-12, "0", sprintf("%.3f", x))

p <- ggplot(df, aes(x = Width_1)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = bw_hist,
                 fill = "#f57c6e", alpha = 1, color = "#f57c6e") +
  geom_line(data = kde_df, aes(x = x, y = y),
            color = "#297270", linewidth = 1.0) +
  scale_x_continuous(limits = c(0, 600),
                     breaks = seq(0, 600, by = 100),
                     expand = expansion(mult = c(0, 0.05))) +   # x??????100
  scale_y_continuous(
    limits = c(0, NA),                       # y ???? 0 ??ʼ
    expand = expansion(mult = c(0, 0.05)),   # ?¶˲????ף??϶???һ???㣨?ɸ? 0??
    labels = y_lab_fun
  ) +
  labs(x = expression(Width[1]~"(m)"), y = "Probability density") +
  theme_classic(base_family = "serif", base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(linewidth = 0.8),
        axis.ticks = element_line(linewidth = 0.8))

p <- p +
  theme(
    axis.ticks.length = unit(-0.15, "cm"),              # ??ֵ = ?̶ȳ???
    axis.text.x = element_text(margin = margin(t = 6)), # ??ֹ???ֺͿ̶??ߴ???
    axis.text.y = element_text(margin = margin(r = 6))
  )

p <- p + theme(
  axis.text.x = element_text(color = "black"),  # x???̶ȱ?ǩ??ɫ
  axis.text.y = element_text(color = "black")   # y???̶ȱ?ǩ??ɫ
)

print(p)

# ???豣??ΪͼƬ
# Ŀ???ļ???????·??OK??
out_