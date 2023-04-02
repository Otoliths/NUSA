# load package
library(ggplot2)
library(ggpubr)

# read MMnet_result
data <- read.csv("input/MMnet_result.csv")

Confidence_mean1 <- (0.9998 + 0.9999 + 0.9998 + 0.9991 + 0.9998 + 0.9998 + 1 + 0.9996 + 0.9999 + 0.9998) / 10
a <- ggplot(
  data = data[which(data$SP == "Anguilla bengalensis"), ],
  aes(x = Group, y = Contribution, fill = Group)
) +
  geom_bar(stat = "summary", fun = mean, width = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", colour = "black", width = 0.25, position = position_dodge(.9)) +
  scale_fill_manual(values = c("#FFCE4EFF", "#3D98D3FF"), guide = "none") +
  geom_jitter(size = 3, alpha = 0.5, shape = 21, width = 0.15) +
  scale_x_discrete(expand = c(0.25, 0.15)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", size = 10, colour = "black"),
    axis.text = element_text(size = 8, colour = "black")
  ) +
  annotate("text", x = 2, y = 0.98, label = "Confidence = 99.98%") +
  xlab("")


Confidence_mean2 <- (0.9999 + 0.9999 + 0.9999 + 1 + 0.9999 + 0.9988 + 1 + 0.9997 + 1 + 1) / 10
b <- ggplot(
  data = data[which(data$SP == "Anguilla bicolor"), ],
  aes(x = Group, y = Contribution, fill = Group)
) +
  geom_bar(stat = "summary", fun = mean, width = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", colour = "black", width = 0.25, position = position_dodge(.9)) +
  scale_fill_manual(values = c("#FFCE4EFF", "#3D98D3FF"), guide = "none") +
  geom_jitter(size = 3, alpha = 0.5, shape = 21, width = 0.15) +
  scale_x_discrete(expand = c(0.25, 0.15)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", size = 10, colour = "black"),
    axis.text = element_text(size = 8, colour = "black")
  ) +
  annotate("text", x = 2, y = 0.98, label = "Confidence = 99.98%") +
  xlab("")


Confidence_mean3 <- (0.9997 + 0.9978 + 0.9975 + 0.9999 + 0.9961 + 0.9998 + 0.9999 + 0.9995 + 0.9999 + 0.9999) / 10
c <- ggplot(
  data = data[which(data$SP == "Anguilla marmorata"), ],
  aes(x = Group, y = Contribution, fill = Group)
) +
  geom_bar(stat = "summary", fun = mean, width = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", colour = "black", width = 0.25, position = position_dodge(.9)) +
  scale_fill_manual(values = c("#FFCE4EFF", "#3D98D3FF"), guide = "none") +
  geom_jitter(size = 3, alpha = 0.5, shape = 21, width = 0.15) +
  scale_x_discrete(expand = c(0.25, 0.15)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold", size = 10, colour = "black"),
    axis.text = element_text(size = 8, colour = "black")
  ) +
  annotate("text", x = 2, y = 0.98, label = "Confidence = 99.90%") +
  xlab("")

library(cowplot)
plot_grid(a, b, c, labels = c("", "", ""), label_size = 12, nrow = 1)


Confidence <- data.frame(
  Confidence = c("99.98%", "99.98%", "99.90%"),
  SP = c("Anguilla bengalensis", "Anguilla bicolor", "Anguilla marmorata"),
  Group = rep("Image", 3),
  y = rep(0.98, 3)
)


ggplot(
  data = data,
  aes(x = Group, y = Contribution, fill = Group)
) +
  geom_bar(stat = "summary", fun = mean, width = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", colour = "black", width = 0.25, position = position_dodge(.9)) +
  scale_fill_manual(values = c("#FFCE4EFF", "#3D98D3FF"), guide = "none") +
  geom_text(data = Confidence, aes(y = y, label = paste("Confidence =", Confidence)), size = 3, hjust = 0.7, vjust = 1) +
  geom_jitter(size = 2.5, alpha = 0.5, shape = 21, width = 0.15) +
  scale_x_discrete(expand = c(0.25, 0.15)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  theme_bw() +
  facet_grid(. ~ SP) +
  theme(
    axis.title = element_text(face = "bold", size = 10, colour = "black"),
    axis.text = element_text(size = 8, colour = "black"),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold.italic")
  ) +
  xlab("Category")


ggsave("Output/Figure2c.pdf", width = 19, height = 8, units = "cm", dpi = 300)
