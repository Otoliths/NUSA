# load package
library(ggtree)
library(ggplot2)
options(warn = -1)

# read tree
tr <- readRDS("input/eel_tree.rds")

# color map for posterior probabilities
clr_map <- c("<0.60" = "white", "0.60-0.75" = "gray75", "0.75-0.95" = "gray40", "0.95+" = "black")

clr_zone <- c(
  "Nu-Salween River" = "#E71F19",
  "Western Indian Ocean" = "lightgrey",
  "Eastern Indian Ocean" = "#28A9A1",
  "Northwest Pacific" = "#046586",
  "Western Pacific" = "#F4A016",
  "Eastern Pacific" = "#F6BBC6"
)

node_show <- c(seq(764, 778), 779, 780, 966, 967, 1137)


ggtree(tr, ladderize = T, size = 0.1, layout = "fan", open.angle = 10) +
  geom_tippoint(aes(colour = zone, subset = (nchar(label) < 20)), size = 0.5, shape = 18, na.rm = F) +
  geom_rootedge(rootedge = 0.006, size = 0.1) +
  geom_tiplab(aes(subset = (nchar(label) > 20)), parse = T, size = 0.5) +
  geom_treescale(x = 0.07, y = 0, width = 0.01, offset = 3, fontsize = 1.5, linesize = 0.1) +
  geom_nodepoint(aes(fill = posterior_bins, subset = (node %in% node_show)), shape = 21, alpha = 1, size = 0.6) +
  scale_fill_manual(na.translate = F, name = "Posterior node support", values = clr_map) +
  scale_color_manual("Region", values = clr_zone) +
  geom_cladelabel(
    node = 967, label = "A. bicolor", barsize = 1, color = "deepskyblue3",
    angle = 75, hjust = 0.1, offset = 0.0028, offset.text = 0.004, fontsize = 3
  ) +
  geom_cladelabel(
    node = 1137, label = "A. marmorata", barsize = 1, color = "green4",
    angle = -40, hjust = 0.1, offset = 0.027, offset.text = 0.004, fontsize = 3
  ) +
  geom_cladelabel(
    node = 780, label = "A. bengalensis", barsize = 1, color = "tomato2",
    angle = 28, hjust = 0.1, offset = 0.0075, offset.text = 0.004, fontsize = 3
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 3)),
    colour = guide_legend(override.aes = list(size = 3), order = 1)
  )


ggsave("output/Figure2b.pdf", width = 18, height = 12, units = "cm")
