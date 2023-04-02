# https://yulab-smu.top/treedata-book/chapter2.html#groupotu
library(tmap)
library(dplyr)
library(sf)
library(ggplot2)
library(ggnewscale)
library(patchwork)
library(grid)

data(World, metro, rivers, land)
current <- st_read("input/Major_Ocean_Currents-shp/Major_Ocean_Currents.shp")
st_transform(World, crs = "+proj=robin +lon_0=155 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")$geometry
dt <- readRDS("input/indo_pacific_eel.rds")

dt %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) -> dt

clr_zone <- c(
  "Nu-Salween River" = "#E71F19",
  "Western Indian Ocean" = "lightgrey",
  "Eastern Indian Ocean" = "#28A9A1",
  "Northwest Pacific" = "#046586",
  "Western Pacific" = "#F4A016",
  "Eastern Pacific" = "#F6BBC6"
)

current_dt <- current[-which(current$OBJECTID_1 %in% c(59, 9, 6, 60, 51, 53, 50, 17, 14, 22, 56)), ]
current_dt$TEMP <- factor(current_dt$TEMP, levels = c("warm", "cold"))

p1 <- ggplot() +
  geom_sf(data = World, fill = "#FFFFE0", colour = NA) +
  geom_sf(data = rivers, colour = "lightblue", size = 0.05) +
  geom_sf(data = current_dt, aes(colour = TEMP, fill = TEMP), size = 0.2) +
  # geom_sf_text(data = current_dt,aes(label = OBJECTID_1))+
  scale_color_manual("Major ocean current", values = c("#B22222", "#1E90FF")) +
  scale_fill_manual("Major ocean current", values = c("#B22222", "#1E90FF")) +
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = dt, aes(size = n, colour = zone), shape = 19) +
  scale_size("Sample Size", range = c(0.5, 4)) +
  scale_color_manual("Region", values = clr_zone) +
  scale_x_continuous(limits = c(-15000000, 5200000), breaks = c(0, 30, 60, 90, 120, 150, 180, 210)) +
  scale_y_continuous(limits = c(-4000000, 5000000), breaks = c(-30, 0, 30)) +
  coord_sf(crs = "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_bw() +
  facet_grid(group ~ .) +
  theme(
    axis.text = element_text(colour = "black", size = 5),
    axis.title = element_text(face = "bold", size = 6, colour = "black"),
    panel.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_line(linetype = 2, size = 0.3),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) +
  # zmargin2+
  guides(
    colour = guide_legend(order = 2, nrow = 2),
    size = guide_legend(order = 1)
  ) +
  xlab("Longitude (°)") +
  ylab("Latitude (°)")



zfit <- read.csv("input/mixstock_fit.csv")
zfit$from <- factor(zfit$from, levels = c("Western Indian Ocean", "Eastern Indian Ocean", "Northwest Pacific", "Western Pacific", "Eastern Pacific"))
theme_set(theme_bw())
zmargin2 <- theme(panel.spacing = unit(0, "lines"))
p2 <- ggplot(zfit, aes(x = factor(from), y = est, ymin = lwr, ymax = upr)) +
  geom_pointrange(size = 0.2, aes(colour = from), show.legend = F) +
  facet_grid(sp ~ .) +
  # zmargin2+
  ylim(0, 1) +
  xlab("Source populations") +
  ylab("Estimated source contributions") +
  scale_color_manual("Region", values = clr_zone) +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", size = 6, colour = "black"),
    axis.text = element_text(size = 5, colour = "black"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 6)
  )


p1 + p2 + plot_layout(guides = "collect", widths = c(2, 0.5)) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 5, colour = "black"),
    legend.title = element_text(face = "bold", size = 6, colour = "black"),
    legend.title.align = 0,
    legend.direction = "vertical"
  )


ggsave("output/Figure3.pdf", width = 20, height = 18, units = "cm", dpi = 300)
