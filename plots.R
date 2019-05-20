library(dplyr)
library(HaDeX)
library(ggplot2)
library(patchwork)

parse_path <- function(x) {
  path_raw <- strsplit(x, " ")[[1]] %>% 
    matrix(ncol = 3, byrow = TRUE) 
  
  path_raw <- path_raw[, -1]
  storage.mode(path_raw) <- "numeric"
  path_raw[, 2] <- path_raw[, 2]*-1
  colnames(path_raw) <- c("X", "Y")
  path_raw
}

isu1_overlap <- graphic_overlapping(read_hdx("/home/michal/Dropbox/HR-HDXMS/baza csv z przerobionych Dynamxow/Isu1.csv"))[["data"]]
isu1_overlap[["value"]] <- isu1_overlap[["value"]] + 33

isu1_raw <- "M 1040 7002.03125 L 1040 7000 L 3300 7000 L 3357.96875 340 L 3420 7000 L 3480 7000 L 3540 6960 L 3600 6960 L 3680 6560 L 3800 6560 L 3860 7002.03125 L 3920 6560 L 4180 6560 L 4240 7000 L 4360 7000 L 4423.984375 6380 L 4486.015625 6525 L 4548.984375 7002.03125 L 4612.03125 6525 L 4680 7000 L 4980 7000 L 5040 6360 L 5120 6360 L 5176.015625 7002.03125 L 5240 6940 L 5480 6940 L 5560 7000 L 6060 7000 L 6120 6960 L 6240 6960 L 6300 6400 L 6740 6400 L 6800 7000 L 7620 7000 L 7680 6800 L 7740 6800 L 7807.03125 7002.03125 L 7870 6916.015625 L 7932.96875 340 L 8000 7000 L 11320 7000"
isu1_dat <- parse_path(isu1_raw)

isu1_dat[, "X"] <- (isu1_dat[, "X"] - min(isu1_dat[, "X"]))/(max(isu1_dat[, "X"]) - min(isu1_dat[, "X"]))*162 + 1
isu1_dat[, "Y"] <- (isu1_dat[, "Y"] - min(isu1_dat[, "Y"]))/(max(isu1_dat[, "Y"]) - min(isu1_dat[, "Y"]))
isu1_plot_dat <- data.frame(isu1_dat)
isu1_plot_dat <- isu1_plot_dat[isu1_plot_dat[["X"]] > min(isu1_overlap[["value"]]), ]
isu1_plot_dat[["Z"]] <- ifelse(isu1_plot_dat[["X"]] < 39, "'N-terminal effect'", "HDX kinetics")

p1 <- ggplot(isu1_plot_dat, aes(x = X, y = Y, linetype = Z)) +
  geom_line() +
  scale_x_continuous("", breaks = seq(0, 180, by = 20), limits = c(0, 180)) +
  scale_y_continuous("Estimated kinetics of the H/D exchange") +
  scale_linetype_manual("", values = c("dashed", "solid")) +
  theme_bw(base_size = 13) +
  theme(legend.justification=c(0, 1), 
        legend.position=c(0.75, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank())

p2 <- ggplot(isu1_overlap, aes(x = value, y = ID, group = ID)) +
  geom_line() +
  scale_x_continuous("Position in sequence", breaks = seq(0, 180, by = 20), limits = c(0, 180)) +
  scale_y_continuous("") +
  theme_bw(base_size = 13) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


p1/p2 + plot_layout(ncol = 1, heights = c(3, 1))


