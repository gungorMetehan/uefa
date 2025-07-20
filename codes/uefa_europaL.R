# importing data
uefa_europaL <- read.csv("https://raw.githubusercontent.com/gungorMetehan/uefa/refs/heads/main/datasets/uefa_europaL.csv", header = T, sep = ",")

# packages
library(dplyr)
library(ggplot2)

# adding 'total_success' for ordering countries in y-axis
uefa_europaL <- uefa_europaL |>
  group_by(country) |>
  mutate(total_success = sum(highest, na.rm = TRUE)) |>
  ungroup() |>
  mutate(country = reorder(country, total_success))

# adding breaks for x-axis
uefa_europaL$season <- as.factor(uefa_europaL$season)
season_levels <- levels(uefa_europaL$season)
season_breaks <- season_levels[seq(1, length(season_levels), by = 5)]

# geom_rtile() function (stackoverflow)
install.packages("statebins")
library(statebins)

`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

GeomRtile <- ggproto("GeomRtile", 
                     statebins:::GeomRrect, 
                     
                     extra_params = c("na.rm"),
                     setup_data = function(data, params) {
                       data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                       data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                       
                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                 ymin = y - height / 2, ymax = y + height / 2, height = NULL
                       )
                     },
                     default_aes = aes(
                       fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                       alpha = NA, width = NA, height = NA
                     ),
                     required_aes = c("x", "y"),
                     non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                     draw_key = draw_key_polygon
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(5, "pt"),
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      radius = radius,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

# captions and the title
p_caption1 <- "\n\nData Source: uefa.com\n\n"
p_caption2 <- "\n\nVisualization by Metehan Güngör\n\n"
p_title <- "Most Successful Countries in UEFA Europa League (1955/58 - 2024/25)"

# plotting
ggplot(uefa_europaL, aes(x = season, y = country, fill = as.factor(highest))) +
  # not geom_tile() but geom_rtile() for rounded corners
  geom_rtile(color = "white", size = 1)+
  labs(x = "", y = "", caption = c(p_caption1, p_caption2), title = p_title) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical",
        legend.title = element_blank(),
        legend.margin = margin(grid::unit(0, "cm")),
        legend.text = element_text(size = 8),
        legend.key.height = grid::unit(0.75, "cm"),
        legend.key.width = grid::unit(0.3, "cm"),
        axis.text.x = element_text(size = 7.5, color = "grey30", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(vjust = 0, color = "grey30", hjust = 0, size = 10),
        axis.ticks = element_line(linewidth = 0.25),
        plot.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title = element_text(color = "grey30", hjust = 0, size = 24, face = "bold"),
        text = element_text(family = "AvantGarde"),
        plot.caption = element_text(family = "AvantGarde", size = 10, color = "grey30", vjust = 0, hjust = c(0, 1))
  ) +
  # five categories (na.value argument is not required)
  scale_fill_manual(
    values = c("0" = "grey95", "1" = "#006c8f", "2" = "#008eb0", "3" = "#00d6e8", "4" = "#00fbff"),
    labels = c("Pre-QF Exit", "Quarter-Final", "Semi-Final", "Final", "Champion"),
    na.value = "grey90"
  ) +
  # displaying one out of every five values (seasons)
  scale_x_discrete(breaks = season_breaks)



