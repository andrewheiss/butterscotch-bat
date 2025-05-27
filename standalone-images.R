## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: libraries-data
library(tidyverse)
library(tidygraph)
library(ggraph)

update_geom_defaults("text", list(family = "Inter"))
update_geom_defaults("label", list(family = "Inter"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-agency-response-tree
#| fig-cap: Possible donor- and recipient-country responses to human rights violations
#| fig-alt: |
#|   A flowchart showing possible reactions to human rights oppression.
#|   Recipient governments violate human rights. Donor governments can respond by
#|   doing nothing, threatening the recipient government, withdrawing or suspending
#|   aid, or shifting aid. Recipient governments can counter-respond by doing
#|   nothing, modifying policies to improve relationships, or pressuring donors
#|   to back down.
#| include: true
#| fig-width: 6
#| fig-height: 3.8
#| out-width: 80%

# fig-alt-short: "A flowchart showing possible reactions to oppression: recipient governments violate human rights, donors respond, and recipients counter-respond."

nodes <- tribble(
  ~id, ~x, ~y,  ~step,
  1,   0,  2.5, "Violate human rights",
  2,   1,  4,   "Do nothing",
  3,   1,  3,   "Threaten",
  4,   1,  2,   "Withdraw\nor suspend",
  5,   1,  1,   "Shift aid",
  6,   2,  4,   "Do nothing",
  7,   2,  2.5, "Modify to improve\nrelationship",
  8,   2,  1,   "Pressure donors\nto back down"
)

edges <- tibble(
  from = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
  to =   c(2, 3, 4, 5, 6, 7, 8, 6, 7, 8, 6, 7, 8, 6, 7, 8)
)

actors <- tribble(
  ~actor, ~label, ~x,
  "Recipient", "Recipient government\naction", 0,
  "Donor", "Donor government\nresponse", 1,
  "Recipient", "Recipient government\nresponse", 2
) |>
  mutate(
    xmin = x - 0.5,
    xmax = x + 0.5
  )

tidy_graph <- tbl_graph(nodes = nodes, edges = edges) |> 
  create_layout(layout = nodes)

fig1 <- ggraph(tidy_graph) +
  geom_rect(
    data = actors, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = 4.2, fill = actor), inherit.aes = FALSE,
    alpha = 0.25
  ) +
  geom_rect(
    data = actors, aes(xmin = xmin, xmax = xmax, ymin = 4.2, ymax = Inf, fill = actor), inherit.aes = FALSE,
    alpha = 0.5
  ) +
  geom_text(
    data = actors, aes(x = x, y = 4.4, label = label), inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold", lineheight = 1
  ) +
  geom_node_label(
    aes(x = x, y = y, label = step),
    label.r = unit(0, units = "pt"),
    size = 3.5, lineheight = 1, fontface = "bold"
  ) +
  geom_edge_link(
    aes(
      start_cap = label_rect(node1.step, padding = margin(5, 5, 5, 5, "pt")),
      end_cap = label_rect(node2.step, padding = margin(5, 5, 5, 5, "pt"))
    ),
    arrow = arrow(angle = 30, length = unit(6, "pt"), type = "closed"),
    edge_linewidth = 0.25, color = "grey40"
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_fill_manual(values = c("grey40", "grey80"), guide = "none") +
  expand_limits(y = 0.9) +
  theme_void()

fig1

ggsave(
  "img-out/vector/figure-1.pdf", fig1,
  width = 6, height = 3.3, units = "in",
  device = cairo_pdf, bg = "white"
)

ggsave(
  "img-out/vector/figure-1.eps", fig1,
  width = 6, height = 3.3, units = "in",
  device = cairo_ps, bg = "white"
)

ggsave(
  "img-out/vector/figure-1_text.svg", fig1,
  width = 6, height = 3.3, units = "in",
  device = svglite::svglite, bg = "white"
)

ggsave(
  "img-out/vector/figure-1_text-outlines.svg", fig1,
  width = 6, height = 3.3, units = "in",
  device = svg, bg = "white"
)

ggsave(
  "img-out/raster/figure-1_1200.png", fig1,
  width = 6, height = 3.3, units = "in",
  device = ragg::agg_png, res = 1200, bg = "white"
)

ggsave(
  "img-out/raster/figure-1_1200.tiff", fig1,
  width = 6, height = 3.3, units = "in",
  device = grDevices::tiff, res = 1200, bg = "white"
)

#

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-individual-response-tree
#| fig-cap: Possible individual responses to civil society restrictions in recipient countries
#| fig-alt: |
#|   A flowchart showing possible individual and NGO reactions to civil society
#|   restrictions. NGO host governments restrict civil society. Individual private
#|   donors can respond by doing nothing, increasing their donations, or decreasing 
#|   their donations. NGOs then respond in an unknown way.
#| include: true
#| fig-width: 6
#| fig-height: 2.8
#| out-width: 80%

# fig-alt-short: "A flowchart showing possible individual and NGO reactions to civil society restrictions: governments limit civil society, individual donors respond."

nodes <- tribble(
  ~id, ~x, ~y, ~step,
  1,   0,  3,  "Restrict civil society",
  2,   1,  4,  "Do nothing",
  3,   1,  3,  "Increase\ndonations",
  4,   1,  2,  "Decrease\ndonations",
  5,   2,  3,  "?"
)

edges <- tibble(
  from = c(1, 1, 1, 2, 3, 4),
  to =   c(2, 3, 4, 5, 5, 5)
)

actors <- tribble(
  ~actor, ~label, ~x,
  "Recipient", "NGO host government\naction", 0,
  "Donor", "Individual donor\nresponse", 1,
  "NGO", "NGO\nresponse", 2
) |>
  mutate(
    xmin = x - 0.5,
    xmax = x + 0.5
  )

tidy_graph <- tbl_graph(nodes = nodes, edges = edges) |> 
  create_layout(layout = nodes)

fig2 <- ggraph(tidy_graph) +
  geom_rect(
    data = actors, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = 4.2, fill = actor), inherit.aes = FALSE,
    alpha = 0.25
  ) +
  geom_rect(
    data = actors, aes(xmin = xmin, xmax = xmax, ymin = 4.2, ymax = Inf, fill = actor), inherit.aes = FALSE,
    alpha = 0.5
  ) +
  geom_text(
    data = actors, aes(x = x, y = 4.4, label = label), inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold", lineheight = 1
  ) +
  geom_node_label(
    aes(x = x, y = y, label = step),
    label.r = unit(0, units = "pt"),
    size = 3.5, lineheight = 1, fontface = "bold"
  ) +
  geom_edge_link(
    aes(
      start_cap = label_rect(node1.step, padding = margin(5, 5, 5, 5, "pt")),
      end_cap = label_rect(node2.step, padding = margin(5, 5, 5, 5, "pt"))
    ),
    arrow = arrow(angle = 30, length = unit(6, "pt"), type = "closed"),
    edge_linewidth = 0.25, color = "grey40"
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_fill_manual(values = c("grey40", "grey5", "grey80"), guide = "none") +
  expand_limits(y = c(1.8, 4.5)) +
  theme_void()

fig2

ggsave(
  "img-out/vector/figure-2.pdf", fig2,
  width = 6, height = 3.3, units = "in",
  device = cairo_pdf, bg = "white"
)

ggsave(
  "img-out/vector/figure-2.eps", fig2,
  width = 6, height = 3.3, units = "in",
  device = cairo_ps, bg = "white"
)

ggsave(
  "img-out/vector/figure-2_text.svg", fig2,
  width = 6, height = 3.3, units = "in",
  device = svglite::svglite, bg = "white"
)

ggsave(
  "img-out/vector/figure-2_text-outlines.svg", fig2,
  width = 6, height = 3.3, units = "in",
  device = svg, bg = "white"
)

ggsave(
  "img-out/raster/figure-2_1200.png", fig2,
  width = 6, height = 3.3, units = "in",
  device = ragg::agg_png, res = 1200, bg = "white"
)

ggsave(
  "img-out/raster/figure-2_1200.tiff", fig2,
  width = 6, height = 3.3, units = "in",
  device = grDevices::tiff, res = 1200, bg = "white"
)
