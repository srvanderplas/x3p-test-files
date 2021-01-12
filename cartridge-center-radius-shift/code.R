library(png)
library(imager)
library(tidyverse)
library(x3ptools)
tmp <- load.image("cartridge-center-radius-shift/cmc_test.png")
tmp2 <- as.cimg(tmp[,,,1:3])
tmp2 <- grayscale(tmp2)
# plot(tmp2)

#
# mask_cir <- function(dim = c(3600, 3600), off_center = c(0, 0), radius = 1200) {
#   # First get the circle element, in a 2*radius+1 square
#   square <- dim - off_center
#
#   init <- px.circle(radius, x = square[1], y = square[2])
#
#   init %>%
#     as.cimg() %>%
#     pad(off_center[1], 'x', pos = -sign(off_center[1]), val = 0) %>%
#     pad(off_center[2], 'y', pos = -sign(off_center[2]), val = 0) %>%
#     crop.bbox(., px.left(., dim[1])) %>%
#     crop.bbox(., px.top(., dim[2])) %>%
#     (function(x) 1 - x) %>%
#     as.data.frame() %>%
#     filter(value == 1)
#
# }
#
# center_opts <- c(0, 50, 100)
# radius_opts <- c(900, 950, 1000)
# masked_data <- expand.grid(file = "cmc_test.png",
#             center1 = center_opts, center2 = center_opts,
#             radius = radius_opts) %>%
#   mutate(center = map2(center1, center2, ~c(.x, .y)),
#          mask = map2(center, radius, ~mask_cir(off_center = .x, radius = .y))
#   )
#
# masked_data <- mutate(masked_data, name = sprintf("cartridge-center-radius-shift/center_rad_%04d_offset_%03d_%03d.x3p", radius, center1, center2))
# write_rds(masked_data, "cartridge-center-radius-shift/Masked_data.Rds")
masked_data <- read_rds("cartridge-center-radius-shift/Masked_data.Rds")
library(x3ptools)

matrix_to_x3p <- function(x, name) {
  pxset <- x %>% as.cimg %>% as.pixset
  x$value <- tmp2[pxset]
  x$value[x$value == 0] <- NA
  df_to_x3p(x) %>%
    write_x3p(., name, quiet = T)
}
#
# matrix_to_x3p(masked_data$mask[[1]], masked_data$name[1])
# image_x3p(read_x3p("center_rad_0900_offset_000_000.x3p"))

masked_data %>%
  filter(!name %in% list.files("cartridge-center-radius-shift/", "*.x3p"))
purrr::walk2(masked_data$mask, masked_data$name, matrix_to_x3p)
