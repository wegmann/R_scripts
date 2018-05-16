library(tidyverse)
d0 <- read_csv(system.file("imr", "reg_flow.csv", package = "migest"))
d0 

d1 <- read_csv(system.file("vidwp", "reg_plot.csv", package = "migest"))
d1

library(tweenr)

d2 <- d0 %>%
  mutate(corridor = paste(orig_reg, dest_reg, sep = " -> ")) %>%
  select(corridor, year0, flow) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year0", group = "corridor", ease = "ease", nframes = 100) %>%
  tbl_df()
d2

d2 <- d2 %>%
  separate(col = .group, into = c("orig_reg", "dest_reg"), sep = " -> ") %>%
  select(orig_reg, dest_reg, flow, everything()) %>%
  mutate(flow = flow/1e06)
d2

# create a directory to store the individual plots
dir.create("./plot-gif/")

library(circlize)
for(f in unique(d2$.frame)){
  # open a PNG plotting device
  png(file = paste0("./plot-gif/globalchord", f, ".png"), height = 7, width = 7, 
      units = "in", res = 500)
  
  # intialise the circos plot
  circos.clear()
  par(mar = rep(0, 4), cex=1)
  circos.par(start.degree = 90, track.margin=c(-0.1, 0.1), 
             gap.degree = 4, points.overflow.warning = FALSE)
  
  # plot the chord diagram
  chordDiagram(x = filter(d2, .frame == f), directional = 1, order = d1$region,
               grid.col = d1$col1, annotationTrack = "grid",
               transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
               direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
               diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE)
  
  # add labels and axis
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = d1 %>% filter(region == sector.index) %>% pull(reg1)
    reg2 = d1 %>% filter(region == sector.index) %>% pull(reg2)
    
    circos.text(x = mean(xlim), y = ifelse(is.na(reg2), 3, 4),
                labels = reg1, facing = "bending", cex = 1.1)
    circos.text(x = mean(xlim), y = 2.75, labels = reg2, facing = "bending", cex = 1.1)
    circos.axis(h = "top", labels.cex = 0.8,
                labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
  })
  
  # close plotting device
  dev.off()
}

library(magick)

img <- image_read(path = "./plot-gif/globalchord0.png")
for(f in unique(d2$.frame)[-1]){
  img0 <- image_read(path = paste0("./plot-gif/globalchord",f,".png"))
  img <- c(img, img0)
  message(f)
}

img1 <- image_scale(image = img, geometry = "720x720")

ani0 <- image_animate(image = img1, fps = 10)
image_write(image = ani0, path = "./plot-gif/globalchord.gif")

# fixes for scale gap

scale_gap <- function(flow_m, flow_max, gap_at_max = 1, gaps = NULL) {
  p <- flow_m / flow_max
  if(length(gap_at_max) == 1 & !is.null(gaps)) {
    gap_at_max <- rep(gap_at_max, gaps)
  }
  gap_degree <- (360 - sum(gap_at_max)) * (1 - p)
  gap_m <- (gap_degree + sum(gap_at_max))/gaps
  return(gap_m)
}

d3 <- d2 %>%
  group_by(.frame) %>%
  summarise(flow = sum(flow)) %>%
  mutate(gaps = scale_gap(flow_m = flow, flow_max = max(.$flow), 
                          gap_at_max = 4, gaps = 9))

d3

circos.par(start.degree = 90, track.margin = c(-0.1, 0.1),
           gap.degree = filter(d3, .frame == f)$gaps, 
           points.overflow.warning = FALSE)


library(magrittr)

reg_max <- d0 %>%
  group_by(year0, orig_reg) %>%
  mutate(tot_out = sum(flow)) %>%
  group_by(year0, dest_reg) %>%
  mutate(tot_in = sum(flow)) %>%
  filter(orig_reg == dest_reg) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = orig_reg) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot)/1e06) %$%
  'names<-'(tot_max, reg)

reg_max

chordDiagram(x = filter(d2, .frame == f), directional = 1, order = d1$region,
             grid.col = d1$col1, annotationTrack = "grid",
             transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
             direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",
             diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE, 
             xmax = reg_max)




