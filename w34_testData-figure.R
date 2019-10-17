source("packages.R")

w34 <- nc::capture_first_df(
  fread("w34_testData.csv"),
  bin=list(
    "\\(",
    height.lower=".*", as.numeric,
    ",",
    height.upper=".*", as.numeric,
    "\\]",
    prop=".*", as.numeric,
    "%"))
w34[, height.mid := (height.lower+height.upper)/2]

table(w34$treeID)

some.trees <- w34[treeID %in% c(1, 2, 3, 13, 9, 11, 16, 19)]

gg.data <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_wrap("treeID", labeller=label_both)+
  geom_point(aes(
    `95th_eucDist`, height.mid),
    data=some.trees)

g <- gfpop::graph(
  gfpop::Edge("trunk", "canopy", "up"),
  gfpop::Edge("canopy", "canopy", "down"))

seg.dt <- some.trees[, {
  fit <- gfpop::gfpop(`95th_eucDist`, g)
  i.start <- with(fit, c(1L, changepoints[-length(changepoints)]+1L))
  i.end <- fit$changepoints
  data.table(
    i.start,
    i.end,
    height.start=height.lower[i.start],
    height.end=height.upper[i.end],
    state=fit$states,
    mean=fit$parameters)
}, by=treeID]

gg.model <- gg.data+
  geom_segment(aes(
    mean, height.start,
    xend=mean, yend=height.end),
    color="red",
    size=1,
    data=seg.dt)+
  geom_hline(aes(
    yintercept=height.end),
    color="red",
    data=seg.dt[i.start==1])+
  xlab("Distance/radius from trunk parallel to ground (meters)")+
  ylab("Height from ground (meters)")

png("w34_testData-figure.png", 6, 6, units="in", res=100)
print(gg.model)
dev.off()
