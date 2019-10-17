source("packages.R")

w34 <- nc::capture_first_df(
  fread("w34_testData.csv"),
  bin=list(
    "\\(",
    lower=".*", as.numeric,
    ",",
    upper=".*", as.numeric,
    "\\]",
    prop=".*", as.numeric,
    "%"))
w34[, mid := (lower+upper)/2]

table(w34$treeID)

some.trees <- w34[treeID %in% c(1, 3, 13, 9, 11, 16, 19)]

gg.data <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_wrap("treeID")+
  geom_point(aes(
    mid, `95th_eucDist`),
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
    segStart=lower[i.start],
    segEnd=upper[i.end],
    state=fit$states,
    mean=fit$parameters)
}, by=treeID]

gg.model <- gg.data+
  geom_segment(aes(
    segStart, mean,
    xend=segEnd, yend=mean),
    color="red",
    size=1,
    data=seg.dt)+
  geom_vline(aes(
    xintercept=segEnd),
    color="red",
    data=seg.dt[i.start==1])

png("w34_testData-figure.png", 6, 6, units="in", res=100)
print(gg.model)
dev.off()
