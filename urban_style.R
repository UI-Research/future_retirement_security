titler <- function(title) {
  textGrob(title, 
           x = unit(0, "npc"), 
           hjust = 0, 
           vjust = 0,
           gp = gpar(fontsize = 12, fontfamily = "Lato"))
}

subtitler <- function(subtitle) {
  textGrob(subtitle, 
           x = unit(0, "npc"), 
           hjust = 0, 
           vjust = 0,
           gp = gpar(fontsize = 9.5, fontfamily = "Lato"))
}

sourcer <- function(source) {
  grobTree(
    textGrob("Source: ", 
             name = "source1",
             x = unit(0, "npc"), 
             hjust = 0, 
             vjust = 0,
             gp = gpar(fontsize = 8, fontfamily = "Lato", fontface = "bold")),
    textGrob(source, 
             x = unit(0, "npc") + grobWidth("source1"), 
             hjust = 0, 
             vjust = 0,
             gp = gpar(fontsize = 8, fontfamily = "Lato"))
  )
}

caption <- grobTree(
  gp = gpar(fontsize = 7, hjust = 1), 
  textGrob(label = "I N S T I T U T E", 
           name = "caption1",
           x = unit(1, "npc"),  
           y = unit(0, "npc"),
           hjust = 1, 
           vjust = 0),
  textGrob(label = "U R B A N  ", 
           x = unit(1, "npc") - grobWidth("caption1") - unit(0.01, "lines"),         
           y = unit(0, "npc"), 
           hjust = 1, 
           vjust = 0, 
           gp = gpar(col = "#1696d2"))
)

plotr <- function(..., heights) {
  grid.arrange(...,
               heights = heights) 
}
