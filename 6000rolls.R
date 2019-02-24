library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)

NumRolls <- 6000
DiceSides <- 6
NumImages <- 300
PlotToScreen <- FALSE
SaveToFiles <- TRUE

if(NumRolls %% NumImages) {
  stop("NumRolls must be evenly divisible by NumImages.\n")
}

RollDice <- function(x) {
  return(sample(1:DiceSides, 1))
}

GetTotals <- function(rollNumber, rollValue) {
  return(
    sum(
      length(rslt_raw[rslt_raw$roll <= rollNumber & rslt_raw$value == rollValue, 2])
    )
  )
}

rslt_raw <- data.frame(roll = 1:NumRolls, value = sapply(1:NumRolls, RollDice))

numVals <- NumRolls * DiceSides

rslt_summary <- tibble(
  roll_indices = unlist(lapply(1:NumRolls, rep.int, times = DiceSides)),
  dice_values = factor(rep(1:DiceSides, NumRolls), levels = 1:DiceSides),
  counts = integer(numVals),
  fraction = double(numVals)
)

rslt_summary$counts <- mapply(GetTotals,
                              rollNumber = rslt_summary$roll_indices,
                              rollValue = rslt_summary$dice_values)

rslt_summary$fraction <- rslt_summary$counts / rslt_summary$roll_indices

# *********************************************************************
graphics.off()

dir1 <- file.path(".", "dice_simulation_img1")

if(!dir.exists(dir1)) {
  dir.create(dir1)
}

PlotGraph1 <- function(i) {
  ggplot() +
    geom_col(
      aes(x = dice_values, y = counts, fill = dice_values),
      rslt_summary[1:DiceSides + DiceSides * (i - 1), ], show.legend = FALSE) +
    geom_hline(yintercept = i / DiceSides, linetype = 2, size = 1.5) +
    coord_flip() +
    #lims(y = c(0, ceiling(NumRolls / DiceSides / 100) * 100)) +
    labs(
      title = paste("Simulating rolling a", DiceSides, "sided die"),
      subtitle = paste("Number of rolls:", sprintf("%04d", i)),
      x = "Die Faces",
      y = "Total Rolls"
    ) +
    theme(
      plot.margin = margin(25, 35, 20, 20),
      axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 10)),
      text = element_text(size = 18)
    )
}

for(i in 1:NumImages * NumRolls / NumImages) {
  
  cat(paste("Plot ", i, " of ", NumRolls, ": ", round(i / NumRolls * 100, 1), "% complete.\n"), sep = "")
  
  p1 <- PlotGraph1(i)
  
  if(PlotToScreen) {
    print(p1)
  }
  
  if(SaveToFiles) {
    ggsave(
      filename = file.path(dir1, paste0("img", sprintf("%04d", i), ".png")),
      plot = p1)
  }
}  


# **************************************************************
dir2 <- file.path(".", "dice_simulation_img2")

if(!dir.exists(dir2)) {
  dir.create(dir2)
}

PlotGraph2 <- function(i) {
  ggplot(rslt_summary[rslt_summary$roll_indices <= i, ],
         aes(
           x = dice_values,
           y = fraction,
           fill = dice_values)) +
    geom_boxplot(show.legend = FALSE) +
    geom_hline(yintercept = 1 / DiceSides, linetype = 2, size = 1.5, color = "gray", alpha = 0.5) +
    labs(
      title = paste("Simulating rolling a", DiceSides, "sided die"),
      subtitle = paste("Number of rolls:", sprintf("%04d", i)),
      x = "Die Faces",
      y = "Fraction of Occurrences"
    ) +
    theme(
      plot.margin = margin(25, 35, 20, 20),
      axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
      text = element_text(size = 18)
    )
}

for(i in 1:NumImages * NumRolls / NumImages) {
  cat(paste("Plot ", i, " of ", NumRolls, ": ", round(i / NumRolls * 100, 1), "% complete.\n"), sep = "")
  
  p2 <- PlotGraph2(i)
  
  if(PlotToScreen) {
    print(p2)
  }
  
  if(SaveToFiles) {
    ggsave(
      filename = file.path(dir2, paste0("img", sprintf("%04d", i), ".png")),
      plot = p2)
  }
}