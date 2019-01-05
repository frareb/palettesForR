#' Show a palette.
#' @param myPal The palette name.
#' @details Adapted from http://www.r-graph-gallery.com/42-colors-names/
#' @examples 
#' data(Caramel_gpl)
#' showPalette(myPal = Caramel_gpl)
#' @export
showPalette <- function(myPal){
  op <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(0, 0, 2, 0))
  graphics::plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       axes = FALSE, xlab = "", ylab = "")
  numRow <- floor(sqrt(length(myPal))) + 1
  numCol <- numRow
  graphics::rect(
    xleft = rep((0:(numCol - 1)/numCol), numRow),  
    ybottom = sort(rep((0:(numRow - 1)/numRow),numCol), decreasing = TRUE),
    xright = rep((1:numCol/numCol), numRow),
    ytop = sort(rep((1:numRow/numRow), numCol), decreasing = TRUE),
    border = grDevices::grey(0.5), 
    col = myPal[seq(1, numRow*numCol)])
  myLabels <- c(as.character(1:length(myPal)), 
                rep("", numRow*numCol - length(myPal)))
  graphics::text(
    x = rep((0:(numCol - 1)/numCol), numRow) + 0.02,
    y = sort(rep((0:(numRow - 1)/numRow), numCol), decreasing = TRUE) + 0.02,
    labels = myLabels, 
    cex = 0.6)
  graphics::par(op)
}
