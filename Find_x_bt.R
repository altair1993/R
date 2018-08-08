# solve x of a function by using [uniroot]
# without the limitation that two bound's anwers must have diff sign (+/-)

find_x_bt <- function(fn, low, up) {
  strg = c()
  
  newx = "a"
  oldx = "a"
  
  seqx <- seq(from = low, to = up, by = 0.05)
  
  for (i in seqx) {
    if (fr(i - 1) * fr(i + 1) < 0) {
      newx <- round(uniroot(fr, interval = c(i - 1, i + 1))[[1]], digits = 4)
      if (newx != oldx) {
        strg <- c(strg, newx)
        oldx <- newx
      } 
    } 
  }
  return(strg)
}
