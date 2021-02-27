#' @name run_model
#' @title Run AlgalGame model
#' @param inputs A list of parameters. Run \link{show_parms} to see all settings.
#' @param Ttot Total of time
#' @param dt Time interval
#' @param L Column depth
#' @param N Number of vertical grids
#' @param ini Initial states of Biomass and Nutrient
#' @param opt.abg option of abg, default as \code{"Zhou2019"}
#' @param plot whether to plot the raster
#' @param out_steady Whether to run steady resolver
#' @param out_steady.list Input params of \link{steady.1D}
#' @return An out object.
#' @references
#' Klausmeier C A, Litchman E. Algal games: The vertical distribution of phytoplankton in
#'   poorly mixed water columns\[J\]. Limnology and Oceanography, 2001, 46(8): 1998-2007.
#' @importFrom deSolve ode.1D
#' @importFrom ReacTran setup.prop.1D setup.grid.1D tran.1D advection.1D
#' @importFrom rootSolve steady.1D
#' @importFrom utils read.csv
#' @importFrom cowplot plot_grid
#' @importFrom data.table frollapply
#' @export
#' @examples
#' library(AlgalGame)
#' res <- run_model()
#' ggimage(res$out)
#'
run_model <- function(
  inputs = NULL,
  Ttot = 2, dt = 1e-3,
  L = 2, N = 50,
  ini = NULL,
  opt.abg = "Zhou2019",
  plot = TRUE,
  run.steady = FALSE,
  run.steady.list = list(time = 0),
  moving_avg = FALSE
) {

  if(!is.null(opt.abg)) {
    if(opt.abg == "Zhou2019") {
      daily_abg <- read.csv(system.file("abg_daily_taihu.csv",
                                        package = "AlgalGame"),
                            comment.char = "#")
    }
  }

  # parameter settings
  parms <- show_parms()
  parms <- parms$Value %>% setNames(., parms$Param)
  parms <- as.list(parms)
  parms$DR <- parms$Db

  # replace by inputs
  if(!is.null(opt.abg) & is.null(inputs$abg)) {
    parms$abg <- round(mean(daily_abg$abg), 3)
  }
  fresh_inputs <- names(inputs) %in% names(parms)
  if(any(fresh_inputs == FALSE)) {
    names(inputs)[fresh_inputs == FALSE] %>%
      paste0("`", ., "`") %>%
      paste0(., collapse = ", ") %>%
      sprintf("%s are not parameters requred!", .) %>%
      paste0(., "\nRun `show_parms` to check it out") %>%
      warning()
  }
  tofresh_inputs <- inputs[which(fresh_inputs == TRUE)]
  for(n_parm in names(tofresh_inputs)) {
    parms[n_parm] <- inputs[n_parm]
  }

  # grid of space
  Grid  <- setup.grid.1D(L = L, N = N)
  x     <- Grid$x.mid
  N     <- Grid$N

  # grid of time
  dt    <- dt
  times <- seq(0, Ttot, dt)

  # ini state
  if(is.null(ini)) {
    # bini <- c(rep(1000, floor(N/3)),
    #           rep(2000, floor(N/3)),
    #           rep(1000, N - 2*floor(N/3)))
    bini <- rep(2000, N)
    ini <- list(bini = bini, Rini = rep(0.1, N))
    state <- c(ini$bini, ini$Rini) # bini unit [ cells / L]
  } else {
    state <- c(ini$bini, ini$Rini) # bini unit [ cells / L]
  }

  # CFL condition: CFL should <= Cmax (depending on whether the solver is explicit or implicit)
  CFL = parms$vmax * dt / unique(Grid$dx)
  if(CFL > 1) {
    dt <- round(dt / CFL, 4)
    times <- seq(0, Ttot, dt)
  }

  # Stable condition: ddd should <= 1/2
  # ddd = parms$Db * dt / (Grid$dx[1]^2)
  # if(ddd > 1/2) {
  #   dt <- round((Grid$dx[1]^2)/(2*parms$Db), 4)
  #   times <- seq(0, Ttot, dt)
  # }

  # run the ode
  parms$N <- N
  parms$Grid <- Grid

  if(run.steady == FALSE) {

    out <- ode.1D(
      y      = state,
      times  = times,
      func   = Model,
      parms  = parms,
      names  = c("Biomass", "Nutrient"),
      nspec  = 2,
      # method = "vode",
      method = "lsode",
      atol   = rep(c(1e-2, 1e-4), each = N),
      rtol   = rep(c(1e-2, 1e-4), each = N)
    )

  } else {

    out <- NULL

  }


  # steady
  if(run.steady) {

    out.steady <-
      with(run.steady.list, {

        steady.1D(
          y      = state,
          time   = time,
          func   = Model,
          parms  = parms,
          names  = c("Biomass", "Nutrient"),
          nspec  = 2,
          method = "runsteady",
          atol   = rep(c(1e-2, 1e-4), each = N),
          rtol   = rep(c(1e-2, 1e-4), each = N)
        )

        })

  } else {

    out.steady <- NULL

  }

  # plot the results
  if(run.steady == FALSE & plot) {

    # title_text <- with(parms, sprintf("abg=%.3f a=%.3f vmax=%.3f", abg, a, vmax))

    plotlist <- list(
      ggimage(out, "Biomass", depth = Grid$x.mid),
      ggimage(out, "Nutrient", depth = Grid$x.mid),
      ggimage(out, "Light", trans = "log10", depth = Grid$x.mid) #+ ggtitle(title_text)
    )

    gplot <- plot_grid(plotlist = plotlist, align = "v", ncol = 1)

  } else {

    gplot <- NULL

  }

  return(
    list(
      out        = out,
      out.steady = out.steady,
      parms      = parms,
      depth      = parms$Grid$x.mid,
      Ttot       = Ttot,
      dt         = dt,
      L          = L,
      N          = N,
      opt.abg    = opt.abg,
      plot       = plot,
      run.steady = run.steady,
      run.steady.list = run.steady.list,
      gplot      = gplot,
      CFL        = CFL
    )
  )

}


#' @name MichMem.2
#' @title Liebig and MichMen functions
#' @param I  Light
#' @param R  Nutrient
#' @param KI light half-saturation constant
#' @param KR P half-saturation constant
#' @param Is Is
#' @param Rs Rs
#' @param r  r
#' @param m  m
#' @noRd
MichMem.2 <- function(I, R, KI, KR, Is, Rs, r, m) {
  Is    <- KI * m / (r - m)
  Rs    <- KR * m / (r - m)
  fI    <- round(r*I/(I+KI), 8)
  fR    <- round(r*R/(R+KR), 8)
  sign_I = sign(pmax(I - Is, 0))
  sign_R = sign(pmax(R - Rs, 0))
  signs <- sign_I * sign_R
  LIMIT <- rep(NA, length(I))
  LIMIT[sign_I == 1 & sign_R == 1] <- 0 # "None"
  LIMIT[sign_I == 0 & sign_R == 1] <- 1 # "Light"
  LIMIT[sign_I == 1 & sign_R == 0] <- 2 # "Nutrient"
  LIMIT[sign_I == 0 & sign_R == 0] <- 3 # "Both"
  # net_g <- pmin(fI, fR) * signs - m
  net_g <- pmin(fI, fR) - m
  return(
    list(
      fI = fI,
      fR = fR,
      net_g = net_g,
      Is = Is,
      Rs = Rs,
      LIMIT = LIMIT
    )
  )
}


#' @name Model
#' @title Biomass - Nutrient - Light PDEs
#' @param t time
#' @param state state
#' @param parms parms
#' @importFrom scales rescale
#' @noRd
Model <- function(t, state, parms) {
  with(parms, {

    # get the current variable from state
    B <- state[1:N]
    R <- state[(N+1):(2*N)]

    # Light
    Iin_time    <- pmax(858.2*sin(0.3179*((t*24+8) %% 24)+4.098), 0)
    I           <- Iin * exp(-cumsum(Grid$dx*(a*B+abg)))
    # I           <- Iin_time * exp(-cumsum(Grid$dx*(a*B+abg)))
    I = round(I, 8)
    I[which(is.infinite(I))] <- 0

    # calculate the net growth rate and grid
    res_mm2     <- MichMem.2(I, R, KI, KR, Is, Rs, r, m)
    net_g       <- res_mm2$net_g
    net_g_grid  <- setup.prop.1D(xy = cbind(x=Grid$x.mid, y=net_g),
                                 grid = Grid,
                                 interpolate = "linear")

    # nutrient
    Uptake      <- -B / Y * (net_g + m)
    Recycling   <- e_r * m * B / Y
    Rdiffusion  <- tran.1D(C = R,
                           D = DR,
                           v = 0,
                           flux.up = 0,
                           flux.down = -h*(Rin - R[length(R)]),
                           # flux.down = 0,
                           # C.down = 20,
                           dx = Grid)

    Rnew        <- Uptake + Recycling + Rdiffusion$dC

    # biomass
    net_g_grid$int <- round(net_g_grid$int, 8)
    vsign          <- sign(diff(net_g_grid$int))
    weight         <- abs(diff(net_g_grid$int)) %>% rescale() %>% round(3)

    Movement.adv    <- advection.1D(C = B,
                                    v = vmax * vsign * weight,
                                    # v = -vmax,
                                    adv.method = "muscl",
                                    # adv.method = "quick",
                                    flux.up = 0,
                                    flux.down = 0,
                                    dx = Grid)

    Movement.dif    <- tran.1D(C = B,
                               D = Db,
                               flux.up = 0,
                               flux.down = 0,
                               dx = Grid)

    NetGrowth   <- net_g * B
    Bnew        <- NetGrowth + Movement.adv$dC + Movement.dif$dC
    # Bnew <- Movement.dif$dC

    # return variables
    res <- c(Bnew, Rnew)
    return(list(
      res,
      Light = I#,
      # vsign = vsign,
      # Uptake = Uptake,
      # NetGrowth = NetGrowth,
      # Move.adv = Movement.adv$dC,
      # Move.dif = Movement.dif$dC,
      # LIMIT    = res_mm2$LIMIT
      ))

  })
}


#' @name  show_parms
#' @title Print all parameters in AlgalGame
#' @export
#' @return list of parameters
show_parms <- function() {
  parms <- read.csv(system.file("parms.csv", package = "AlgalGame"),
                    stringsAsFactors = FALSE,
                    skip = 1)
  return(parms)
}




