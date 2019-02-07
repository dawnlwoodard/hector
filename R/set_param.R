#' Set parameter inside Hector core
#'
#' Set a parameter inside a Hector core to a particular value, and
#' then [reset()] the core.
#'
#' @param parameter Name of parameter to set. Must be length 1. See [parameters].
#' @param value (numeric) New parameter value. Must be length 1 (scalar).
#' @param unit (character) Parameter unit. See [parameters].
#' @param reset_date (numeric) Year to reset to. Default is 0.0. See
#'   `date` argument of [reset()].
#' @inheritParams setvar
#' @return (invisibly) Hector core with parameter changed
#' @author Alexey Shiklomanov
#' @export
set_param <- function(core, parameter, value, unit, reset_date = 0.0) {
  stopifnot(
    length(parameter) == 1,
    length(value) == 1,
    length(unit) == 1
  )
  recognized_parameters <- c(
    PREINDUSTRIAL_CO2(),
    Q10_RH(),
    BETA(),
    ECS(),
    AERO_SCALE(),
    DIFFUSIVITY()
  )
  if (!parameter %in% recognized_parameters) {
    warning(
      "Parameter `", parameter, "` not recognized. ",
      "This may work, but may produce unexpected results."
    )
  }
  setvar(core, NA, parameter, value, unit)
  reset(core, reset_date)
  invisible(core)
}
