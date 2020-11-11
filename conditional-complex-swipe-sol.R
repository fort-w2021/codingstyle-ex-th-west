# Decide whether to swipe right (express interest) or left (express disinterest)
#   on somebody's profile.
# inputs:
#   swiper: list object representing somebody swiping left or right in a
#   dating app
#   profile: list object representing the profile of the person who is being
#   swiped left or right
# output: TRUE/FALSE
swipe_right <- function(swiper, profile) {
  if (!profile[["has_picture"]]) {
    stop("cant't decide without a picture.")
  }
  if (!profile[["picture_attractive"]]) {
    return(FALSE)
  }
  if (all(is.na(profile[["likes"]]))) {
    stop("can't decide without informative profile.")
  }
  if (swiper[["sober"]] & !profile[["rather_weird"]]
  & any(swiper[["likes"]] %in% profile[["likes"]])) {
    return(TRUE)
  } else {
    ifelse(!swiper[["sober"]], TRUE, FALSE)
  }
}
