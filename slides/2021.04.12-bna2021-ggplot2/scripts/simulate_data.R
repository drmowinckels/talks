# Data Generation for BNA2021 Festival R workshop
# 2 April 2021
# delia.fuhrmann@kcl.ac.uk & rik.henson@mrc-cbu.cam.ac.uk
# adapted by a.m.mowinckel@psykologi.uio.no
#######################################

# Create function to simulate data.
# Alter default argument values to customise the simulated data more
simulate_data <- function(Nparticipant =  20,
                          Nface = 8,
                          Nwave = 6,
                          Ntrial = 5,
                          face.emotion = c('Happy','Sad'),
                          min.age = 10,
                          seed = 1234,
                          out_dir = NULL) {
  require(dplyr)
  set.seed(seed = seed)
  
  Nemot <- length(face.emotion)
  participant.age <- 0:(Nwave-1)+min.age # Each participant sampled annually
  
  # Generate Gaussian measurement noise
  noise <- rnorm(Ntrial*Nface*Nemot*Nparticipant*Nwave, mean=0, sd=1) 
  
  # Generate random effects
  face.intercept <- runif(Nface, min=0.1, max=0.9) 
  ppt.intercept <- runif(Nparticipant, min=0.5, max=10)
  ppt.slope <- runif(Nparticipant, min=0.5, max=10)
  
  # Create base data
  df_long <-tibble(
    participant     = rep(1:Nparticipant, each = Ntrial*Nface*Nemot,  times = Nwave),
    face.identity   = rep(1:Nface, each = Ntrial, times = Nemot*Nparticipant*Nwave),
    face.emotion    = rep(face.emotion, each = Ntrial*Nface, times = Nparticipant*Nwave),
    participant.age = rep(participant.age, each = Ntrial*Nface*Nemot*Nparticipant),
    trial           = rep(1:Ntrial, times = Nface*Nemot*Nparticipant*Nwave)
  ) %>% 
    # force to factors for correct modelling
   mutate(
      age.category  = ifelse(participant.age < 13, "Child", "Adolescent"),
      face.identity = as.factor(face.identity),
      face.emotion  = as.factor(face.emotion),
      participant   = as.factor(participant),
      age.category  = factor(age.category, levels = c('Child','Adolescent'))
    ) %>% 
    # add random effects
   mutate(
      face.intercept = rep(face.intercept, each = Ntrial, times = Nemot*Nparticipant*Nwave),
      ppt.intercept  = rep(ppt.intercept, each = Ntrial*Nface*Nemot, times = Nwave),
      ppt.slope      = rep(ppt.slope, each = Ntrial*Nface*Nemot, times = Nwave)
    ) %>% 
    # Generate ffa (only fixed effect that has effect is age (not face.emotion))
   mutate(
      ffa = face.intercept + participant.age/20 + ppt.intercept + ppt.slope*scale(participant.age) + noise
    ) %>% 
   select(starts_with("participant"), age.category, trial, 
                  starts_with("face"), starts_with("ppt"), 
                  ffa, everything()) %>% 
    arrange(participant, participant.age, face.emotion, face.identity, trial)
  
  df_avg <- df_long %>% # Average over trials and stimuli
   group_by(participant, age.category) %>% 
   summarise(ffa = mean(ffa),.groups = "drop")
  
  if(!is.null(out_dir)){
    message("saving files to '", out_dir, "'")
    if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    save(df_long, file = file.path(out_dir, 'df_long.RData'))
    save(df_avg,  file = file.path(out_dir, 'df_avg.RData'))
  }
  return(df_long)
}

df_long <- simulate_data(out_dir = "slides/2021.04.12-bna2021-ggplot2/data")
