# Carried left entries
c_l_entry_lefty <- c_l_entry %>%
  filter(Handedness == 'L')
c_l_entry_righty <- c_l_entry %>%
  filter(Handedness == 'R')
c_l_entry_lefty_xG = sum(c_l_entry_lefty$xG,na.rm = TRUE)/(length(which(c_l_entry_lefty$Event == 'Zone Entry')))
c_l_entry_righty_xG = sum(c_l_entry_righty$xG,na.rm = TRUE)/(length(which(c_l_entry_righty$Event == 'Zone Entry')))

# Carried middle left entries
c_ml_entry_lefty <- c_ml_entry %>%
  filter(Handedness == 'L')
c_ml_entry_righty <- c_ml_entry %>%
  filter(Handedness == 'R')
c_ml_entry_lefty_xG = sum(c_ml_entry_lefty$xG,na.rm = TRUE)/(length(which(c_ml_entry_lefty$Event == 'Zone Entry')))
c_ml_entry_righty_xG = sum(c_ml_entry_righty$xG,na.rm = TRUE)/(length(which(c_ml_entry_righty$Event == 'Zone Entry')))

# Carried middle entries
c_m_entry_lefty <- c_m_entry %>%
  filter(Handedness == 'L')
c_m_entry_righty <- c_m_entry %>%
  filter(Handedness == 'R')
c_m_entry_lefty_xG = sum(c_m_entry_lefty$xG,na.rm = TRUE)/(length(which(c_m_entry_lefty$Event == 'Zone Entry')))
c_m_entry_righty_xG = sum(c_m_entry_righty$xG,na.rm = TRUE)/(length(which(c_m_entry_righty$Event == 'Zone Entry')))

# Carried middle right entries
c_mr_entry_lefty <- c_mr_entry %>%
  filter(Handedness == 'L')
c_mr_entry_righty <- c_mr_entry %>%
  filter(Handedness == 'R')
c_mr_entry_lefty_xG = sum(c_mr_entry_lefty$xG,na.rm = TRUE)/(length(which(c_mr_entry_lefty$Event == 'Zone Entry')))
c_mr_entry_righty_xG = sum(c_mr_entry_righty$xG,na.rm = TRUE)/(length(which(c_mr_entry_righty$Event == 'Zone Entry')))

# Carried right entries
c_r_entry_lefty <- c_r_entry %>%
  filter(Handedness == 'L')
c_r_entry_righty <- c_r_entry %>%
  filter(Handedness == 'R')
c_r_entry_lefty_xG = sum(c_r_entry_lefty$xG,na.rm = TRUE)/(length(which(c_r_entry_lefty$Event == 'Zone Entry')))
c_r_entry_righty_xG = sum(c_r_entry_righty$xG,na.rm = TRUE)/(length(which(c_r_entry_righty$Event == 'Zone Entry')))

# Played left entries
p_l_entry_lefty <- p_l_entry %>%
  filter(Handedness == 'L')
p_l_entry_righty <- p_l_entry %>%
  filter(Handedness == 'R')
p_l_entry_lefty_xG = sum(p_l_entry_lefty$xG,na.rm = TRUE)/(length(which(p_l_entry_lefty$Event == 'Zone Entry')))
p_l_entry_righty_xG = sum(p_l_entry_righty$xG,na.rm = TRUE)/(length(which(p_l_entry_righty$Event == 'Zone Entry')))

# Played middle left entries
p_ml_entry_lefty <- p_ml_entry %>%
  filter(Handedness == 'L')
p_ml_entry_righty <- p_ml_entry %>%
  filter(Handedness == 'R')
p_ml_entry_lefty_xG = sum(p_ml_entry_lefty$xG,na.rm = TRUE)/(length(which(p_ml_entry_lefty$Event == 'Zone Entry')))
p_ml_entry_righty_xG = sum(p_ml_entry_righty$xG,na.rm = TRUE)/(length(which(p_ml_entry_righty$Event == 'Zone Entry')))

# Played middle entries
p_m_entry_lefty <- p_m_entry %>%
  filter(Handedness == 'L')
p_m_entry_righty <- p_m_entry %>%
  filter(Handedness == 'R')
p_m_entry_lefty_xG = sum(p_m_entry_lefty$xG,na.rm = TRUE)/(length(which(p_m_entry_lefty$Event == 'Zone Entry')))
p_m_entry_righty_xG = sum(p_m_entry_righty$xG,na.rm = TRUE)/(length(which(p_m_entry_righty$Event == 'Zone Entry')))

# Played middle right entries
p_mr_entry_lefty <- p_mr_entry %>%
  filter(Handedness == 'L')
p_mr_entry_righty <- p_mr_entry %>%
  filter(Handedness == 'R')
p_mr_entry_lefty_xG = sum(p_mr_entry_lefty$xG,na.rm = TRUE)/(length(which(p_mr_entry_lefty$Event == 'Zone Entry')))
p_mr_entry_righty_xG = sum(p_mr_entry_righty$xG,na.rm = TRUE)/(length(which(p_mr_entry_righty$Event == 'Zone Entry')))

# Played right entries
p_r_entry_lefty <- p_r_entry %>%
  filter(Handedness == 'L')
p_r_entry_righty <- p_r_entry %>%
  filter(Handedness == 'R')
p_r_entry_lefty_xG = sum(p_r_entry_lefty$xG,na.rm = TRUE)/(length(which(p_r_entry_lefty$Event == 'Zone Entry')))
p_r_entry_righty_xG = sum(p_r_entry_righty$xG,na.rm = TRUE)/(length(which(p_r_entry_righty$Event == 'Zone Entry')))

# Dumped left entries
d_l_entry_lefty <- d_l_entry %>%
  filter(Handedness == 'L')
d_l_entry_righty <- d_l_entry %>%
  filter(Handedness == 'R')
d_l_entry_lefty_xG = sum(d_l_entry_lefty$xG,na.rm = TRUE)/(length(which(d_l_entry_lefty$Event == 'Zone Entry')))
d_l_entry_righty_xG = sum(d_l_entry_righty$xG,na.rm = TRUE)/(length(which(d_l_entry_righty$Event == 'Zone Entry')))

# Dumped middle left entries
d_ml_entry_lefty <- d_ml_entry %>%
  filter(Handedness == 'L')
d_ml_entry_righty <- d_ml_entry %>%
  filter(Handedness == 'R')
d_ml_entry_lefty_xG = sum(d_ml_entry_lefty$xG,na.rm = TRUE)/(length(which(d_ml_entry_lefty$Event == 'Zone Entry')))
d_ml_entry_righty_xG = sum(d_ml_entry_righty$xG,na.rm = TRUE)/(length(which(d_ml_entry_righty$Event == 'Zone Entry')))

# Dumped middle entries
d_m_entry_lefty <- d_m_entry %>%
  filter(Handedness == 'L')
d_m_entry_righty <- d_m_entry %>%
  filter(Handedness == 'R')
d_m_entry_lefty_xG = sum(d_m_entry_lefty$xG,na.rm = TRUE)/(length(which(d_m_entry_lefty$Event == 'Zone Entry')))
d_m_entry_righty_xG = sum(d_m_entry_righty$xG,na.rm = TRUE)/(length(which(d_m_entry_righty$Event == 'Zone Entry')))

# Dumped middle right entries
d_mr_entry_lefty <- d_mr_entry %>%
  filter(Handedness == 'L')
d_mr_entry_righty <- d_mr_entry %>%
  filter(Handedness == 'R')
d_mr_entry_lefty_xG = sum(d_mr_entry_lefty$xG,na.rm = TRUE)/(length(which(d_mr_entry_lefty$Event == 'Zone Entry')))
d_mr_entry_righty_xG = sum(d_mr_entry_righty$xG,na.rm = TRUE)/(length(which(d_mr_entry_righty$Event == 'Zone Entry')))

# Dumped right entries
d_r_entry_lefty <- d_r_entry %>%
  filter(Handedness == 'L')
d_r_entry_righty <- d_r_entry %>%
  filter(Handedness == 'R')
d_r_entry_lefty_xG = sum(d_r_entry_lefty$xG,na.rm = TRUE)/(length(which(d_r_entry_lefty$Event == 'Zone Entry')))
d_r_entry_righty_xG = sum(d_r_entry_righty$xG,na.rm = TRUE)/(length(which(d_r_entry_righty$Event == 'Zone Entry')))

