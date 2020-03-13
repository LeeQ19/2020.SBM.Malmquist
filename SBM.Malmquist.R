#########################################################################################################################
### Project  : 
### Script   : 
### Contents : 
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
pkgs <- c("ggplot2", "DJL")
sapply(pkgs, require, character.only = T)

# Load data & parameters
df.2d <- read.csv("dataset.csv", header = T)
df.3d <- simplify2array(by(df.2d[,-c(1:3, 5)], df.2d$year, as.matrix))
name <- unique(df.2d$id)
id.x  <- c(2:7, 9) #  in: prod, ce, fiscal, emp, fert, mach, pest, area, water (eco has missing value)
id.y  <- c(1) # out: prod


#########################################################################################################################
### Productivity Analysis
#########################################################################################################################

# Run SBM each year
f.t     <- rev(unique(df.2d$year))
res.sbm <- matrix(NA, nrow(df.3d), length(f.t), dimnames = list(name, f.t))
for(i in 1:length(f.t)){res.sbm[, i] <- dm.sbm(df.3d[, id.x, i], df.3d[, id.y, i])$eff}

# SBM long
df.sbm.raw <- data.frame(Year = rep(f.t, each = length(name)),
                         DMU  = name,
                         Eff  = c(res.sbm))

# Figure 5. Prodictivity changes of 33 gas providers (900*600)
ggplot(data = df.sbm.raw, aes(x = Year, y = Eff, group = DMU, colour = DMU)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = df.sbm.raw, aes(x = Year, y = Eff, group = DMU, colour = DMU), size = 1.2) + 
  scale_x_continuous(name = "Year",             breaks = seq(f.t[1], f.t[length(f.t)],   1)) +
  scale_y_continuous(name = "Efficiency (SBM)", breaks = seq( 0.1,    1, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Run Malmquist
res.malm.raw <- roc.malmquist(df.3d[,id.x,], df.3d[,id.y,], tm = f.t, dm = "sbm", orientation = "n")

# Figure 6. CU (900*600)
ggplot(data = res.malm.raw$cu, 
       aes(x = Period, y = CU, group = DMU, colour = DMU)) + 
  geom_hline(yintercept = 1.0, color = "gray", size = 1) +
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.raw$cu, aes(x = Period, y = CU, group = DMU, colour = DMU), size = 1.2) +
  scale_y_continuous(name = "Technical Efficiency Change (TEC)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 7. FS (900*600)
ggplot(data = res.malm.raw$fs, 
       aes(x = Period, y = FS, group = DMU, colour = DMU)) + 
  geom_hline(yintercept = 1.0, color = "gray", size = 1) +
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.raw$fs, aes(x = Period, y = FS, group = DMU, colour = DMU), size = 1.2) + 
  scale_y_continuous(name = "Frontier Shift (FS)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 8. MI (900*600)
ggplot(data = res.malm.raw$mi, 
       aes(x = Period, y = MI, group = DMU, colour = DMU)) + 
  geom_hline(yintercept = 1.0, color = "gray", size = 1) +
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.raw$mi, aes(x = Period, y = MI, group = DMU, colour = DMU), size = 1.2) + 
  scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

