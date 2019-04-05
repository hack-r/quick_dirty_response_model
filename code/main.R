##########################################
# File:            main.R                #
# Context:         Master file           #
# Contact author:  Miller, J.            #
# Initial Date:    2019-04-04            #
# Version:         0.0.1                 #
# Description:     Ad hoc response model #
##########################################


# Libraries and Options ---------------------------------------------------
source("code//libraries_options_functions.R")


# Extract Raw Data --------------------------------------------------------
source("..//code//pull_data.R")

# Feature Engineering -----------------------------------------------------
source("..//code//feature_engineering.R")

# EDA ---------------------------------------------------------------------
source("..//code//eda.R")

# Modeling ----------------------------------------------------------------

# BL1: XGB
source("..//code//eda.R")

# BL2-4: RF, RRF, GRRF 
source("..//code//eda.R")

# ML1: Artifical Neural Network (ANN/DL)
source("..//code//ml1.R")

# BL5-7, ML2 (H2O StackedEnsemble)
source("..//code//h2o.R")


# Validation --------------------------------------------------------------
source("..//code//validate.R")


# Export Results ----------------------------------------------------------

