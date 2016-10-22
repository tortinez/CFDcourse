#!/bin/bash
#
# Script to install dependencies of Code_Saturne under LINUX
#
# Arnau Miro, Manel Soria
# UPC - ESEIAAT (DFIS)
##############################################################
#
#                   CONFIGURATION PART
#
##############################################################

VERS=4.0.5                # Code_Saturne version to install
INSTALL_DIR="/opt"           # Installation directory
EDITOR=gedit                 # Favourite text editor

##############################################################
#
#                   AUTOMATED PART
#
##############################################################

# Some variables
CODE_SATURNE_TAR=code_saturne-$VERS.tar.gz
CODE_SATURNE_DIR=code_saturne-$VERS

# System update
sudo apt-get update && sudo apt-get upgrade -y

# Install dependencies
sudo apt-get install gfortran mpi-default-bin mpi-default-dev \
                     libxml2-dev pyqt4-dev-tools git make cmake \
                     g++ python2.7-dev zip unzip zlib1g-dev zeitgeist

# Install code_saturne and dependencies
wget http://code-saturne.org/cms/sites/default/files/releases/$CODE_SATURNE_TAR
tar xvzf $CODE_SATURNE_TAR
rm $CODE_SATURNE_TAR

# Put everything into a directory
mkdir Code_Saturne
mv $CODE_SATURNE_DIR Code_Saturne/
sudo mv Code_Saturne $INSTALL_DIR
cd $INSTALL_DIR/Code_Saturne

# Run the auto-install script
./$CODE_SATURNE_DIR/install_saturne.py
sed -i '/prefix /c\prefix    '$INSTALL_DIR'/Code_Saturne/'$VERS'/' setup
$EDITOR setup
./$CODE_SATURNE_DIR/install_saturne.py
