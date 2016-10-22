!-------------------------------------------------------------------------------

!                      Code_Saturne version 4.0.3
!                      --------------------------
! This file is part of Code_Saturne, a general-purpose CFD tool.
!
! Copyright (C) 1998-2015 EDF S.A.
!
! This program is free software; you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation; either version 2 of the License, or (at your option) any later
! version.
!
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
! details.
!
! You should have received a copy of the GNU General Public License along with
! this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
! Street, Fifth Floor, Boston, MA 02110-1301, USA.

!-------------------------------------------------------------------------------

! Purpose:
! -------

! User subroutines for input of calculation parameters (Fortran modules).
!   These subroutines are called in all cases.

! If the Code_Saturne GUI is used, this file is not required (but may be
!   used to override parameters entered through the GUI, and to set
!   parameters not accessible through the GUI).

! Several routines are present in the file, each destined to defined
!   specific parameters.

! To modify the default value of parameters which do not appear in the
!   examples provided, code should be placed as follows:
!   - usipsu   for numerical and physical options
!   - usipes   for input-output related options

! As a convention, "specific physics" defers to the following modules only:
!   pulverized coal, gas combustion, electric arcs.

! In addition, specific routines are provided for the definition of some
!   "specific physics" options.
!   These routines are described at the end of this file and will be activated
!   when the corresponding option is selected in the usppmo routine.

!-------------------------------------------------------------------------------

subroutine usipsu ( nmodpp )
!===============================================================================
! Purpose:
! -------

! User subroutine for the input of additional user parameters.

!-------------------------------------------------------------------------------
! Arguments
!__________________.____._____.________________________________________________.
! name             !type!mode ! role                                           !
!__________________!____!_____!________________________________________________!
! nmodpp           ! i  ! <-- ! number of active specific physics models       !
!__________________!____!_____!________________________________________________!

!     Type: i (integer), r (real), s (string), a (array), l (logical),
!           and composite types (ex: ra real array)
!     mode: <-- input, --> output, <-> modifies data, --- work array
!===============================================================================

!===============================================================================
! Module files
!===============================================================================

use paramx
use cstnum
use dimens
use numvar
use optcal
use cstphy
use entsor
use parall
use period
use ihmpre
use albase
use ppppar
use ppthch
use ppincl
use coincl
use cpincl
use elincl
use field
use cavitation
use rotation

!===============================================================================

implicit none

! Arguments

integer nmodpp

! Local variables

logical       ilved, inoprv
integer       ii, jj, kscmin, kscmax, keydri
integer       f_id, idim1, itycat, ityloc, iscdri, iscal, ifcvsl

!===============================================================================
! Gradient reconstruction
!===============================================================================
! --- imrgra: Type of gradient reconstruction
! ==========
!        0 <-- iterative reconstruction of the non-orthogonalities
!        1 <-- least squares method based on the first neighbour cells
!        2 <-- least squares method based on the extended neighbourhood
!        3 <-- least squares method based on partial extended  neighbourhood
!        4 <-- iterative reconstruction using the least squares method
!        5 <-- iterative reconstruction using the least squares method based on the extended neighbourhood
!        6 <-- iterative reconstruction using least squares method based on partial extended  neighbourhood
if (.FALSE.) then
  imrgra = 5
endif

!===============================================================================
! Time scheme
!===============================================================================
! --- ischtp: Order of the activated time scheme
! ==========
!        1 <-- first order time scheme [default]
!        2 <-- second order time scheme
if (.FALSE.) then
  ischtp = 2
endif

!===============================================================================
! Restart period
!===============================================================================
! --- ntsuit: saving period of the restart files
! ==========
!       -2 <-- no restart at all
!       -1 <-- only at the end of the calculation
!        0 <-- default (four times during the calculation)
!       >0 <-- period
if (.FALSE.) then
  ntsuit = 100 ! every 600 timesteps
endif

return
end subroutine usipsu
