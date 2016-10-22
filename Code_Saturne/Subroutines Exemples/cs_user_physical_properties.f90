!-------------------------------------------------------------------------------

!                      Code_Saturne version 4.0.1
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

!===============================================================================
! Purpose:
! -------

!> \file cs_user_physical_properties.f90
!> \brief Definition of physical variable laws.
!>
!> usphyv
!> \brief Definition of physical variable laws.
!>
!> \section Warning
!>
!> It is \b forbidden to modify turbulent viscosity \c visct here
!> (a specific subroutine is dedicated to that: \ref usvist)
!>
!> - icp = 1 must <b> have been specified </b>
!>    in \ref usipsu if we wish to define a variable specific heat
!>    cpro_cp (otherwise: memory overwrite).
!>
!> - the kivisl field integer key (scalar_diffusivity_id)
!>    must <b> have been specified </b>
!>    in \ref usipsu if we wish to define a variable viscosity
!>    \c viscls.
!>
!>
!> \remarks
!>  - This routine is called at the beginning of each time step
!>    Thus, <b> AT THE FIRST TIME STEP </b> (non-restart case), the only
!>    values initialized before this call are those defined
!>      - in the GUI or  \ref usipsu (cs_user_parameters.f90)
!>             - density    (initialized at \c ro0)
!>             - viscosity  (initialized at \c viscl0)
!>      - in the GUI or \ref cs_user_initialization
!>             - calculation variables (initialized at 0 by defaut
!>             or to the value given in the GUI or in \ref cs_user_initialization)
!>
!>  - We may define here variation laws for cell properties, for:
!>     - density:                                    rom    kg/m3
!>     - density at boundary faces:                  romb   kg/m3)
!>     - molecular viscosity:                        cpro_viscl  kg/(m s)
!>     - specific heat:                              cpro_cp     J/(kg degrees)
!>     - diffusivities associated with scalars:      cpro_vscalt kg/(m s)
!>
!> \b Warning: if the scalar is the temperature, cpro_vscalt corresponds
!> to its conductivity (Lambda) in W/(m K)
!>
!>
!> The types of boundary faces at the previous time step are available
!>   (except at the first time step, where arrays \c itypfb and \c itrifb have
!>   not been initialized yet)
!>
!> It is recommended to keep only the minimum necessary in this file
!>   (i.e. remove all unused example code)
!>
!>
!> \section usphyv_cell_id Cells identification
!>
!> Cells may be identified using the \ref getcel subroutine.
!> The syntax of this subroutine is described in the
!> \ref cs_user_boundary_conditions subroutine,
!> but a more thorough description can be found in the user guide.
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     nvar          total number of variables
!> \param[in]     nscal         total number of scalars
!> \param[in]     mbrom         indicator of filling of romb array
!> \param[in]     dt            time step (per cell)
!_______________________________________________________________________________

subroutine usphyv &
 ( nvar   , nscal  ,                                              &
   mbrom  ,                                                       &
   dt     )

!===============================================================================

!===============================================================================
! Module files
!===============================================================================

use paramx
use pointe
use numvar
use optcal
use cstphy
use entsor
use parall
use period
use ppppar
use ppthch
use ppincl
use field
use mesh

!===============================================================================

implicit none

! Arguments

integer          nvar   , nscal

integer          mbrom

double precision dt(ncelet)

! Local variables

integer          ivart, iel, ifac
integer          ith, iscal, ii, ifcvsl
double precision vara, varb, varc, varam, varbm, varcm, vardm
double precision                   varal, varbl, varcl, vardl
double precision                   varac, varbc
double precision xvart
double precision xbuffzone, ybuffzone

double precision, dimension(:), pointer :: viscosity
double precision, dimension(:,:), pointer :: velocity

!===============================================================================

!===============================================================================
!  Buffer zone: variable viscosity at a given zone
!  =========
!    Below, we define the buffer zone
!    Values of this property must be defined at cell centers
!  =============================================================================
if (.FALSE.) then
! --- Buffer zone start and viscosity value
xbuffzone = 15.0d0
ybuffzone = 15.0d0

! --- Variable viscosity
ivivar = 1 ! Override and allow for variable viscosity

! --- Molecular dynamic viscosity
call field_get_val_s(iprpfl(iviscl),viscosity)
! Gets the whole viscosity field

! Molecular dynamic viscosity in kg/(m.s) at cell centers
!--------------------------------------------------------
! zone    x > value
! so      gradually adapt buffer zone
do iel = 1, ncel ! loop all the cells
  ! Get the cell center position and decide whether it is inside the buffer zone
  if (xyzcen(1,iel) .GT. xbuffzone) then ! start of the buffer zone
    if (viscosity(iel) .NE. 0.d0) then ! a check for parallel runs
      viscosity(iel) = 100.0d0*viscl0 ! Set viscosity
    endif
  endif
  ! Get the cell center position and decide whether it is inside the buffer zone
  if (xyzcen(2,iel) .GT. ybuffzone) then ! start of the buffer zone
    if (viscosity(iel) .NE. 0.d0) then ! a check for parallel runs
      viscosity(iel) = 100.0d0*viscl0 ! Set viscosity
    endif
  endif
enddo
endif

!===============================================================================
return
end subroutine usphyv
