!-------------------------------------------------------------------------------

!                      Code_Saturne version 4.0.1n!                      --------------------------
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

!> \file cs_user_postprocess_var.f90
!> \brief Output additional variables on a postprocessing mesh.
!>
!> Several "automatic" postprocessing meshes may be defined:
!> - The volume mesh (ipart=-1)
!> - The boundary mesh (ipart=-2)
!> - SYRTHES coupling surface (ipart < -2)
!> - Cooling tower exchange zone meshes (ipart < -2) if 'ichrze' = 1
!>
!> Additional meshes (cells or faces) may also be defined through the GUI or
!> using the cs_user_postprocess_meshes() function from the
!> cs_user_postprocess.c file.
!>
!> This subroutine is called once for each post-processing mesh
!> (with a different value of 'ipart') for each time step at which output
!> on this mesh is active.
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ipart         number of the post-processing mesh (< 0 or > 0)
!> \param[in]     nvar          total number of variables
!> \param[in]     nscal         total number of scalars
!> \param[in]     nvlsta        number of Lagrangian statistical variables
!> \param[in]     ncelps        number of cells in post-processing mesh
!> \param[in]     nfacps        number of interior faces in post-process. mesh
!> \param[in]     nfbrps        number of boundary faces in post-process. mesh
!> \param[in]     itypps        global presence flag (0 or 1) for cells (1),
!>                              interior faces (2), or boundary faces (3) in
!>                              post-processing mesh
!> \param[in]     lstcel        list of cells in post-processing mesh
!> \param[in]     lstfac        list of interior faces in post-processing mesh
!> \param[in]     lstfbr        list of boundary faces in post-processing mesh
!_______________________________________________________________________________

subroutine usvpst &
 ( ipart  ,                                                       &
   nvar   , nscal  , nvlsta ,                                     &
   ncelps , nfacps , nfbrps ,                                     &
   itypps ,                                                       &
   lstcel , lstfac , lstfbr )

!===============================================================================

!===============================================================================

!===============================================================================
! Module files
!===============================================================================

use paramx
use pointe
use entsor
use cstnum
use cstphy
use optcal
use numvar
use parall
use period
use lagpar
use lagran
use ppppar
use ppthch
use ppincl
use radiat
use cplsat
use mesh
use field
use field_operator
use post
use cs_f_interfaces
use cs_c_bindings
use rotation
use turbomachinery

!===============================================================================

implicit none

! Arguments

integer          ipart
integer          nvar,   nscal , nvlsta
integer          ncelps, nfacps, nfbrps

integer          itypps(3)
integer          lstcel(ncelps), lstfac(nfacps), lstfbr(nfbrps)


! Local variables

integer          ntindp, f_id
integer          iel, ifac, iloc, ivar
integer          idimt, ii , jj
logical          ientla, ivarpr
integer          inc, iprev
double precision rvoid(1)

double precision, dimension(:), allocatable :: tracel
double precision, dimension(:,:,:), allocatable :: gradv

integer          intpst
data             intpst /0/
save             intpst

!===============================================================================


!===============================================================================
! Increment call counter once per time step (possibly used in some tests)
!===============================================================================

if (ipart .eq. -1) then
  intpst = intpst + 1
endif

!===============================================================================
! 1. Handle variables to output
!    MUST BE FILLED IN by the user at indicated places
!===============================================================================

! The ipart argument matches a post-processing maehs id (using the EnSight
! vocabulary; the MED and CGNS equivalents are "mesh" and "base" respectively).
! The user will have defined post-processing meshes using the GUI or the
! cs_user_postprocess_meshes() function from the cs_user_postprocess.c
! file.

! This subroutine is called once for each post-processing mesh
! (with a different value of 'ipart') for each time step at which output
! on this mesh is active. For each mesh and for all variables we wish to
! post-process here, we must define certain parameters and pass them to
! the 'post_write_var' subroutine, which is in charge of the actual output.
! These parameters are:

! namevr <-- variable name
! idimt  <-- variable dimension
!            (1: scalar, 3: vector, 6: symmetric tensor, 9: tensor)
! ientla <-- when idimt >1, this flag specifies if the array containing the
!            variable values is interlaced when ientla = .true.
!            (x1, y1, z1, x2, y2, z2, x3, y3, z3...), or non-interlaced when
!            ientla = .false. (x1, x2, x3,...,y1, y2, y3,...,z1, z2, z3,...).
! ivarpr <-- specifies if the array containing the variable is defined on
!            the "parent" mesh or locally.
!            Even if the 'ipart' post-processing mesh contains all the
!            elements of its parent mesh, their numbering may be different,
!            especially when different element types are present.
!            A local array passed as an argument to 'post_write_var' is built
!            relative to the numbering of the 'ipart' post-processing mesh.
!            To post-process a variable contained for example in the 'user'
!            array, it should first be re-ordered, as shown here:
!              do iloc = 1, ncelps
!                iel = lstcel(iloc)
!                scel(iloc) = user(iel)
!              enddo
!            An alternative option is provided, to avoid unnecessary copies:
!            an array defined on the parent mesh, such our 'user' example,
!            may be passed directly to 'post_write_var', specifying that values
!            are defined on the parent mesh instead of the post-processing mesh,
!            by setting the 'ivarpr' argument of 'post_write_var' to .true..

! Note: be cautious with variable name lengths.

! We allow up to 32 characters here, but names may be truncted depending on the
! output format.

! The name length is not limited internally, so in case of 2 variables whoses
! names differ only after the truncation character, the corresponding names will
! both appear in the ".case" file; simply renaming one of the field descriptors
! in this text file will correct the output.

! Whitespace at the beginning or the end of a line is truncated automatically.
! Depending on the format used, prohibited characters (under EnSight, characters
! (  ) ] [ + - @           ! # * ^ $ / as well as white spaces and tabulations
! are automatically replaced by the _ character.

! Examples:

!   For post-processing mesh 2, we output the velocity, pressure, and prescribed
!   temperature at boundary faces (as well as 0 on possible interior faces)

!   For post-processing mesh 1, we output all the variables usually
!   post-processed, using a more compact coding.

!   Examples given here correspond to the meshes defined in
!   cs_user_postprocess.c

if (ipart .eq. -1) then ! Variables on the main volume mesh
!===============================================================================
! Vorticity computation
!===============================================================================
  if (.TRUE.) then
    idimt = 3
    ientla = .true.
    ivarpr = .false.
    allocate(gradv(3,3,ncelet),tracel(idimt*ncelps))

    ! Compute gradient
    iprev = 0 ! Do not use previous timestep values
    inc   = 1 ! Increment
    call field_gradient_vector(ivarfl(iu), iprev, imrgra, inc, gradv)

    ! Compute vorticity
    do iloc = 1, ncelps
      iel = lstcel(iloc)
      tracel(1 + (iloc-1)*idimt) = gradv(2,3,iel) - gradv(3,2,iel) ! vorticity x
      tracel(2 + (iloc-1)*idimt) = gradv(3,1,iel) - gradv(1,3,iel) ! vorticity y
      tracel(3 + (iloc-1)*idimt) = gradv(1,2,iel) - gradv(2,1,iel) ! vorticity z
    enddo

    ! Save variables
    call post_write_var(ipart, 'Vorticity', idimt, ientla, ivarpr,  &
                        ntcabs, ttcabs, tracel, rvoid, rvoid)

    ! Deallocate variables
    deallocate(gradv,tracel)
  endif

!===============================================================================

else if (ipart .eq. -2) then ! Variables on the boundary mesh

endif ! end of test on post-processing mesh number

return

end subroutine usvpst
