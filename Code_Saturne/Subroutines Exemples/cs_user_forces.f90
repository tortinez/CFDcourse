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

!-------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     nvar          total number of variables
!> \param[in]     nscal         total number of scalars
!> \param[in]     dt            time step (per cell)
!_______________________________________________________________________________

subroutine cs_f_user_extra_operations &
 ( nvar   , nscal  ,                                              &
   dt     )

!===============================================================================

!===============================================================================
! Module files
!===============================================================================

use paramx
use dimens, only: ndimfb
use pointe
use numvar
use optcal
use cstphy
use cstnum
use entsor
use lagpar
use lagran
use lagdim
use parall
use period
use ppppar
use ppthch
use ppincl
use mesh
use field
use field_operator
use turbomachinery

!===============================================================================

implicit none

! Arguments

integer          nvar   , nscal

double precision dt(ncelet)

! Local variables

!< [loc_var_dec]
integer          ifac, iel
integer          ii, pos, nstsave
integer          ilelt, nlelt

character(len=1000) filename, proc_rang

double precision xpre(3), xfor(3), area, vinf
double precision, dimension(:), pointer :: pressure
double precision, dimension(:,:), pointer :: forces

integer, allocatable, dimension(:) :: lstelt
!< [loc_var_dec]

!===============================================================================

!===============================================================================
! Initialization
!===============================================================================

! Allocate a temporary array for cells or interior/boundary faces selection
allocate(lstelt(max(ncel,nfac,nfabor)))

!===============================================================================
! GLOBAL EFFORTS: compute forces and pressure forces over a boundary
!===============================================================================
if (.TRUE.) then
  nstsave = 1000 ! Save interval to write file

  ! Preallocate arrays
  area = 1.0d0 * 1.0d0 ! Chord * depth
  vinf = 1.0d0         ! contains reference velocity

  ! When computations are correctly done
  if (ineedf .eq. 1) then
    ! Start writing file in first iteration
    if (ntcabs.eq.ntpabs+1) then
      if (irangp .eq. 0) then
        ! Open file as binary stream file
        open(unit=impusr(1), file="coefficients.dat", action="write", &
                             access="stream", form="unformatted")
        write(impusr(1)) nstsave, area, vinf, ro0
        flush(impusr(1))
      endif
    endif

    if (mod(ntcabs,nstsave) .eq. 0) then
      ! Get the faces in a certain boundary condition
      call getfbr('BODY', nlelt, lstelt)  ! for stress computation

      ! Get forces if post-processed
      call field_get_val_s(ivarfl(ipr), pressure) ! Pressure efforts
      call field_get_val_v(iforbr, forces) ! Efforts as computed by Code Saturne

      ! Compute the forces and save on xfor (see src/base/post_util.f90)
      xpre(1:ndim) = 0.d0
      xfor(1:ndim) = 0.d0
      do ilelt = 1, nlelt
        ifac = lstelt(ilelt)
        iel = ifabor(ifac)
        do ii = 1, ndim
          xpre(ii) = xpre(ii) + pressure(iel)*surfbo(ii,ifac)/surfbn(ifac) ! sum of all pressure forces
          xfor(ii) = xfor(ii) + forces(ii,ifac) ! sum of all forces
        enddo
      enddo

      ! If the computation is parallel, rearrange the forces
      if (irangp .ge. 0) then
        call parrsm(ndim,xfor)
        call parrsm(ndim,xpre)
      endif

      ! Finally write the results in a file
      if (irangp.eq.0) then
        ! Write result
        write(impusr(1)) ttcabs,xpre(:),xfor(:)
        flush(impusr(1)) ! Make sure that the data is written to the file
      endif
    endif ! end if mod
    ! Close the file
    if (ntcabs .eq. ntmabs) then
      if (irangp .eq. 0) then
        close(impusr(1))
      endif
    endif
  endif ! end if ineedf
endif ! end if true

! Deallocate the temporary array
deallocate(lstelt)

end subroutine cs_f_user_extra_operations
