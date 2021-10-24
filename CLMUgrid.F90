Program clmu2grid

   use netcdf

   IMPLICIT NONE

   INTEGER, PARAMETER :: r8 = SELECTED_REAL_KIND(12)

   REAL(r8), PARAMETER :: sdelta = 1./240.
   REAL(r8), PARAMETER :: udelta = 1./120.
   REAL(r8), PARAMETER :: fv     = -999._r8

   INTEGER, PARAMETER :: plat = 21600, plon = 43200
   INTEGER, PARAMETER :: ulat = 20880, ulon = 43200
   INTEGER, PARAMETER :: nlat = 43200, nlon = 86400
   INTEGER, PARAMETER :: rid = 33 , den_clss = 3
   INTEGER, PARAMETER :: nxo = 720, nyo = 360
   INTEGER, PARAMETER :: ns  = 2  , nr  = 2
   INTEGER, PARAMETER :: ulev= 10
   INTEGER, PARAMETER :: mon = 12
   INTEGER, PARAMETER :: npft= 16 
   
! input variables
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: tlat
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: tlon
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: hlat
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: hlats
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: hlatn
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: hlon
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: hlonw
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: hlone
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: urlat
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: nrlat
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: urlats
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: urlatn
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: urlon
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: urlonw
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: urlone
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: gfcc_tc
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: gedi_th
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: gl30_wt
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: mask
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: harea
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: uarea

   INTEGER , ALLOCATABLE, DIMENSION(:,:) :: urclss
   INTEGER , ALLOCATABLE, DIMENSION(:,:) :: urrgid

   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: umd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: uhd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: utbd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: umask

   REAL(r8), ALLOCATABLE, DIMENSION(:,:)     :: npct_urban
   REAL(r8), ALLOCATABLE, DIMENSION(:,:)     :: pct_urban
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:)   :: pct_pft
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:)   :: npct_pft
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:)   :: htop_pft
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:)   :: nhtop_pft
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: lai_pft
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: nlai_pft

   REAL(r8), DIMENSION(den_clss,rid) :: hwrcan, wtrf, wtrd, emrf, emwl
   REAL(r8), DIMENSION(den_clss,rid) :: emimrd, emperd, htrf, whc, ulevimrd 
   REAL(r8), DIMENSION(den_clss,rid) :: thrf, thwl, tbmin, tbmax
   
   REAL(r8), DIMENSION(den_clss,rid,ulev) :: cvrf, cvwl, cvimrd, &
                                             tkrf, tkwl, tkimrd
   REAL(r8), DIMENSION(den_clss,rid,ns,nr):: albrf, albwl, albimrd, albperd
   ! output variables
   INTEGER , ALLOCATABLE, DIMENSION(:,:) :: ur_clss
   INTEGER , ALLOCATABLE, DIMENSION(:,:) :: ur_rgid
   
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: latso
   REAL(r8), ALLOCATABLE, DIMENSION(:)   :: lonso
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: cnt
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: area
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: nhd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: nmd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: ntbd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: pct_tc
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: pct_urwt
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: htop_ur
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: hwr_can
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: wt_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: wt_rd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: em_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: em_wl
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: em_imrd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: em_perd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: ht_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: w_hc
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: ulev_imrd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: th_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: th_wl
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: tb_min
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: tb_max

   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: pct_tland
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: pct_wland
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: pct_hland
   REAL(r8), ALLOCATABLE, DIMENSION(:,:) :: ur_land
   
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: ur_dc
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: pct_ur
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: lai_wgt
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:) :: ur_lai
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: cv_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: cv_wl
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: cv_imrd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: tk_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: tk_wl
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:) :: tk_imrd

   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: alb_rf
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: alb_wl
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: alb_imrd
   REAL(r8), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: alb_perd

   ! variable ids
   INTEGER :: ncid, uhd_vid, umd_vid, utbd_vid, htop_urvid
   INTEGER :: lat_vid, lon_vid, lat_dimid, lon_dimid, ns_dimid, nr_dimid, ulev_dimid
   INTEGER :: ns_vid, nr_vid, pct_tcvid, pct_urvid, pct_urwtvid, ulev_vid
   INTEGER :: pftvid, laivid, maskid, umaskid, t_pftvid, ur_laivid, htop_pftvid
   INTEGER :: hlat_vid, hlon_vid, gfcc_tcvid, gedi_thvid, gl30_wtvid
   INTEGER :: urlat_vid, urlon_vid, ur_clssvid, ur_rgvid, hwr_canvid
   INTEGER :: wt_rfvid, wt_rdvid, em_rfvid, em_wlvid, em_imrdvid, em_perdvid
   INTEGER :: ht_rfvid, whcvid, cv_rfvid, cv_wlvid, cv_imrdvid, ulev_imrdvid
   INTEGER :: th_rfvid, th_wlvid, tbminvid, tbmaxvid
   INTEGER :: tk_rfvid, tk_wlvid, tk_imrdvid
   INTEGER :: alb_rfvid, alb_imrdvid, alb_perdvid, alb_wlvid
   INTEGER :: uxid, uyid, upftvid, mon_dimid, den_dimid
   INTEGER :: den_vid, mon_vid, ur_landvid

   REAL(r8) :: pi, deg2rad, re, dx, dy, sumarea, sumur, sumpct
   REAL(r8) :: dll, delta, fac
   !REAL(r8) :: lone1(nxy1), lonw1(nxy1), latn1(nxy1), lats1(nxy1)
   REAL(r8) :: lone2(nxo), lonw2(nxo), latn2(nyo), lats2(nyo)
   REAL(r8) :: lone3(nxo), lonw3(nxo), latn3(nyo), lats3(nyo)

   INTEGER  :: i, j, k, io, jo, m, ii, jj

   REAL(r8) :: wgt(3)
   INTEGER  :: n_ns(2), n_nr(2), n_den(3), n_ulev(10), n_mon(12)
   INTEGER  :: XY2D(2), XY3D(3), XY4D(4), UR3D(3), UL3D(3), XY5D(5)

   pi = 4.*atan(1.)
   deg2rad = pi/180.
   re = 6.37122e6 * 0.001

   allocate( tlat  (nlat) )
   allocate( tlon  (nlon) )
   allocate( hlat  (nlat) )
   allocate( hlats (nlat) )
   allocate( hlatn (nlat) )
   allocate( hlon  (nlon) )
   allocate( hlonw (nlon) )
   allocate( hlone (nlon) )
   allocate( urlat (ulat) )
   allocate( nrlat (ulat) )
   allocate( urlats(ulat) )
   allocate( urlatn(ulat) )
   allocate( urlon (ulon) )
   allocate( urlonw(ulon) )
   allocate( urlone(ulon) )
   allocate( harea  (nlon, nlat) )
   allocate( mask   (nlon, nlat) )
   allocate( gfcc_tc(nlon, nlat) )
   allocate( gedi_th(nlon, nlat) )
   allocate( gl30_wt(nlon, nlat) )

   allocate( uarea  (ulon, ulat) )
   allocate( umask  (plon, plat) )
   allocate( umd    (ulon, ulat) )
   allocate( nmd    (ulon, ulat) )
   allocate( uhd    (ulon, ulat) )
   allocate( nhd    (ulon, ulat) )
   allocate( utbd   (ulon, ulat) )
   allocate( ntbd   (ulon, ulat) )
   allocate( urclss (plon, plat) )
   allocate( urrgid (plon, plat) )

   allocate( latso     (nyo) )
   allocate( lonso     (nxo) )
   allocate( area      (nxo, nyo) )
   allocate( pct_urban (nxo, nyo) )
   allocate( npct_urban(nxo, nyo) )
   allocate( htop_pft  (nxo, nyo, npft     ) )
   allocate( nhtop_pft (nxo, nyo, npft     ) )
   allocate( pct_pft   (nxo, nyo, npft     ) )
   allocate( npct_pft  (nxo, nyo, npft     ) )
   allocate( lai_pft   (nxo, nyo, npft, mon) )
   allocate( nlai_pft  (nxo, nyo, npft, mon) )
   allocate( ur_dc     (nxo, nyo, den_clss ) )
   allocate( pct_ur    (nxo, nyo, den_clss ) )

   allocate( cnt      (nxo, nyo) )
   allocate( pct_tc   (nxo, nyo) )
   allocate( pct_tland(nxo, nyo) )
   allocate( pct_wland(nxo, nyo) )
   allocate( pct_hland(nxo, nyo) )
   allocate( pct_urwt (nxo, nyo) )
   allocate( htop_ur  (nxo, nyo) )
   allocate( ur_land  (nxo, nyo) )
   allocate( ur_clss  (nxo, nyo) )
   allocate( ur_rgid  (nxo, nyo) )
   allocate( ur_lai   (nxo, nyo, mon) )
   allocate( lai_wgt  (nxo, nyo, mon) )
   allocate( hwr_can  (nxo, nyo, den_clss) )
   allocate( wt_rf    (nxo, nyo, den_clss) )
   allocate( wt_rd    (nxo, nyo, den_clss) )
   allocate( em_rf    (nxo, nyo, den_clss) )
   allocate( em_wl    (nxo, nyo, den_clss) )
   allocate( em_imrd  (nxo, nyo, den_clss) )
   allocate( em_perd  (nxo, nyo, den_clss) )
   allocate( ht_rf    (nxo, nyo, den_clss) )
   allocate( w_hc     (nxo, nyo, den_clss) )
   allocate( ulev_imrd(nxo, nyo, den_clss) )
   allocate( th_rf    (nxo, nyo, den_clss) )
   allocate( th_wl    (nxo, nyo, den_clss) )
   allocate( tb_min   (nxo, nyo, den_clss) )
   allocate( tb_max   (nxo, nyo, den_clss) )
   allocate( cv_rf    (nxo, nyo, den_clss, ulev) )
   allocate( cv_wl    (nxo, nyo, den_clss, ulev) )
   allocate( cv_imrd  (nxo, nyo, den_clss, ulev) )
   allocate( tk_rf    (nxo, nyo, den_clss, ulev) )
   allocate( tk_wl    (nxo, nyo, den_clss, ulev) )
   allocate( tk_imrd  (nxo, nyo, den_clss, ulev) )
   allocate( alb_rf   (nxo, nyo, den_clss, ns, nr) )
   allocate( alb_wl   (nxo, nyo, den_clss, ns, nr) )
   allocate( alb_imrd (nxo, nyo, den_clss, ns, nr) )
   allocate( alb_perd (nxo, nyo, den_clss, ns, nr) )

   ! initialization
   cnt      (:,:)   = 0.
   pct_tc   (:,:)   = 0.
   pct_urwt (:,:)   = 0.
   htop_ur  (:,:)   = 0.
   ur_land  (:,:)   = 0.
   ur_dc    (:,:,:) = 0.
   pct_tland(:,:)   = 0.
   pct_wland(:,:)   = 0.
   pct_hland(:,:)   = 0.
   utbd     (:,:)   = 0.
   uhd      (:,:)   = 0.
   umd      (:,:)   = 0.
   ur_lai   (:,:,:) = 0.
   lai_wgt  (:,:,:) = 0.

   ur_rgid(:,:) = 0
   ur_clss(:,:) = 0


   ! process global 500m raw data
   print *, "*** processing 500m raw data ***"
   print *, "    reading data"
   CALL check( nf90_open("SRF_500m.nc", nf90_nowrite, ncid) )

   !CALL check( nf90_inq_varid(ncid, "lat"  , hlat_vid       ) )
   !CALL check( nf90_inq_varid(ncid, "lon"  , hlon_vid       ) )
   CALL check( nf90_inq_varid(ncid, "PCT_Tree"   , gfcc_tcvid     ) )
   CALL check( nf90_inq_varid(ncid, "Hgt_Tree"   , gedi_thvid     ) )
   CALL check( nf90_inq_varid(ncid, "PCT_Water"  , gl30_wtvid     ) )

   !CALL check( nf90_get_var(ncid, hlat_vid  , hlat   ) )
   !CALL check( nf90_get_var(ncid, hlon_vid  , hlon   ) )
   CALL check( nf90_get_var(ncid, gfcc_tcvid, gfcc_tc) )
   CALL check( nf90_get_var(ncid, gedi_thvid, gedi_th) )
   CALL check( nf90_get_var(ncid, gl30_wtvid, gl30_wt) )

   CALL check( nf90_close(ncid) )

   ! calculate the edge of small grids
   DO i = 1, nlat
      hlats(i) = -90. + (i-1)*sdelta
      hlatn(i) = -90. + i*sdelta
      hlat (i) = -90. + i*sdelta - 0.5*sdelta
   ENDDO
   
   DO i = 1, nlon
      hlonw(i) = -180. + (i-1)*sdelta
      hlone(i) = -180. + i*sdelta
      hlon (i) = -180. + i*sdelta - 0.5*sdelta
   ENDDO
   
   DO i = 1, nlat
      dx = (hlone(1)-hlonw(1))*deg2rad
      dy = sin(hlatn(i)*deg2rad) - sin(hlats(i)*deg2rad)
      harea(:,i) = dx*dy*re*re
   ENDDO

   delta = (360./nxo)*1._r8
   DO i = 1, 360
      lats2(i) = -90. + (i-1)*delta
      latn2(i) = -90 + i*delta
   ENDDO

   DO i = 1, 720
      lonw2(i) = -180. + (i-1)*delta
      lone2(i) = -180. + i*delta
   ENDDO

   print *, "    aggregating data"
!$OMP PARALLEL DO NUM_THREADS(92) &
!$OMP PRIVATE(i,j,io,jo)
   DO i = 1,nlat
      DO j = 1,nlon
         
      ! calculate io, jo
         jo = int((hlon(j) +180.)/delta)
         io = int((hlat(i) + 90.)/delta)

      ! for each model grid for aggregation
         IF (gfcc_tc(j,i) .ne. -999._r8) THEN
            pct_tc   (jo,io) = pct_tc   (jo,io) + gfcc_tc(j,i)*harea(j,i)
            pct_tland(jo,io) = pct_tland(jo,io) + harea(j,i)
         ENDIF

         IF (gl30_wt(j,i) .ne. -999._r8) THEN 
            pct_urwt (jo,io) = pct_urwt (jo,io) + gl30_wt(j,i)*harea(j,i)
            pct_wland(jo,io) = pct_wland(jo,io) + harea(j,i)
         ENDIF

         IF (gfcc_tc(j,i) .ne. -999._r8) THEN
            IF (gedi_th(j,i) .ge. 3) THEN
               htop_ur  (jo,io) = htop_ur(jo,io) + gedi_th(j,i)*gfcc_tc(j,i)*harea(j,i)
               pct_hland(jo,io) = pct_hland(jo,io) + gfcc_tc(j,i)*harea(j,i)
            ELSE
               gedi_th(j,i) = 0
               htop_ur  (jo,io) = htop_ur(jo,io) + gedi_th(j,i)*gfcc_tc(j,i)*harea(j,i)
               pct_hland(jo,io) = pct_hland(jo,io) + gfcc_tc(j,i)*harea(j,i)
            ENDIF
         ENDIF 

      ENDDO
   ENDDO
!$OMP END PARALLEL DO

   ! aggregating on model grid for tree cove, water and tree height forest
   DO i = 1,nyo
      DO j = 1,nxo
         IF (pct_tland(j,i) > 0.) THEN
            pct_tc(j,i) = pct_tc  (j,i) / pct_tland(j,i)! * 100.
         ENDIF
            IF (pct_tc(j,i) .gt. 100._r8) THEN
               print *, pct_tc(j,i)
            ENDIF
         
         IF (pct_wland(j,i) > 0.) THEN
            pct_urwt(j,i) = pct_urwt(j,i) / pct_wland(j,i)! * 100.
         ENDIF

         IF (pct_hland(j,i) > 0.) THEN
            htop_ur(j,i) = htop_ur(j,i) / pct_hland(j,i)
         ENDIF

      ENDDO  
   ENDDO
   
   ! calculate output lat/lon
   DO i = 1, nxo
      lonso(i) = -180. + i*delta - 0.5*delta
   ENDDO
   DO i = 1, nyo
      latso(i) =  -90. + i*delta - 0.5*delta
   ENDDO
   print *, "********************************"

   ! aggregating 1km urban data to model grid
   print *, "*** processing CLM 1km urban data ***"

   print *, "    reading 1km urban properties data"
   CALL check( nf90_open("urban_properties_data.1km.210726-121520.nc", nf90_nowrite, ncid) )

   !CALL check( nf90_inq_varid(ncid, "LAT"                , urlat_vid   ) )
   !CALL check( nf90_inq_varid(ncid, "LON"                , urlon_vid   ) )
   CALL check( nf90_inq_varid(ncid, "LANDMASK"           , umaskid     ) )
   CALL check( nf90_inq_varid(ncid, "URBAN_DENSITY_CLASS", ur_clssvid  ) )
   CALL check( nf90_inq_varid(ncid, "REGION_ID"          , ur_rgvid    ) )
   CALL check( nf90_inq_varid(ncid, "CANYON_HWR"         , hwr_canvid  ) )
   CALL check( nf90_inq_varid(ncid, "WTLUNIT_ROOF"       , wt_rfvid    ) )
   CALL check( nf90_inq_varid(ncid, "WTROAD_PERV"        , wt_rdvid    ) )
   CALL check( nf90_inq_varid(ncid, "EM_ROOF"            , em_rfvid    ) )
   CALL check( nf90_inq_varid(ncid, "EM_WALL"            , em_wlvid    ) )
   CALL check( nf90_inq_varid(ncid, "EM_IMPROAD"         , em_imrdvid  ) )
   CALL check( nf90_inq_varid(ncid, "EM_PERROAD"         , em_perdvid  ) )
   CALL check( nf90_inq_varid(ncid, "ALB_ROOF"           , alb_rfvid   ) )
   CALL check( nf90_inq_varid(ncid, "ALB_WALL"           , alb_wlvid   ) )
   CALL check( nf90_inq_varid(ncid, "ALB_IMPROAD"        , alb_imrdvid ) )
   CALL check( nf90_inq_varid(ncid, "ALB_PERROAD"        , alb_perdvid ) )
   CALL check( nf90_inq_varid(ncid, "HT_ROOF"            , ht_rfvid    ) )
   CALL check( nf90_inq_varid(ncid, "WIND_HGT_CANYON"    , whcvid      ) )
   CALL check( nf90_inq_varid(ncid, "TK_ROOF"            , tk_rfvid    ) )
   CALL check( nf90_inq_varid(ncid, "TK_WALL"            , tk_wlvid    ) )
   CALL check( nf90_inq_varid(ncid, "TK_IMPROAD"         , tk_imrdvid  ) )
   CALL check( nf90_inq_varid(ncid, "CV_ROOF"            , cv_rfvid    ) )
   CALL check( nf90_inq_varid(ncid, "CV_WALL"            , cv_wlvid    ) )
   CALL check( nf90_inq_varid(ncid, "CV_IMPROAD"         , cv_imrdvid  ) )
   CALL check( nf90_inq_varid(ncid, "NLEV_IMPROAD"       , ulev_imrdvid) )
   CALL check( nf90_inq_varid(ncid, "THICK_ROOF"         , th_rfvid    ) )
   CALL check( nf90_inq_varid(ncid, "THICK_WALL"         , th_wlvid    ) )
   CALL check( nf90_inq_varid(ncid, "T_BUILDING_MIN"     , tbminvid    ) )
   CALL check( nf90_inq_varid(ncid, "T_BUILDING_MAX"     , tbmaxvid    ) )

   !CALL check( nf90_get_var(ncid, urlat_vid   , urlat    ) )
   !CALL check( nf90_get_var(ncid, urlon_vid   , urlon    ) )
   CALL check( nf90_get_var(ncid, umaskid     , umask   ) )
   CALL check( nf90_get_var(ncid, ur_clssvid  , urclss  ) )
   CALL check( nf90_get_var(ncid, ur_rgvid    , urrgid  ) )
   CALL check( nf90_get_var(ncid, hwr_canvid  , hwrcan  ) )
   CALL check( nf90_get_var(ncid, wt_rfvid    , wtrf    ) )
   CALL check( nf90_get_var(ncid, wt_rdvid    , wtrd    ) )
   CALL check( nf90_get_var(ncid, em_rfvid    , emrf    ) )
   CALL check( nf90_get_var(ncid, em_wlvid    , emwl    ) )
   CALL check( nf90_get_var(ncid, em_imrdvid  , emimrd  ) )
   CALL check( nf90_get_var(ncid, em_perdvid  , emperd  ) )
   CALL check( nf90_get_var(ncid, alb_rfvid   , albrf   ) )
   CALL check( nf90_get_var(ncid, alb_wlvid   , albwl   ) )
   CALL check( nf90_get_var(ncid, alb_imrdvid , albimrd ) )
   CALL check( nf90_get_var(ncid, alb_perdvid , albperd ) )
   CALL check( nf90_get_var(ncid, ht_rfvid    , htrf    ) )
   CALL check( nf90_get_var(ncid, whcvid      , whc     ) )
   CALL check( nf90_get_var(ncid, tk_rfvid    , tkrf    ) )
   CALL check( nf90_get_var(ncid, tk_wlvid    , tkwl    ) )
   CALL check( nf90_get_var(ncid, tk_imrdvid  , tkimrd  ) )
   CALL check( nf90_get_var(ncid, cv_rfvid    , cvrf    ) )
   CALL check( nf90_get_var(ncid, cv_wlvid    , cvwl    ) )
   CALL check( nf90_get_var(ncid, cv_imrdvid  , cvimrd  ) )
   CALL check( nf90_get_var(ncid, ulev_imrdvid, ulevimrd) )
   CALL check( nf90_get_var(ncid, th_rfvid    , thrf    ) )
   CALL check( nf90_get_var(ncid, th_wlvid    , thwl    ) )
   CALL check( nf90_get_var(ncid, tbminvid    , tbmin   ) )
   CALL check( nf90_get_var(ncid, tbmaxvid    , tbmax   ) )

   CALL check( nf90_close(ncid) )

   print *, "    reading 1km urband class data"

   CALL check( nf90_open("hd_flip.nc", nf90_nowrite, ncid) )

   CALL check( nf90_inq_varid(ncid, "hd"     , uhd_vid  ) )
   CALL check( nf90_inq_varid(ncid, "lat"    , urlat_vid) )
   CALL check( nf90_inq_varid(ncid, "lon"    , urlon_vid) )

   CALL check( nf90_get_var(ncid, urlat_vid, urlat) )
   CALL check( nf90_get_var(ncid, urlon_vid, urlon) )
   CALL check( nf90_get_var(ncid, uhd_vid  , uhd  ) )

   CALL check( nf90_close(ncid) )

   CALL check( nf90_open("md_flip.nc", nf90_nowrite, ncid) )

   CALL check( nf90_inq_varid(ncid, "md" , umd_vid) )

   CALL check( nf90_get_var(ncid, umd_vid, umd    ) )

   CALL check( nf90_close(ncid) )

   CALL check( nf90_open("td_flip.nc", nf90_nowrite, ncid) )

   CALL check( nf90_inq_varid(ncid, "tbd" , utbd_vid) )

   CALL check( nf90_get_var(ncid, utbd_vid, utbd    ) )

   CALL check( nf90_close(ncid) )

   print *, "    reading PFTs data"

   CALL check( nf90_open("global_0.5x0.5.MOD2005_V4.5.nc", nf90_nowrite, ncid) )

   CALL check( nf90_inq_varid(ncid, "PCT_PFT"        , pftvid     ) )
   CALL check( nf90_inq_varid(ncid, "MONTHLY_PFT_LAI", laivid     ) )
   !CALL check( nf90_inq_varid(ncid, "MONTHLY_PFT_SAI", saivid     ) )
   CALL check( nf90_inq_varid(ncid, "PCT_URBAN"      , upftvid    ) )
   CALL check( nf90_inq_varid(ncid, "HTOP_PFT"       , htop_pftvid) )

   CALL check( nf90_get_var(ncid, pftvid     , npct_pft  ) )
   CALL check( nf90_get_var(ncid, upftvid    , npct_urban) )
   !CALL check( nf90_get_var(ncid, saivid     , sai_pft  ) )
   CALL check( nf90_get_var(ncid, laivid     , nlai_pft  ) )
   CALL check( nf90_get_var(ncid, htop_pftvid, nhtop_pft ) )

   CALL check( nf90_close(ncid) )

   ! data flip
   ! 调整PFTs数据与1km数据一致 pft(lon,n,npft) = pft(lon,lat-n+1,npft)
   DO i = 1, nyo
      DO j = 1, nxo
         k = nyo - i + 1
         pct_urban(j,i)     = npct_urban(j,k)
         htop_pft (j,i,:)   = nhtop_pft(j,k,:)
         pct_pft  (j,i,:)   = npct_pft(j,k,:)
         lai_pft  (j,i,:,:) = nlai_pft(j,k,:,:)
      ENDDO
   ENDDO

   ! calculate the edge of small grids
   DO i = 1, 20800
      urlats(i) = urlat (i) - sdelta
      urlatn(i) = urlats(i) + udelta
      !urlatn(i) = urlat(i) + i*udelta
   ENDDO
   
   DO i = 1, 43200
      urlonw(i) = urlon (i) - sdelta
      urlone(i) = urlonw(i) + udelta
      !urlone(i) = urlon(i) + i*udelta
   ENDDO
   
   DO i = 1, 20800
      dx = (urlone(1)-urlonw(1))*deg2rad
      dy = sin(urlatn(i)*deg2rad) - sin(urlats(i)*deg2rad)
      uarea(:,i) = dx*dy*re*re
   ENDDO

   DO i = 1, nyo
      lats3(i) = -180 + (i-1)*delta
      latn3(i) = -180 + i*delta
   ENDDO

   DO i = 1, nxo
      lonw3(i) = -90 + (i-1)*delta
      lone3(i) = -90 + i*delta
   ENDDO

   print *, "    aggregating urban class data"
!$OMP PARALLEL DO NUM_THREADS(92) &
!$OMP PRIVATE(i,j,io,jo)
   DO i = 1, 20880
      DO j = 1, 43200
         ! calculate io, jo
         io = int((urlat(i)+ 90.)/delta)
         jo = int((urlon(j)+180.)/delta)

         IF (utbd(j,i) .ge. 0) THEN
            ur_dc(jo,io,1) = ur_dc(jo,io,1) + utbd(j,i)*uarea(j,i)
            ur_land(jo,io) = ur_land(jo,io) + uarea(j,i)*1.
         ENDIF

         IF (uhd(j,i) .ne. -128) THEN
            ur_dc(jo,io,2) = ur_dc(jo,io,2) + uhd (j,i)*uarea(j,i)
         ENDIF

         IF (umd(j,i) .ge. 0) THEN
            ur_dc(jo,io,3) = ur_dc(jo,io,3) + umd (j,i)*uarea(j,i)
         ENDIF

      ENDDO
   ENDDO
!$OMP END PARALLEL DO

   !CALL check( nf90_create("urcheck.nc", nf90_netcdf4, ncid) )

   !CALL check( nf90_def_dim(ncid, "lat", nyo, lat_dimid     ) )
   !CALL check( nf90_def_dim(ncid, "lon", nxo, lon_dimid     ) )
   !CALL check( nf90_def_dim(ncid, "den", den_clss, den_dimid) )

   !CALL check( nf90_def_var(ncid, "pct_ur", nf90_float, (/lon_dimid,lat_dimid,den_dimid/), pct_urvid) )
   !CALL check( nf90_inq_varid(ncid, "pct_ur", pct_urvid) )
   !CALL check( nf90_put_var(ncid, pct_urvid, ur_dc     ) )

   !CALL check( nf90_def_var(ncid, "ur_land", nf90_float, (/lon_dimid,lat_dimid/), ur_landvid) )
   !CALL check( nf90_inq_varid(ncid, "ur_land", ur_landvid) )
   !CALL check( nf90_put_var(ncid, ur_landvid, ur_land    ) )

   !CALL check( nf90_close(ncid) )

   DO i = 1, nyo
      DO j = 1, nxo
         !
         IF (ur_land(j,i) > 0.) THEN
            !
            DO k = 1, 3
               ur_dc (j,i,K) = ur_dc(j,i,K) / ur_land(j,i) !* 100._r8
            ENDDO

            sumur = ur_dc(j,i,1) + ur_dc(j,i,2) + ur_dc(j,i,3)
            IF (sumur > 0.) THEN
               !ur_dc (j,i,1:3) = ur_dc(j,i,1:3)/sumur
               wgt   (1)     = ur_dc(j,i,1) / sumur
               wgt   (2)     = ur_dc(j,i,2) / sumur
               wgt   (3)     = ur_dc(j,i,3) / sumur
               pct_ur(j,i,1) = pct_urban(j,i) * wgt(1)!ur_dc(j,i,1)
               pct_ur(j,i,2) = pct_urban(j,i) * wgt(2)!ur_dc(j,i,2)
               pct_ur(j,i,3) = pct_urban(j,i) * wgt(3)!ur_dc(j,i,3)
            ENDIF
         ENDIF
      
      ENDDO
   ENDDO
  
   print *, "    assigning building properites for model grid"
   DO i = 1, nyo
      DO j = 1, nxo
         ! assign building properties in model resolution
         io = int((latso(i)+ 90+delta/2)/udelta)
         jo = int((lonso(j)+180+delta/2)/udelta)
         
         ur_rgid(j,i) = urrgid(jo,io)
         ur_clss(j,i) = urclss(jo,io)

         uxid = ur_rgid(j,i)
         uyid = ur_clss(j,i)

         ! 
         DO k = 1, 3
            hwr_can  (j,i,k) = hwrcan  (k,uxid)
            wt_rf    (j,i,k) = wtrf    (k,uxid)
            wt_rd    (j,i,k) = wtrd    (k,uxid)
            em_rf    (j,i,k) = emrf    (k,uxid)
            em_wl    (j,i,k) = emwl    (k,uxid)
            em_imrd  (j,i,k) = emimrd  (k,uxid)
            em_perd  (j,i,k) = emperd  (uyid,uxid)
            ulev_imrd(j,i,k) = ulevimrd(k,uxid)
            th_rf    (j,i,k) = thrf    (k,uxid)
            th_wl    (j,i,k) = thwl    (k,uxid)
            tb_min   (j,i,k) = tbmin   (k,uxid)
            tb_max   (j,i,k) = tbmax   (k,uxid)
            ht_rf    (j,i,k) = htrf    (k,uxid)
            w_hc     (j,i,k) = whc     (k,uxid)

            alb_rf  (j,i,k,:,:) = albrf  (k,uxid,:,:)
            alb_wl  (j,i,k,:,:) = albwl  (k,uxid,:,:)
            alb_imrd(j,i,k,:,:) = albimrd(k,uxid,:,:)
            alb_perd(j,i,k,:,:) = albperd(k,uxid,:,:)
         
            tk_rf  (j,i,k,:) = tkrf  (k,uxid,:)
            tk_wl  (j,i,k,:) = tkwl  (k,uxid,:)
            tk_imrd(j,i,k,:) = tkimrd(k,uxid,:)
            cv_rf  (j,i,k,:) = cvrf  (k,uxid,:)
            cv_wl  (j,i,k,:) = cvwl  (k,uxid,:)
            cv_imrd(j,i,k,:) = cvimrd(k,uxid,:)
         ENDDO
     
         ! calculate urban lai
         !-----------------------------------------------------
         !             ___npft
         ! urban_lai = \   pct_pft*pft_lai*(htop_gedi/htop_pft)
         !             /__ 1
         !-----------------------------------------------------      
         DO m = 2, npft
            IF (htop_pft(j,i,m) > 0) THEN
                fac = htop_ur(j,i) / htop_pft(j,i,m)
                DO k = 1, 12       
                   ur_lai (j,i,k) = ur_lai(j,i,k)+pct_pft(j,i,m)*lai_pft(j,i,m,k)*min(fac,1.)/100.
                   lai_wgt(j,i,k) = lai_wgt(j,i,k) + pct_pft(j,i,m)*lai_pft(j,i,m,k)
                ENDDO
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   print *, "*****************************************"

   CALL check( nf90_create("colm_urban_data.nc", NF90_NETCDF4, ncid) )

   CALL check( nf90_def_dim(ncid, "lat"     , nyo     , lat_dimid ) )
   CALL check( nf90_def_dim(ncid, "lon"     , nxo     , lon_dimid ) )
   CALL check( nf90_def_dim(ncid, "numsolar", ns      , ns_dimid  ) )
   CALL check( nf90_def_dim(ncid, "numrad"  , nr      , nr_dimid  ) )
   CALL check( nf90_def_dim(ncid, "ulev"    , ulev    , ulev_dimid) )
   CALL check( nf90_def_dim(ncid, "density" , den_clss, den_dimid ) )
   CALL check( nf90_def_dim(ncid, "month"   , mon     , mon_dimid ) )

   CALL check( nf90_def_var(ncid, "lat"     , NF90_FLOAT, lat_dimid , lat_vid ) )
   CALL check( nf90_def_var(ncid, "lon"     , NF90_FLOAT, lon_dimid , lon_vid ) )
   CALL check( nf90_def_var(ncid, "numsolar", NF90_INT  , ns_dimid  , ns_vid  ) )
   CALL check( nf90_def_var(ncid, "numrad"  , NF90_INT  , nr_dimid  , nr_vid  ) )
   CALL check( nf90_def_var(ncid, "ulev"    , NF90_INT  , ulev_dimid, ulev_vid) )
   CALL check( nf90_def_var(ncid, "density" , NF90_INT  , den_dimid , den_vid ) )
   CALL check( nf90_def_var(ncid, "month"   , NF90_INT  , mon_dimid , mon_vid ) )

   CALL check( nf90_put_att(ncid, lat_vid , "long_name", "Latitude"        ) )
   CALL check( nf90_put_att(ncid, lat_vid , "units"    , "degrees_north"   ) )
   CALL check( nf90_put_att(ncid, lon_vid , "long_name", "Longitude"       ) )
   CALL check( nf90_put_att(ncid, lon_vid , "units"    , "degrees_east"    ) )
   CALL check( nf90_put_att(ncid, ns_vid  , "long_name", "solar"           ) )
   CALL check( nf90_put_att(ncid, ns_vid  , "units"    , "1-dir,2-diff"    ) )
   CALL check( nf90_put_att(ncid, nr_vid  , "long_name", "radiation_band"  ) )
   CALL check( nf90_put_att(ncid, nr_vid  , "units"    , "1-vis,2-nir"     ) )
   CALL check( nf90_put_att(ncid, ulev_vid, "long_name", "urban_layer"     ) )
   CALL check( nf90_put_att(ncid, ulev_vid, "units"    , "urban_layer"     ) )
   CALL check( nf90_put_att(ncid, den_vid , "long_name", "urban_density"   ) )
   CALL check( nf90_put_att(ncid, den_vid , "units"    , "1-tbd,2-hd,3-md" ) )
   CALL check( nf90_put_att(ncid, mon_vid , "long_name", "month"           ) )
   CALL check( nf90_put_att(ncid, mon_vid , "units"    , "month"           ) )

   DO i = 1, 2
      n_ns(i) = i
      n_nr(i) = i
   ENDDO

   DO i = 1, 3
      n_den(i) = i
   ENDDO
   
   DO i = 1, 10
      n_ulev(i) = i
   ENDDO

   DO i = 1, 12
      n_mon(i) = i
   ENDDO

   XY2D = (/ lon_dimid, lat_dimid /)
   XY3D = (/ lon_dimid, lat_dimid, den_dimid /)
   CALL check( nf90_def_var(ncid, "pct_urtc"       , NF90_FLOAT, XY2D, pct_tcvid  ) )
   CALL check( nf90_def_var(ncid, "pct_urwt"       , NF90_FLOAT, XY2D, pct_urwtvid) )
   CALL check( nf90_def_var(ncid, "htop_ur"        , NF90_FLOAT, XY2D, htop_urvid ) )
   !CALL check( nf90_def_var(ncid, "lai_ur"         , NF90_FLOAT, XY2D, ur_laivid ) )
   CALL check( nf90_def_var(ncid, "REGION_ID"      , NF90_INT  , XY3D, ur_rgvid   ) )
   CALL check( nf90_def_var(ncid, "CANYON_HWR"     , NF90_FLOAT, XY3D, hwr_canvid ) )
   CALL check( nf90_def_var(ncid, "WTLUNIT_ROOF"   , NF90_FLOAT, XY3D, wt_rfvid   ) )
   CALL check( nf90_def_var(ncid, "WTROAD_PERV"    , NF90_FLOAT, XY3D, wt_rdvid   ) )
   CALL check( nf90_def_var(ncid, "EM_ROOF"        , NF90_FLOAT, XY3D, em_rfvid   ) )
   CALL check( nf90_def_var(ncid, "EM_WALL"        , NF90_FLOAT, XY3D, em_wlvid   ) )
   CALL check( nf90_def_var(ncid, "EM_IMPROAD"     , NF90_FLOAT, XY3D, em_imrdvid ) )
   CALL check( nf90_def_var(ncid, "EM_PERROAD"     , NF90_FLOAT, XY3D, em_perdvid ) )
   CALL check( nf90_def_var(ncid, "HT_ROOF"        , NF90_FLOAT, XY3D, ht_rfvid   ) )
   CALL check( nf90_def_var(ncid, "WIND_HGT_CANYON", NF90_FLOAT, XY3D, whcvid     ) )
   CALL check( nf90_def_var(ncid, "NLEV_IMPROAD"   , NF90_FLOAT, XY3D, ulev_vid   ) )
   CALL check( nf90_def_var(ncid, "THICK_ROOF"     , NF90_FLOAT, XY3D, th_rfvid   ) )
   CALL check( nf90_def_var(ncid, "THICK_WALL"     , NF90_FLOAT, XY3D, th_wlvid   ) )
   CALL check( nf90_def_var(ncid, "T_BUILDING_MIN" , NF90_FLOAT, XY3D, tbminvid   ) )
   CALL check( nf90_def_var(ncid, "T_BUILDING_MAX" , NF90_FLOAT, XY3D, tbmaxvid   ) )

   XY4D = (/ lon_dimid, lat_dimid, den_dimid, ulev_dimid /)
   UR3D = (/ lon_dimid, lat_dimid, den_dimid  /)
   CALL check( nf90_def_var(ncid, "pct_urban" , NF90_FLOAT, UR3D, pct_urvid ) )
   CALL check( nf90_def_var(ncid, "TK_ROOF"   , NF90_FLOAT, XY4D, tk_rfvid  ) )
   CALL check( nf90_def_var(ncid, "TK_WALL"   , NF90_FLOAT, XY4D, tk_wlvid  ) )
   CALL check( nf90_def_var(ncid, "TK_IMPROAD", NF90_FLOAT, XY4D, tk_imrdvid) )
   CALL check( nf90_def_var(ncid, "CV_ROOF"   , NF90_FLOAT, XY4D, cv_rfvid  ) )
   CALL check( nf90_def_var(ncid, "CV_WALL"   , NF90_FLOAT, XY4D, cv_wlvid  ) )
   CALL check( nf90_def_var(ncid, "CV_IMPROAD", NF90_FLOAT, XY4D, cv_imrdvid) )

   UL3D = (/ lon_dimid, lat_dimid, mon_dimid /)
   CALL check( nf90_def_var(ncid, "urban_lai", NF90_FLOAT, UL3D, ur_laivid ) )

   XY5D = (/ lon_dimid, lat_dimid, den_dimid, ns_dimid, nr_dimid /)
   CALL check( nf90_def_var(ncid, "ALB_ROOF"   , NF90_FLOAT, XY5D, alb_rfvid   ) )
   CALL check( nf90_def_var(ncid, "ALB_WALL"   , NF90_FLOAT, XY5D, alb_wlvid   ) )
   CALL check( nf90_def_var(ncid, "ALB_IMPROAD", NF90_FLOAT, XY5D, alb_imrdvid ) )
   CALL check( nf90_def_var(ncid, "ALB_PERROAD", NF90_FLOAT, XY5D, alb_perdvid ) )

   CALL check( nf90_put_att(ncid, pct_urvid , "units"     , "%"                                          ) )
   CALL check( nf90_put_att(ncid, pct_urvid , "long_name" , "urban_density_cover"                        ) )
   !CALL check( nf90_put_att(ncid, pct_urvid , "_FillValue", fv                   ) )

   CALL check( nf90_put_att(ncid, gfcc_tcvid, "units"     , "%"                                          ) )
   CALL check( nf90_put_att(ncid, gfcc_tcvid, "long_name" , "urban_tree_cover"                           ) )
   !CALL check( nf90_put_att(ncid, gfcc_tcvid, "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, gedi_thvid, "units"     , "meters"                                     ) )
   CALL check( nf90_put_att(ncid, gedi_thvid, "long_name" , "urban_tree_height"                          ) )
   !CALL check( nf90_put_att(ncid, gedi_thvid, "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, gl30_wtvid, "units"     , "%"                                          ) )
   CALL check( nf90_put_att(ncid, gl30_wtvid, "long_name" , "urban_water_cover"                          ) )
   !CALL check( nf90_put_att(ncid, gl30_wtvid, "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, ur_laivid, "units"     , "m^2/m^2"                                     ) )
   CALL check( nf90_put_att(ncid, ur_laivid, "long_name" , "urban_tree_lai"                              ) )
  ! CALL check( nf90_put_att(ncid, ur_laivid, "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, hwr_canvid, "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, hwr_canvid, "long_name" , "canyon height to width ratio"               ) )
   !CALL check( nf90_put_att(ncid, hwr_canvid, "_FillValue", fv   ) )

   CALL check( nf90_put_att(ncid, wt_rfvid  , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, wt_rfvid  , "long_name" , "fraction of roof"                           ) )
   !CALL check( nf90_put_att(ncid, wt_rfvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, wt_rdvid  , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, wt_rdvid  , "long_name" , "fraction of pervious road"                  ) )
   !CALL check( nf90_put_att(ncid, wt_rdvid  , "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, em_rfvid  , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, em_rfvid  , "long_name" , "emissivity of roof"                         ) )
   !CALL check( nf90_put_att(ncid, em_rfvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, em_wlvid  , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, em_wlvid  , "long_name" , "emissivity of wall"                         ) )
   !CALL check( nf90_put_att(ncid, em_wlvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, em_imrdvid, "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, em_imrdvid, "long_name" , "emissivity of impervious road"              ) )
   !CALL check( nf90_put_att(ncid, em_imrdvid, "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, em_perdvid, "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, em_perdvid, "long_name" , "emissivity of pervious road"                ) )
   !CALL check( nf90_put_att(ncid, em_perdvid, "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, ht_rfvid  , "units"     , "meters"                                     ) )
   CALL check( nf90_put_att(ncid, ht_rfvid  , "long_name" , "height of roof"                             ) )
   !CALL check( nf90_put_att(ncid, ht_rfvid  , "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, whcvid    , "units"     , "meters"                                     ) )
   CALL check( nf90_put_att(ncid, whcvid    , "long_name" , "height of wind in canyon"                   ) )
   !CALL check( nf90_put_att(ncid, whcvid    , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, ulev_vid  , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, ulev_vid  , "long_name" , "number of impervious road layers"           ) )
   !CALL check( nf90_put_att(ncid, ulev_vid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, th_rfvid  , "units"     , "m"                                          ) )
   CALL check( nf90_put_att(ncid, th_rfvid  , "long_name" , "thickness of roof"                          ) )
   !CALL check( nf90_put_att(ncid, th_rfvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, th_wlvid  , "units"     , "m"                                          ) )
   CALL check( nf90_put_att(ncid, th_wlvid  , "long_name" , "thickness of wall"                          ) )
   !CALL check( nf90_put_att(ncid, th_wlvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, tbminvid  , "units"     , "K"                                          ) )
   CALL check( nf90_put_att(ncid, tbminvid  , "long_name" , "minimum interior building temperature"      ) )
   !CALL check( nf90_put_att(ncid, tbminvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, tbmaxvid  , "units"     , "K"                                          ) )
   CALL check( nf90_put_att(ncid, tbmaxvid  , "long_name" , "maximum interior building temperature"      ) )
   !CALL check( nf90_put_att(ncid, tbmaxvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, pct_urvid  , "units"     , "%"                                         ) )
   CALL check( nf90_put_att(ncid, pct_urvid  , "long_name" , "percent_of_urban_cover"                    ) )
   !CALL check( nf90_put_att(ncid, pct_urvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, tk_rfvid  , "units"     , "W/m*K"                                      ) )
   CALL check( nf90_put_att(ncid, tk_rfvid  , "long_name" , "thermal conductivity of roof"               ) )
   !CALL check( nf90_put_att(ncid, tk_rfvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, tk_wlvid  , "units", "W/m*K"                                           ) )
   CALL check( nf90_put_att(ncid, tk_wlvid  , "long_name", "thermal conductivity of wall"                ) )
   !CALL check( nf90_put_att(ncid, tk_wlvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, tk_imrdvid, "units"     , "W/m*K"                                      ) )
   CALL check( nf90_put_att(ncid, tk_imrdvid, "long_name" , "thermal conductivity of impervious road"    ) )
   !CALL check( nf90_put_att(ncid, tk_imrdvid, "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, cv_rfvid  , "units"     , "J/m^3*K"                                    ) )
   CALL check( nf90_put_att(ncid, cv_rfvid  , "long_name" , "volumetric heat capacity of roof"           ) )
   !CALL check( nf90_put_att(ncid, cv_rfvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, cv_wlvid  , "units"     , "J/m^3*K"                                    ) )
   CALL check( nf90_put_att(ncid, cv_wlvid  , "long_name" , "volumetric heat capacity of wall"           ) )
   !CALL check( nf90_put_att(ncid, cv_wlvid  , "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, cv_imrdvid, "units"     , "J/m^3*K"                                    ) )
   CALL check( nf90_put_att(ncid, cv_imrdvid, "long_name" , "volumetric heat capacity of impervious road") )
   !CALL check( nf90_put_att(ncid, cv_imrdvid, "_FillValue", fv) )

   CALL check( nf90_put_att(ncid, alb_rfvid , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, alb_rfvid , "long_name" , "albedo of roof"                             ) )
   !CALL check( nf90_put_att(ncid, alb_rfvid , "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, alb_wlvid , "units"     , "unitless"                                   ) )
   CALL check( nf90_put_att(ncid, alb_wlvid , "long_name" , "albedo of wall"                             ) )
   !CALL check( nf90_put_att(ncid, alb_wlvid , "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, alb_imrdvid, "units"     , "unitless"                                  ) )
   CALL check( nf90_put_att(ncid, alb_imrdvid, "long_name" , "albedo of impervious road"                 ) )
   !CALL check( nf90_put_att(ncid, alb_imrdvid, "_FillValue", fv) )
 
   CALL check( nf90_put_att(ncid, alb_perdvid, "units"     , "unitless"                                  ) )
   CALL check( nf90_put_att(ncid, alb_perdvid, "long_name" , "albedo of pervious road"                   ) )
   !CALL check( nf90_put_att(ncid, alb_perdvid, "_FillValue", fv) )
   
   CALL check( nf90_inq_varid(ncid, "lat", urlat_vid           ) )
   CALL check( nf90_put_var  (ncid,urlat_vid, latso            ) )

   CALL check( nf90_inq_varid(ncid, "lon", urlon_vid           ) )
   CALL check( nf90_put_var  (ncid,urlon_vid, lonso            ) )

   call check( nf90_inq_varid(ncid, "numsolar", ns_vid         ) )
   CALL check( nf90_put_var  (ncid, ns_vid, n_ns               ) )

   CALL check( nf90_inq_varid(ncid, "numrad", nr_vid           ) )
   CALL check( nf90_put_var  (ncid, nr_vid, n_nr               ) )

   CALL check( nf90_inq_varid(ncid, "density", den_vid         ) )
   CALL check( nf90_put_var  (ncid, den_vid  , n_den           ) )

   CALL check( nf90_inq_varid(ncid, "pct_urtc", pct_tcvid      ) )
   CALL check( nf90_put_var  (ncid,pct_tcvid, pct_tc           ) )

   CALL check( nf90_inq_varid(ncid, "pct_urwt", pct_urwtvid    ) )
   CALL check( nf90_put_var  (ncid,pct_urwtvid, pct_urwt       ) )

   CALL check( nf90_inq_varid(ncid, "htop_ur", htop_urvid      ) )
   CALL check( nf90_put_var  (ncid,htop_urvid, htop_ur         ) )

   CALL check( nf90_inq_varid(ncid, "urban_lai", ur_laivid     ) )
   CALL check( nf90_put_var  (ncid, ur_laivid, ur_lai          ) )

   CALL check( nf90_inq_varid(ncid, "REGION_ID", ur_rgvid      ) )
   CALL check( nf90_put_var  (ncid,ur_rgvid, ur_rgid           ) )

   CALL check( nf90_inq_varid(ncid, "CANYON_HWR", hwr_canvid   ) )
   CALL check( nf90_put_var  (ncid,hwr_canvid,hwr_can          ) )

   CALL check( nf90_inq_varid(ncid, "WTLUNIT_ROOF",wt_rfvid    ) )
   CALL check( nf90_put_var(ncid,wt_rfvid,wt_rf                ) )

   CALL check( nf90_inq_varid(ncid, "WTROAD_PERV",wt_rdvid     ) )
   CALL check( nf90_put_var(ncid,wt_rdvid, wt_rd               ) )

   CALL check( nf90_inq_varid(ncid, "EM_ROOF",em_rfvid         ) )
   CALL check( nf90_put_var(ncid,em_rfvid,em_rf                ) )

   CALL check( nf90_inq_varid(ncid, "EM_WALL",em_wlvid         ) )
   CALL check( nf90_put_var(ncid,em_wlvid,em_wl                ) )

   CALL check( nf90_inq_varid(ncid, "EM_IMPROAD",em_imrdvid    ) )
   CALL check( nf90_put_var(ncid,em_imrdvid,em_imrd            ) )

   CALL check( nf90_inq_varid(ncid, "EM_PERROAD",em_perdvid    ) )
   CALL check( nf90_put_var(ncid,em_perdvid,em_perd            ) )

   CALL check( nf90_inq_varid(ncid, "HT_ROOF",ht_rfvid         ) )
   CALL check( nf90_put_var(ncid,ht_rfvid,ht_rf                ) )

   CALL check( nf90_inq_varid(ncid, "WIND_HGT_CANYON",whcvid   ) )
   CALL check( nf90_put_var(ncid,whcvid,w_hc                   ) )

   CALL check( nf90_inq_varid(ncid, "THICK_ROOF",th_rfvid      ) )
   CALL check( nf90_put_var(ncid,th_rfvid,th_rf                ) )

   CALL check( nf90_inq_varid(ncid, "THICK_WALL",th_wlvid      ) )
   CALL check( nf90_put_var(ncid,th_wlvid,th_wl                ) )

   CALL check( nf90_inq_varid(ncid, "T_BUILDING_MIN",tbminvid  ) )
   CALL check( nf90_put_var(ncid,tbminvid,tb_min               ) )

   CALL check( nf90_inq_varid(ncid, "T_BUILDING_MAX",tbmaxvid  ) )
   CALL check( nf90_put_var(ncid,tbmaxvid,tb_max               ) )

   CALL check( nf90_inq_varid(ncid, "NLEV_IMPROAD",ulev_imrdvid) )
   CALL check( nf90_put_var(ncid,ulev_imrdvid,ulev_imrd        ) )

   CALL check( nf90_inq_varid(ncid, "pct_urban", pct_urvid     ) )
   CALL check( nf90_put_var(ncid,pct_urvid,pct_ur              ) )

   CALL check( nf90_inq_varid(ncid, "TK_ROOF",tk_rfvid         ) )
   CALL check( nf90_put_var(ncid,tk_rfvid,tk_rf                ) )

   CALL check( nf90_inq_varid(ncid, "TK_WALL",tk_wlvid         ) )
   CALL check( nf90_put_var(ncid,tk_wlvid,tk_wl                ) )

   CALL check( nf90_inq_varid(ncid, "TK_IMPROAD",tk_imrdvid    ) )
   CALL check( nf90_put_var(ncid,tk_imrdvid,tk_imrd            ) )

   CALL check( nf90_inq_varid(ncid, "CV_ROOF",cv_rfvid         ) )
   CALL check( nf90_put_var(ncid,cv_rfvid,cv_rf                ) )

   CALL check( nf90_inq_varid(ncid, "CV_WALL",cv_wlvid         ) )
   CALL check( nf90_put_var(ncid,cv_wlvid,cv_wl                ) )

   CALL check( nf90_inq_varid(ncid, "CV_IMPROAD",cv_imrdvid    ) )
   CALL check( nf90_put_var(ncid,cv_imrdvid,cv_imrd            ) )

   CALL check( nf90_inq_varid(ncid, "ALB_ROOF",alb_rfvid       ) )
   CALL check( nf90_put_var(ncid,alb_rfvid,alb_rf              ) )

   CALL check( nf90_inq_varid(ncid, "ALB_WALL",alb_wlvid       ) )
   CALL check( nf90_put_var(ncid,alb_wlvid,alb_wl              ) )

   CALL check( nf90_inq_varid(ncid, "ALB_IMPROAD",alb_imrdvid  ) )
   CALL check( nf90_put_var(ncid,alb_imrdvid,alb_imrd          ) )

   CALL check( nf90_inq_varid(ncid, "ALB_PERROAD",alb_perdvid  ) )
   CALL check( nf90_put_var(ncid,alb_perdvid,alb_perdvid       ) )

   CALL check( nf90_close(ncid) )
   print*, "*** SUCCESS write surface file ***"
   
   deallocate( tlat   )
   deallocate( tlon   )
   deallocate( hlat   )
   deallocate( hlats  )
   deallocate( hlatn  )
   deallocate( hlon   )
   deallocate( hlonw  )
   deallocate( hlone  )
   deallocate( urlat  )
   deallocate( nrlat  )
   deallocate( urlats )
   deallocate( urlatn )
   deallocate( urlon  )
   deallocate( urlonw )
   deallocate( urlone )
   deallocate( harea  )
   deallocate( mask   )
   deallocate( gfcc_tc)
   deallocate( gedi_th)
   deallocate( gl30_wt)

   deallocate( uarea  )
   deallocate( umask  )
   deallocate( umd    )
   deallocate( nmd    )
   deallocate( uhd    )
   deallocate( nhd    )
   deallocate( utbd   )
   deallocate( ntbd   )
   deallocate( urclss )
   deallocate( urrgid )

   deallocate( latso     )
   deallocate( lonso     )
   deallocate( area      )
   deallocate( pct_urban )
   deallocate( npct_urban)
   deallocate( htop_pft  )
   deallocate( nhtop_pft )
   deallocate( pct_pft   )
   deallocate( npct_pft  )
   deallocate( lai_pft   )
   deallocate( nlai_pft  )
   deallocate( ur_dc     )
   deallocate( pct_ur    )

   deallocate( cnt       )
   deallocate( pct_tc    )
   deallocate( pct_tland )
   deallocate( pct_wland )
   deallocate( pct_hland )
   deallocate( pct_urwt  )
   deallocate( htop_ur   )
   deallocate( ur_land   )
   deallocate( ur_clss   )
   deallocate( ur_rgid   )
   deallocate( ur_lai    )
   deallocate( lai_wgt   )
   deallocate( hwr_can   )
   deallocate( wt_rf     )
   deallocate( wt_rd     )
   deallocate( em_rf     )
   deallocate( em_wl     )
   deallocate( em_imrd   )
   deallocate( em_perd   )
   deallocate( ht_rf     )
   deallocate( w_hc      )
   deallocate( ulev_imrd )
   deallocate( th_rf     )
   deallocate( th_wl     )
   deallocate( tb_min    )
   deallocate( tb_max    )
   deallocate( cv_rf     )
   deallocate( cv_wl     )
   deallocate( cv_imrd   )
   deallocate( tk_rf     )
   deallocate( tk_wl     )
   deallocate( tk_imrd   )
   deallocate( alb_rf    )
   deallocate( alb_wl    )
   deallocate( alb_imrd  )
   deallocate( alb_perd  )

CONTAINS 
   
   SUBROUTINE check(status)
      INTEGER, intent(in) :: status

      IF (status /= nf90_noerr) THEN
         print *, trim( nf90_strerror(status))
         stop 2
      ENDIF 
   END SUBROUTINE check

END PROGRAM clmu2grid
