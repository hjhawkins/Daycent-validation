1845          Starting year #Leap years and fire regime matched with actual .wth file
1979          Last year #Brotherton trial started 1980
brotherton.100 Site file name
0             Labeling type
-1            Labeling year
-1.00         Microcosm
-1            CO2 Systems
-1            pH effect
-1            Soil warming
0             N input scalar option (0 or 1)
0             OMAD scalar option (0 or 1)
0             Climate scalar option
1             Initial system #1) for crop or grasslands 2) forest 3) savanna
GGCP_B        Initial crop #Temperate-montane perennial grasses and forbs, mostly grasses, 50% C4, 20% C3
TMDF          Initial tree #temperate deciduous trees not present

Year Month Option
1             Block #SH temperate grassland, as for eq run, never cultivated,looping weather file 2013-2019
1968          Last year
2             Repeats # years
1845          Output starting year
1             Output month #JUST CHECK SHOULD THIS BE 6?
10.00         Output interval
F             Weather choice
brotherton_base.wth
   1     1 CROP GGCP_B
   1     1 GRAZ GL
   1    60 GRAZ GL
   1    92 LAST
   1   121 GRAZ GL
   1   122 SENM
   1   245 FRST
   1   305 GRAZ GL
   2     1 CROP GGCP_B
   2     1 GRAZ GL
   2    60 GRAZ GL
   2    92 LAST
   2   121 GRAZ GL
   2   122 SENM
   2   135 FIRE C
   2   245 FRST
   2   305 GRAZ GL
   -999 -999 X  
  
2             Block #SH As Block 1, note weather file (2013-2019) should match leap years and end on a no-fire year (since exp run will start on a fire year)
1979          Last year
2             Repeats # years
1969          Output starting year
1             Output month #JUST CHECK SHOULD THIS BE 6?
1.00          Output interval
C             Weather choice
   1     1 CROP GGCP_B
   1     1 GRAZ GL
   1    60 GRAZ GL
   1    92 LAST
   1   121 GRAZ GL
   1   122 SENM
   1   245 FRST
   1   305 GRAZ GL
   2     1 CROP GGCP_B
   2     1 GRAZ GL
   2    60 GRAZ GL
   2    92 LAST
   2   121 GRAZ GL
   2   122 SENM
   2   135 FIRE C
   2   245 FRST
   2   305 GRAZ GL
   -999 -999 X  