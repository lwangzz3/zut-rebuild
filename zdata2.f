      SUBROUTINE ZDATA2                                                 ZDA   10
C                                                                       ZDA   20
C     MAIN OF DATA-2 IN ZUT(99/05)                                      ZDA   30
C                                                                       ZDA   40
      COMMON /VARIAD/ KMAT,IACT                                         ZDA   50
C                                                                       ZDA   60
      COMMON /DATATA/ NXT29,NDA29,IPUO2                                 ZDA   70
C                                                                       ZDA   80
      DIMENSION K(200)                                                  ZDA   90
C                                                                       ZDA  100
C                                                                       ZDA  110
      NXT29 = 1                                                         ZDA  120
      NDA29 = 29                                                        ZDA  130
      IACT = 28                                                         ZDA  140
      KMAT = 3                                                          ZDA  150
      IU4 = 0                                                           ZDA  160
      OPEN(29,STATUS='SCRATCH',ACCESS='DIRECT',RECL=800)                ZDA  170
C                                                                       ZDA  180
      CALL DAT                                                          ZDA  190
C                                                                       ZDA  200
      NHOM=0                                                            ZDA  210
      KOSTDA=0                                                          ZDA  220
      NDANC=0                                                           ZDA  230
C                                                                       ZDA  240
      NGAM = 1                                                          ZDA  250
      NTHER = 1                                                         ZDA  260
      NZUT = 1                                                          ZDA  270
C                                                                       ZDA  280
      CALL SUBMAI(NGAM,NTHER,NZUT,NDANC,KOSTDA,IU4,K(1))                ZDA  290
C                                                                       ZDA  300
C                                                                       ZDA  310
 1002 FORMAT (18I4)                                                     ZDA  320
C                                                                       ZDA  330
      RETURN                                                            ZDA  340
      END                                                               ZDA  350
      SUBROUTINE SUBMAI(NGAM,NTHER,NZUT,NDANC,KOSTDA,IU4,IMAT)          SUB   10
C                                                                       SUB   20
      COMMON /VARIAD/ KMAT,IACT                                         SUB   30
C                                                                       SUB   40
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             SUB   50
C                                                                       SUB   60
      COMMON /GAMD/ CEG(33),AL1(33),IDSATZ(12),NOID,NTBD,NRES,IDRES,WTP,SUB   70
     1 WSTU,WAG,NOAG                                                    SUB   80
C                                                                       SUB   90
      COMMON /THER/ MTBL(20),LIM(30),ID,NX,NHET,NTHGR,MX,IDC,IDO,TTEMP, SUB  100
     1 MMX                                                              SUB  110
C                                                                       SUB  120
      COMMON /DATATA/ NXT29,NDA29,IPUO2                                 SUB  130
C                                                                       SUB  140
      COMMON /AAAA/ AAA(100)                                            SUB  150
C                                                                       SUB  160
      COMMON /COSTS/ KOSTD2,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)SUB  170
     1 ,FRF(4),FABKOS,AUFKOS,DK                                         SUB  180
C                                                                       SUB  190
      COMMON /CPDAT/ ANR,INDBS,DU(21),BETA                              SUB  200
C                                                                       SUB  210
      COMMON /ANR4/ V4Z5(4),U5UG(4)                                     SUB  220
C                                                                       SUB  230
      COMMON /SMDEF/ IMA(30)                                            SUB  240
C                                                                       SUB  250
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, SUB  260
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     SUB  270
C                                                                       SUB  280
      CHARACTER*4 TITEL(18),ENDE/'stop'/                                SUB  290
C                                                                       SUB  300
      DIMENSION NRMAF(40),NRGAM(40),DEN(40),DENG(40),FRACT(15),RADI(15),SUB  310
     1 NFT(30),IMAT(KMAT+IACT)                                          SUB  320
C                                                                       SUB  330
C                                                                       SUB  340
      IU8T = 0                                                          SUB  350
      KOSTD2 = KOSTDA                                                   SUB  360
      FF2 = 0.61                                                        SUB  370
      N3 = 3                                                            SUB  380
      N10 = 10                                                          SUB  390
      N15 = 40                                                          SUB  400
      N30 = 30                                                          SUB  410
      NC = 1                                                            SUB  420
      NF = 1                                                            SUB  430
      TTEMP = 0.                                                        SUB  440
      DO 30 I=1,N30                                                     SUB  450
        NFT(I) = 0                                                      SUB  460
   30 CONTINUE                                                          SUB  470
      FABKOS = 0.                                                       SUB  480
      AUFKOS = 0.                                                       SUB  490
      IMAT(IACT+1) = 26                                                 SUB  500
      IMAT(IACT+2) = 5                                                  SUB  510
      IMAT(IACT+3) = 23                                                 SUB  520
      IF(IACT .LE. 0) GOTO 91                                           SUB  530
      DO 90 L=1,IACT                                                    SUB  540
        IF(IMA(L) .EQ. 9) IMAU4 = L                                     SUB  550
        IMAT(L) = IMA(L)                                                SUB  560
   90 CONTINUE                                                          SUB  570
   91 CONTINUE                                                          SUB  580
      KMA = KMAT + IACT                                                 SUB  590
      WRITE (6,1015) KMA                                                SUB  600
      WRITE (6,1002) (IMAT(L),L=1,KMA)                                  SUB  610
      NRET = NGAM + NTHER                                               SUB  620
      NHET = NTHER - 1                                                  SUB  630
      WRITE (NDA29,REC=NXT29) KMAT,(IMAT(L+IACT),L=1,KMAT)              SUB  640
C                                                                       SUB  650
CARD DZ 1                                                               SUB  660
C                                                                       SUB  670
 7999 READ (5,1000) (TITEL(I),I=1,18)                                   SUB  680
C                                                                       SUB  690
      IF(TITEL(1) .EQ. ENDE) GOTO 8000                                  SUB  700
      IPUO2 = 0                                                         SUB  710
      DO 4567 JKL=1,90                                                  SUB  720
        AAA(JKL)=0.                                                     SUB  730
 4567 CONTINUE                                                          SUB  740
      DO 20 I=1,N15                                                     SUB  750
        DEN(I) = 0.                                                     SUB  760
        NRMAF(I) = 0                                                    SUB  770
        NRGAM(I) = 0                                                    SUB  780
        DENG(I) = 0.                                                    SUB  790
        IF(I .GT. 15) GOTO 20                                           SUB  800
        FRACT(I) = 0.                                                   SUB  810
        RADI(I) = 0.                                                    SUB  820
   20 CONTINUE                                                          SUB  830
C                                                                       SUB  840
CARD DZ 2                                                               SUB  850
C                                                                       SUB  860
      READ (5,1020) NFUTP,NFCP,NFBZ,FF3                                 SUB  870
      NTYP = 0                                                          SUB  880
C                                                                       SUB  890
      NZUS = 0                                                          SUB  900
      IF(FF3 .NE. 0.) FF2 = FF3                                         SUB  910
      IF(NFUTP .LE. 0) GOTO 40                                          SUB  920
      MRK = MAX0(N3,NXT29)                                              SUB  930
      NXT29 = 2                                                         SUB  940
      NFT(MRK) = NFUTP                                                  SUB  950
      WRITE (NDA29,REC=NXT29) N30,(NFT(I),I=1,N30)                      SUB  960
      NXT29 = MRK                                                       SUB  970
   40 CONTINUE                                                          SUB  980
      NFU = NFUTP / 100                                                 SUB  990
      NVR = NFUTP - (100*NFU)                                           SUB 1000
      WRITE (6,1011) (TITEL(I),I=1,18),NFU,NVR,NXT29                    SUB 1010
C                                                                       SUB 1020
      IF(NFCP .NE. 0) CALL CP(NFCP,IU8T)                                SUB 1030
C                                                                       SUB 1040
      NTYP = NTYP + 1                                                   SUB 1050
      IF(NFCP .EQ. 0 .AND. NFBZ .EQ. 0) GOTO 5                          SUB 1060
      GOTO(2,3,4),NTYP                                                  SUB 1070
C                                                                       SUB 1080
    2 CALL KUGEL(NFBZ,*5)                                               SUB 1090
C                                                                       SUB 1100
    3 CALL STAB(NFBZ,*5)                                                SUB 1110
C                                                                       SUB 1120
    4 CONTINUE                                                          SUB 1130
    5 CONTINUE                                                          SUB 1140
C                                                                       SUB 1150
      IF(NZUT.NE.0) CALL ZUT(NTYP,NRET,*7999)                           SUB 1160
C                                                                       SUB 1170
      IF(NGAM .NE. 0) CALL GAM1(NTYP)                                   SUB 1180
C                                                                       SUB 1190
      IF(NTHER .NE. 0) CALL THERMD(NTYP)                                SUB 1200
C                                                                       SUB 1210
      II = 0                                                            SUB 1220
      DO 11 I=1,N10                                                     SUB 1230
        IF(II .LT. 2 .AND. AAA(I) .GT. 0.) II = I                       SUB 1240
        NRGAM(I) = AAA(I)                                               SUB 1250
        DENG(I) = AAA(I+N10)                                            SUB 1260
   11 CONTINUE                                                          SUB 1270
      NISO = 0                                                          SUB 1280
      DO 15 I=1,KMA                                                     SUB 1290
        IMI = 0                                                         SUB 1300
        DO 15 J=1,N15                                                   SUB 1310
          IF(IMAT(I) .NE. NRGAM(J)) GOTO 14                             SUB 1320
          NISO = NISO + 1                                               SUB 1330
          NISO = NISO - IMI                                             SUB 1340
          IMI = IMI + 1                                                 SUB 1350
          NRMAF(NISO) = I                                               SUB 1360
          DEN(NISO) = DENG(J)                                           SUB 1370
   14     CONTINUE                                                      SUB 1380
   15 CONTINUE                                                          SUB 1390
      IF(IU4 .GT. 0) GOTO 105                                           SUB 1400
      IF(INDBS .EQ. 7 .OR. INDBS .EQ. 8) GOTO 105                       SUB 1410
      IF(INDBS .GT. 3) AAA(71) = BETA                                   SUB 1420
      DO 100 I=1,4                                                      SUB 1430
        IF(AAA(71) .LE. U5UG(I)) GOTO 101                               SUB 1440
  100 CONTINUE                                                          SUB 1450
  101 CONTINUE                                                          SUB 1460
      J = I                                                             SUB 1470
      IF(AAA(71) .EQ. U5UG(J) .OR. J .EQ. 1) GOTO 102                   SUB 1480
      X2 = U5UG(J)                                                      SUB 1490
      X1 = U5UG(J-1)                                                    SUB 1500
      Y2 = V4Z5(J)                                                      SUB 1510
      Y1 = V4Z5(J-1)                                                    SUB 1520
      V45 = Y1 + ((Y2-Y1)/(X2-X1)) * (AAA(71)-X1)                       SUB 1530
      GOTO 103                                                          SUB 1540
  102 CONTINUE                                                          SUB 1550
      V45 = V4Z5(J)                                                     SUB 1560
  103 CONTINUE                                                          SUB 1570
      NISO = NISO + 1                                                   SUB 1580
      DO 104 I=NISO,2,-1                                                SUB 1590
        NRMAF(I) = NRMAF(I-1)                                           SUB 1600
        DEN(I) = DEN(I-1)                                               SUB 1610
  104 CONTINUE                                                          SUB 1620
      NRMAF(1) = IMAU4                                                  SUB 1630
      K = 2                                                             SUB 1640
      M = 3                                                             SUB 1650
      IF(INDBS .LE. 3 .OR. INDBS .GT. 6) GOTO 106                       SUB 1660
      K = 3                                                             SUB 1670
      M = 4                                                             SUB 1680
  106 CONTINUE                                                          SUB 1690
      DEN(1) = DEN(K) * V45                                             SUB 1700
      DEN(M) = DEN(M) - DEN(1)                                          SUB 1710
  105 CONTINUE                                                          SUB 1720
      IF(IPUO2 .EQ. 1) DEN(1) = 0.                                      SUB 1730
      IF(BK .LT. 0.99990) GOTO 111                                      SUB 1740
      DO 110 I=1,NISO                                                   SUB 1750
        IF(NRMAF(I) .LE. IACT) DEN(I) = 0.                              SUB 1760
  110 CONTINUE                                                          SUB 1770
  111 CONTINUE                                                          SUB 1780
      JI = 0                                                            SUB 1790
      IZEUG = 0                                                         SUB 1800
      NZEUG = 2                                                         SUB 1810
      MX = MMX                                                          SUB 1820
      MXP1 = MMX + 1                                                    SUB 1830
      FRACOA = 1.                                                       SUB 1840
   19 CONTINUE                                                          SUB 1850
      DO 17 J=1,MX                                                      SUB 1860
        III = J + JI                                                    SUB 1870
        IIII = 10 + J * 10 + II                                         SUB 1880
        FRACT(III) = AAA(IIII)                                          SUB 1890
   17 CONTINUE                                                          SUB 1900
      FRACT(MXP1+JI) = FRACOA                                           SUB 1910
      IZEUG = IZEUG + 1                                                 SUB 1920
      FRACOA = 0.                                                       SUB 1930
      JI = JI + MXP1                                                    SUB 1940
      II = 1                                                            SUB 1950
      IF(IZEUG .LT. 2) GOTO 19                                          SUB 1960
      MXZEUG = NZEUG * MXP1                                             SUB 1970
      RADI(1) = AAA(75)                                                 SUB 1980
      RADI(2) = AAA(72)                                                 SUB 1990
      RADI(3) = AAA(83)                                                 SUB 2000
      RADI(4) = AAA(88)                                                 SUB 2010
      DO 32 I=1,3                                                       SUB 2020
        RADI(4+I) = AAA(91+I)                                           SUB 2030
   32 CONTINUE                                                          SUB 2040
      WRITE (NDA29,REC=NXT29) NISO,(NRMAF(I),DEN(I),I=1,NISO),AAA(75),  SUB 2050
     1 AAA(76),AAA(89),AAA(90),(AAA(I),I=79,88),NFU,NVR,(IMAT(NRMAF(I)),SUB 2060
     2 I=1,NISO)                                                        SUB 2070
      NXT29 = NXT29 + 1                                                 SUB 2080
      GOTO 7999                                                         SUB 2090
 8000 RETURN                                                            SUB 2100
C                                                                       SUB 2110
 1000 FORMAT (18A4)                                                     SUB 2120
 1001 FORMAT (6E12.5)                                                   SUB 2130
 1002 FORMAT (18I4)                                                     SUB 2140
 1005 FORMAT (I4,4X,E12.5)                                              SUB 2150
 1011 FORMAT ('1',130('=')//1X,18A4,5X,'FUEL TYPE: ',I3,',  VARIANT NO: SUB 2160
     1',I3,',  DATA-2-SET NO.: ',I3//)                                  SUB 2170
 1012 FORMAT (A4,2I4,10F6.0)                                            SUB 2180
 1015 FORMAT ('1'//' GAM-LIBRARY IDENTIFICATION FOR THE',I4,' VSOP ISOTOSUB 2190
     1PES:'/)                                                           SUB 2200
 1016 FORMAT (/' FUEL ELEMENT TYPES',I24,3I12)                          SUB 2210
 1020 FORMAT (3I4,E12.5)                                                SUB 2220
      END                                                               SUB 2230
      SUBROUTINE DAT                                                    DAT   10
C                                                                       DAT   20
      CHARACTER*2 DATG(4)                                               DAT   30
C                                                                       DAT   40
      CHARACTER*4 DATH                                                  DAT   50
C                                                                       DAT   60
      CHARACTER*8 DATF                                                  DAT   70
C                                                                       DAT   80
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             DAT   90
C                                                                       DAT  100
C                                                                       DAT  110
      CALL DATE_AND_TIME(DATF)                                          DAT  120
C                                                                       DAT  130
      READ (UNIT=DATH,FMT=101) JAHR                                     DAT  140
      READ (UNIT=DATG(3),FMT=100) IMON                                  DAT  150
      READ (UNIT=DATG(4),FMT=100) ITAG                                  DAT  160
      WRITE (6,2000)                                                    DAT  170
      WRITE (6,20)                                                      DAT  180
      WRITE (6,2001) ITAG,IMON,JAHR                                     DAT  190
      WRITE (6,30)                                                      DAT  200
C                                                                       DAT  210
   20 FORMAT (//////35X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/35X,'DAT  220
     1**',5X,'**',3(6X,'**',5X,'**')/35X,'**',5X,'**',6X,'**',13X,'**', DAT  230
     2 5X,'**',6X,8('*')/36X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,'DAT  240
     3**'/37X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**',   DAT  250
     4 2(2X,'**'),9X,'**'/38X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),DAT  260
     5 3X,'**',2X,'**',9X,'**'////)                                     DAT  270
   30 FORMAT (//////////17X,'OCT. 2005 ',64X,'REPORT: V.S.O.P.(99/05)'/ DAT  280
     1 99X,'JUEL - 4189'/99X,'SECTION 5.2')                             DAT  290
  100 FORMAT (4I2)                                                      DAT  300
  101 FORMAT (I4)                                                       DAT  310
 2000 FORMAT ('1'////18X,12('*'),11X,'****',10X,13('*'),10X,'****',18X, DAT  320
     1 11('*')/18X,13('*'),9X,6('*'),9X,13('*'),9X,6('*'),16X,13('*')/  DAT  330
     2 18X,'***',6X,'****',8X,'***',2X,'***',13X,'***',13X,'***',2X,'***DAT  340
     3',15X,'****',5X,'****'/18X,'***',7X,'***',8X,'***',2X,'***',2(13X,DAT  350
     4 '***'),2X,'***',15X,'***',6X,'***'/18X,'***',2(7X,'***'),4X,'***'DAT  360
     5 ,2(12X,'***'),4X,'***',22X,'***'/18X,'***',2(7X,'***'),4X,'***', DAT  370
     6 2(12X,'***'),4X,'***',4X,8('*'),9X,'***'/18X,'***',7X,'***',2(6X,DAT  380
     7 '***'),2(11X,'***'),6X,'***',3X,8('*'),8X,'***'/18X,'***',7X,'***DAT  390
     8',2(6X,'***'),2(11X,'***'),6X,'***',18X,'***'/18X,'***',7X,'***', DAT  400
     9 5X,14('*'),10X,'***',10X,14('*'),16X,'***'/18X,'***',6X,'****',5XDAT  410
     X ,14('*'),10X,'***',10X,14('*'),15X,'***',6X,'**'/18X,13('*'),5X,'DAT  420
     Y***',8X,'***',2(10X,'***'),8X,'***',14X,12('*')/18X,12('*'),6X,'**DAT  430
     Z*',8X,'***',2(10X,'***'),8X,'***',13X,13('*'))                    DAT  440
 2001 FORMAT (55X,'( ',I2,'.',I2,'.',I4,' )')                           DAT  450
      RETURN                                                            DAT  460
      END                                                               DAT  470
      BLOCK DATA                                                        BLO   10
C                                                                       BLO   20
      COMMON /CONST/ ANUKL,NGAM,GEW,AVO,PI                              BLO   30
C                                                                       BLO   40
      COMMON /FUEL/ BRST,COAT                                           BLO   50
C                                                                       BLO   60
CFZJ035                                                       14.09.04  BLO   70
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO,SPPU2,SPU5,SPPU0                BLO   80
C                                                                       BLO   90
      COMMON /ANR4/ V4Z5,U5UG                                           BLO  100
C                                                                       BLO  110
      COMMON /SMDEF/ IMA                                                BLO  120
C                                                                       BLO  130
      CHARACTER*8 BRST(9) /'UO2     ','UC      ','UC2     ','THO2-UO2', BLO  140
     1 'THC-UC  ','THC2-UC2','PUO2    ','PU-TH-O2','PUO2-UO2'/          BLO  150
C                                                                       BLO  160
      CHARACTER*4 COAT(2)/'PYC ','SIC '/,ANUKL(10)/'C   ','TH  ','U-35',BLO  170
     1 'U-38','PU39','PU40','PU41','PU42','O   ','SI  '/                BLO  180
C                                                                       BLO  190
CFZJ035                                                       14.09.04  BLO  200
      REAL SPTH/12.6/,SPU8/11.29/,SPC/4.6/,SPO/3.8/,SPPU2/10.69/,       BLO  210
     1 SPU5/15./,AVO/0.6022045/,PI/3.14159/,V4Z5(4)/7.5E-3,9.86E-3,     BLO  220
     2 1.01E-2,1.11E-2/,U5UG(4)/7.2E-3,0.1,0.167,0.93/,GEW(10)/12.011,  BLO  230
     3 232.111,235.117,238.125,239.127,240.129,241.131,242.134,16.0,    BLO  240
     4 40.101/,SPPU0/10.2/                                              BLO  250
C                                                                       BLO  260
      INTEGER NGAM(10)/5,6,10,12,14,15,16,17,23,26/,IMA(30)/6,185,7,8,9,BLO  270
     1 10,11,132,12,186,133,187,13,188,177,14,15,16,17,189,178,180,179, BLO  280
     2 181,190,182,183,184,0,0/                                         BLO  290
      END                                                               BLO  300
      SUBROUTINE CP(NFOLG,IU8T)                                         CP    10
C                                                                       CP    20
      CHARACTER*4 ANUC3/'U-33'/,ANUC5/'U-35'/,COAT,ANUKL,QCOAT          CP    30
C                                                                       CP    40
      CHARACTER*8 BRST(9)                                               CP    50
C                                                                       CP    60
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             CP    70
C                                                                       CP    80
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  CP    90
C                                                                       CP   100
      COMMON /FUEL/ BRST,COAT(2)                                        CP   110
C                                                                       CP   120
      COMMON /CPDAT/ ANR,INDBS,NCT,NSIC1,NSIC2,K,PU(4),DCT(5),ROCT(5),RKCP   130
     1 ,ROBR1,ROBR2,BETA                                                CP   140
C                                                                       CP   150
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, CP   160
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2             CP   170
C                                                                       CP   180
      COMMON /AAAA/ AAA(90),RADIU                                       CP   190
C                                                                       CP   200
      COMMON /COSTS/ KOSTDA,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)CP   210
     1 ,FRF(4),FABKOS,AUFKOS,DK,IU8TH                                   CP   220
C                                                                       CP   230
      COMMON /DATATA/ NXT29,NDA29,IPUO2                                 CP   240
C                                                                       CP   250
C                                                                       CP   260
      VOL(X1,X2) = (X2**3-X1**3) * 4. * PI / 3.                         CP   270
      DO 98 I=1,10                                                      CP   280
        NIS(I) = 0                                                      CP   290
        V(I) = 0.                                                       CP   300
        DO 98 J=1,10                                                    CP   310
          A(J,I) = 0.                                                   CP   320
   98 CONTINUE                                                          CP   330
C                                                                       CP   340
CARD DZ 3                                                               CP   350
C                                                                       CP   360
      READ (5,1001) ANR                                                 CP   370
C                                                                       CP   380
      IF(ANR .NE. 0.) WRITE (6,2007) ANR                                CP   390
      AAA(71) = ANR                                                     CP   400
      IU8TH = IU8T                                                      CP   410
      IF(NFOLG .EQ. 1) GOTO 99                                          CP   420
      ANUKL(3) = ANUC5                                                  CP   430
      NGAM(3) = 10                                                      CP   440
      GEW(3) = 235.117                                                  CP   450
C                                                                       CP   460
CARD DZ 4                                                               CP   470
C                                                                       CP   480
      READ (5,1000) INDBS,NCT,NSIC1,NSIC2,IU8TH                         CP   490
C                                                                       CP   500
      IU8T = IU8TH                                                      CP   510
      K = INDBS                                                         CP   520
      WRITE (6,2000) BRST(INDBS)                                        CP   530
C                                                                       CP   540
CARD DZ 5                                                               CP   550
C                                                                       CP   560
      READ (5,1001) RK,ROBR1,ROBR2,BETA                                 CP   570
C                                                                       CP   580
      IF(K .NE. 7 .AND. K .NE. 8 .AND. K .NE. 9) GOTO 9999              CP   590
C                                                                       CP   600
CARD DZ 6                                                               CP   610
C                                                                       CP   620
      READ (5,1001) (PU(I),I=1,4)                                       CP   630
C                                                                       CP   640
      IF(K .NE. 7) GOTO 9999                                            CP   650
      ANR = (PU(1)+PU(3)) * 0.999999                                    CP   660
      K = 8                                                             CP   670
      INDBS = 8                                                         CP   680
      IPUO2 = 1                                                         CP   690
 9999 CONTINUE                                                          CP   700
C                                                                       CP   710
CARD DZ 7                                                               CP   720
C                                                                       CP   730
      READ (5,1001) (DCT(I),ROCT(I),I=1,NCT)                            CP   740
C                                                                       CP   750
      WRITE (6,2001) RK,ROBR1,ROBR2                                     CP   760
      IF(ROBR2 .EQ. 0.) ROBR2 = ROBR1                                   CP   770
      DO 12 I=1,NCT                                                     CP   780
        QCOAT = COAT(1)                                                 CP   790
        IF(I .EQ. NSIC1 .OR. I .EQ. NSIC2) QCOAT = COAT(2)              CP   800
        WRITE (6,2002) DCT(I),ROCT(I),QCOAT                             CP   810
   12 CONTINUE                                                          CP   820
      IF(K .EQ. 7 .OR. K .EQ. 8 .OR. K .EQ. 9) WRITE (6,2005) (PU(I),I= CP   830
     1 1,4)                                                             CP   840
   99 CONTINUE                                                          CP   850
      NIS(1) = 1                                                        CP   860
      GOTO(20,20,20,97,97,97,97,97,20),K                                CP   870
   97 CONTINUE                                                          CP   880
      NIS(2) = 1                                                        CP   890
      IF(IU8TH .EQ. 2) NIS(2) = 2                                       CP   900
      IF(IU8TH .EQ. 3) NIS(2) = 3                                       CP   910
      IF(IU8TH .EQ. 4) NIS(2) = 4                                       CP   920
CFZJ035                                                       14.09.04  CP   930
      IF(IU8TH .EQ. 5) NIS(2) = 5                                       CP   940
   20 CONTINUE                                                          CP   950
      IF(K .NE. 7 .AND. K .NE. 8) NIS(3) = 1                            CP   960
      IF(K .NE. 7 .AND. K .NE. 8) NIS(4) = 1                            CP   970
      IF(K .NE. 7 .AND. K .NE. 8 .AND. K .NE. 9) GOTO 21                CP   980
      NIS(5) = 1                                                        CP   990
      NIS(6) = 1                                                        CP  1000
      NIS(7) = 1                                                        CP  1010
      NIS(8) = 1                                                        CP  1020
   21 CONTINUE                                                          CP  1030
      IF(K .EQ. 1 .OR. K .EQ. 4 .OR. K .EQ. 7 .OR. K .EQ. 8 .OR. K .EQ. CP  1040
     1 9) NIS(9) = 1                                                    CP  1050
      IF(NSIC1 .NE. 0 .OR. NSIC2 .NE. 0) NIS(10) = 1                    CP  1060
      RCP = RK                                                          CP  1070
      DO 14 I=1,NCT                                                     CP  1080
        RCP = RCP + DCT(I)                                              CP  1090
   14 CONTINUE                                                          CP  1100
C                                                                       CP  1110
C     AUSSERER RADIUS DER CP                                            CP  1120
C                                                                       CP  1130
      AAA(72) = RCP                                                     CP  1140
      V(1) = VOL(0.,RK)                                                 CP  1150
      V(2) = VOL(RK,RCP)                                                CP  1160
      SUM = 0.                                                          CP  1170
      VSIC = 0.                                                         CP  1180
      X1 = RK                                                           CP  1190
      DO 16 I=1,NCT                                                     CP  1200
        X2 = X1 + DCT(I)                                                CP  1210
        W = VOL(X1,X2)                                                  CP  1220
        IF(I .NE. NSIC1 .AND. I .NE. NSIC2) GOTO 15                     CP  1230
        VSIC = VSIC + W                                                 CP  1240
        W = W * GEW(1) / GEW(10)                                        CP  1250
   15   SUM = SUM + ROCT(I) * W                                         CP  1260
        X1 = X2                                                         CP  1270
   16 CONTINUE                                                          CP  1280
      A(1,2) = AVO * SUM / (GEW(1)*V(2))                                CP  1290
      IF(NSIC1 .NE. 0 .OR. NSIC2 .NE. 0) A(10,2) = AVO * ROCT(NSIC1) *  CP  1300
     1 VSIC / (GEW(10)*V(2))                                            CP  1310
      GOTO(100,200,300,400,500,600,700,800,900),INDBS                   CP  1320
  100 ALFA = 2.                                                         CP  1330
      ISTR = 9                                                          CP  1340
      GOTO 22                                                           CP  1350
  200 ALFA = 1.                                                         CP  1360
      ISTR = 1                                                          CP  1370
      GOTO 22                                                           CP  1380
  300 ALFA = 2.                                                         CP  1390
      ISTR = 1                                                          CP  1400
   22 GBR = GEW(3) * ANR + GEW(4) * (1.-ANR) + ALFA * GEW(ISTR)         CP  1410
      ROBR = ROBR1                                                      CP  1420
      FAKMOL = 1. - ALFA * GEW(ISTR) / GBR                              CP  1430
      ASM = AVO * ROBR1 / GBR                                           CP  1440
      A(3,1) = ASM * ANR                                                CP  1450
      A(4,1) = ASM * (1.-ANR)                                           CP  1460
C	A(4,1) = ASM * (1-ANR) * 1.005
      A(ISTR,1) = ALFA * ASM                                            CP  1470
      GOTO 17                                                           CP  1480
  400 ALFA = 2.                                                         CP  1490
      ISTR = 9                                                          CP  1500
      GOTO 23                                                           CP  1510
  500 ALFA = 1.                                                         CP  1520
      ISTR = 1                                                          CP  1530
      GOTO 23                                                           CP  1540
  600 ALFA = 2.                                                         CP  1550
      ISTR = 1                                                          CP  1560
   23 G1 = GEW(2) + ALFA * GEW(ISTR)                                    CP  1570
      Z1 = 1. / ANR - 1. / BETA                                         CP  1580
      Z2 = 1. / BETA - 1.                                               CP  1590
      GAMA1 = G1 * Z1                                                   CP  1600
      GAMA2 = GEW(3) + GEW(4) * Z2 + ALFA * GEW(ISTR) / BETA            CP  1610
      GAMA = GAMA1 / (GAMA1+GAMA2)                                      CP  1620
      ROBR = 1. / (GAMA/ROBR1+(1.-GAMA)/ROBR2)                          CP  1630
      GBR = ANR * (GEW(2)*Z1+GEW(3)+GEW(4)*Z2) + ALFA * GEW(ISTR)       CP  1640
      FAKMOL = 1. - ALFA * GEW(ISTR) / GBR                              CP  1650
      ASM = AVO * ROBR / GBR                                            CP  1660
      A(3,1) = ANR * ASM                                                CP  1670
      A(4,1) = Z2 * A(3,1)                                              CP  1680
      A(2,1) = Z1 * A(3,1)                                              CP  1690
      A(ISTR,1) = ALFA * ASM                                            CP  1700
      GOTO 17                                                           CP  1710
  700 ANR = PU(1) + PU(3)                                               CP  1720
      GBR = GEW(9) + GEW(9)                                             CP  1730
      DO 701 I=1,4                                                      CP  1740
        GBR = GBR + PU(I) * GEW(I+4)                                    CP  1750
  701 CONTINUE                                                          CP  1760
      FAKMOL = 1. - 2. * GEW(9) / GBR                                   CP  1770
      ASM = AVO * ROBR1 / GBR                                           CP  1780
      DO 703 I=1,4                                                      CP  1790
        A(I+4,1) = ASM * PU(I)                                          CP  1800
  703 CONTINUE                                                          CP  1810
      A(9,1) = ASM + ASM                                                CP  1820
      ROBR = ROBR1                                                      CP  1830
      GOTO 17                                                           CP  1840
  800 G1 = GEW(2) + 2. * GEW(9)                                         CP  1850
      G2 = 0.                                                           CP  1860
      DO 801 I=1,4                                                      CP  1870
        G2 = G2 + GEW(I+4) * PU(I)                                      CP  1880
  801 CONTINUE                                                          CP  1890
      GAMA1 = G1 * ((PU(1)+PU(3))/ANR-1.)                               CP  1900
      GAMA2 = G2 + 2. * GEW(9)                                          CP  1910
      GAMA = GAMA1 / (GAMA1+GAMA2)                                      CP  1920
      ROBR = 1. / (GAMA/ROBR1+(1.-GAMA)/ROBR2)                          CP  1930
      XXPU = ANR / (PU(1)+PU(3))                                        CP  1940
      GBR = G2 * XXPU + GEW(2) * (1.-XXPU) + 2. * GEW(9)                CP  1950
      FAKMOL = 1. - 2. * GEW(9) / GBR                                   CP  1960
      ASM = AVO * ROBR / GBR                                            CP  1970
      DO 802 I=1,4                                                      CP  1980
        A(I+4,1) = PU(I) * ASM * XXPU                                   CP  1990
  802 CONTINUE                                                          CP  2000
      A(2,1) = (1.-XXPU) * ASM                                          CP  2010
      A(9,1) = 2. * ASM                                                 CP  2020
      GOTO 17                                                           CP  2030
  900 CONTINUE                                                          CP  2040
      OMEGA = (ANR-BETA) / (PU(1)+PU(3)-ANR)                            CP  2050
      WRITE (6,2009) OMEGA,ANR,PU(1),PU(3),BETA                         CP  2060
      GPU = 0.                                                          CP  2070
      DO 901 I=1,4                                                      CP  2080
        GPU = GPU + PU(I) * (GEW(I+4)+2.*GEW(9))                        CP  2090
  901 CONTINUE                                                          CP  2100
      GU = BETA * (GEW(3)+2.*GEW(9)) + (1.-BETA) * (GEW(4)+2.*GEW(9))   CP  2110
      GAMPU = GPU / (GPU+GU/OMEGA)                                      CP  2120
      OO = OMEGA / (OMEGA+1.)                                           CP  2130
      ROBR = 1. / (GAMPU/ROBR1+(1.-GAMPU)/ROBR2)                        CP  2140
      GBR = GU / (1.+OMEGA) + GPU * OO                                  CP  2150
      ASM = AVO * ROBR / GBR                                            CP  2160
      DO 902 I=1,4                                                      CP  2170
        A(I+4,1) = ASM * OO * PU(I)                                     CP  2180
  902 CONTINUE                                                          CP  2190
      A(3,1) = ASM * (1.-OO) * BETA                                     CP  2200
      A(4,1) = ASM * (1.-OO) * (1.-BETA)                                CP  2210
      A(9,1) = (A(3,1)+A(4,1)+A(5,1)+A(6,1)+A(7,1)+A(8,1)) * 2.         CP  2220
      FAKMOL = 1. - 2. * GEW(9) / GBR                                   CP  2230
   17 WRITE (6,2003)                                                    CP  2240
      DO 18 I=1,10                                                      CP  2250
        IF(NIS(I) .EQ. 0) GOTO 18                                       CP  2260
        WRITE (6,2004) NGAM(I),ANUKL(I),(A(I,J),J=1,2)                  CP  2270
   18 CONTINUE                                                          CP  2280
      VCP = V(1) + V(2)                                                 CP  2290
      WRITE (6,2006) V(1),V(2)                                          CP  2300
      GSMCP = ASM * GBR * FAKMOL * V(1) / AVO                           CP  2310
      GSPCP = A(3,1) * GEW(3) * V(1) / AVO                              CP  2320
      IF(INDBS .EQ. 7 .OR. INDBS .EQ. 8) GSPCP = (A(5,1)*GEW(5)+A(7,1)* CP  2330
     1 GEW(7)) * V(1) / AVO                                             CP  2340
      IF(INDBS .EQ. 9) GSPCP = (A(5,1)*GEW(5)+A(7,1)*GEW(7)+A(3,1)*     CP  2350
     1 GEW(3)) * V(1) / AVO                                             CP  2360
      RETURN                                                            CP  2370
C                                                                       CP  2380
 1000 FORMAT (18I4)                                                     CP  2390
 1001 FORMAT (6E12.5)                                                   CP  2400
 2000 FORMAT (1H /1H ,10X,'FUEL = ',A8//11X,'COATED PARTICLES - DATA'//)CP  2410
 2001 FORMAT (20X,'PART.RADIUS (CM) = ',E12.5,10X,'RO1,RO2    (G/CC) = 'CP  2420
     1 ,2E12.5//20X,'COATING-LAYERS:')                                  CP  2430
 2002 FORMAT (20X,'THICKNESS   (CM) = ',E12.5,10X,'DENSITY    (G/CC) = 'CP  2440
     1 ,E12.5,5X,A4)                                                    CP  2450
 2003 FORMAT (1H /1H ,8X,'GAMNO  ISOTOP   PARTICLE       COATING'//)    CP  2460
 2004 FORMAT (I14,2X,A4,2E14.5)                                         CP  2470
 2005 FORMAT (/10X,'ATOMS-FRACTION OF PU: PU-239 = ',F6.4/32X,'PU-240 = CP  2480
     1',F6.4/32X,'PU-241 = ',F6.4/32X,'PU-242 = ',F6.4)                 CP  2490
 2006 FORMAT (1H /1H ,11X,'VOLUME  ',2E14.5//)                          CP  2500
 2007 FORMAT (1H /1H ,10X,'FISSILE ENRICHMENT (N-FISS/N-HM) =',E15.5//) CP  2510
 2009 FORMAT (1H ,'OMEGA=',E12.5,' TOT.ENRCH.=',E12.5,' PU39/PU41',E12.5CP  2520
     1 ,'/',E12.5,' U-ENRICH. =',E12.5//)                               CP  2530
      END                                                               CP  2540
      SUBROUTINE KUGEL(NFOLG,*)                                         KUG   10
C                                                                       KUG   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             KUG   30
C                                                                       KUG   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  KUG   50
C                                                                       KUG   60
      COMMON /KUDAT/ ROMTX,ROSCH,QR1,QR2,QROSM,QFF1,QINDBK,QBK,QVMOD    KUG   70
C                                                                       KUG   80
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, KUG   90
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     KUG  100
C                                                                       KUG  110
      COMMON /AAAA/ AAA(100)                                            KUG  120
C                                                                       KUG  130
      COMMON /COSTS/ KOSTDA,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)KUG  140
     1 ,FRF(4),FABKOS,AUFKOS,DK                                         KUG  150
C                                                                       KUG  160
      COMMON /RVERH/ ROVERH,CBK,ROBK                                    KUG  170
C                                                                       KUG  180
      CHARACTER*4 ANUKL                                                 KUG  190
C                                                                       KUG  200
C                                                                       KUG  210
      VOL(X1,X2) = (X2**3-X1**3) * 4. * PI / 3.                         KUG  220
      RAD(VV) = EXP(ALOG(VV*0.75/PI)/3.)                                KUG  230
      DO 98 I=3,10                                                      KUG  240
        V(I) = 0.                                                       KUG  250
        DO 98 J=1,10                                                    KUG  260
          A(J,I) = 0.                                                   KUG  270
   98 CONTINUE                                                          KUG  280
      CBK = 0.                                                          KUG  290
      WRITE (6,2000)                                                    KUG  300
      IF(NFOLG .EQ. 0) GOTO 99                                          KUG  310
C                                                                       KUG  320
CARD DZ 8                                                               KUG  330
C                                                                       KUG  340
      READ (5,1001) QR1,QR2,QFF1,QVMOD,QINDBK,QBK                       KUG  350
C                                                                       KUG  360
CARD DZ 9                                                               KUG  370
C                                                                       KUG  380
      READ (5,1001) QROSM,ROMTX,ROSCH,ROBK,QR0                          KUG  390
C                                                                       KUG  400
      IF(QFF1 .EQ. 0. .AND. QVMOD .EQ. 0. .AND. QROSM .EQ. 0.) QROSM =  KUG  410
     1 1.E-5                                                            KUG  420
      IF(QR1 .EQ. 0. .AND. QVMOD .EQ. 0.) QR1 = 2.5                     KUG  430
      IF(QINDBK .LE. 0.) WRITE (6,2001) ROMTX,ROSCH,ROBK                KUG  440
      IF(ROBK .LE. 0.) ROBK = ROSCH                                     KUG  450
      IF(QINDBK .GT. 0.) WRITE (6,2001) ROMTX,ROSCH,ROBK                KUG  460
      ROVERH = ROBK / ROSCH                                             KUG  470
      IF(ROBK .GT. 0. .AND. ROBK .NE. ROSCH) CBK = 1.                   KUG  480
   99 CONTINUE                                                          KUG  490
      R1 = QR1                                                          KUG  500
      R2 = QR2                                                          KUG  510
      ROSM = QROSM                                                      KUG  520
      FF1 = QFF1                                                        KUG  530
      BK = QBK                                                          KUG  540
      IF(BK .GE. 1.) BK = 0.99999                                       KUG  550
      VMOD = QVMOD                                                      KUG  560
      IF(QR1 .LE. 0. .OR. QR0 .LE. 0.) GOTO 20                          KUG  570
      R1 = (QR1**3.-QR0**3.)**(1./3.)                                   KUG  580
   20 CONTINUE                                                          KUG  590
      ALFA = FAKMOL * ROBR * V(1) / VCP                                 KUG  600
      A(1,3) = AVO * ROMTX / GEW(1)                                     KUG  610
      A(1,4) = AVO * ROSCH / GEW(1)                                     KUG  620
      IF(R1 .EQ. 0.) GOTO 2                                             KUG  630
      V(3) = VOL(0.,R1)                                                 KUG  640
      IF(R2 .EQ. 0.) GOTO 2                                             KUG  650
      V(4) = VOL(R1,R2)                                                 KUG  660
      IF(ROSM .EQ. 0. .AND. FF1 .EQ. 0.) GOTO 1                         KUG  670
      IF(ROSM .EQ. 0.) ROSM = ALFA * FF1                                KUG  680
      IF(FF1 .EQ. 0.) FF1 = ROSM / ALFA                                 KUG  690
      GAMA = A(1,2) * V(2) * V(3) * FF1 / VCP + A(1,3) * V(3) * (1.-FF1)KUG  700
     1 + A(1,4) * V(4)                                                  KUG  710
      IF(QINDBK .EQ. 0.) GOTO 5                                         KUG  720
      WRITE (6,2004)                                                    KUG  730
      BETA = BK / (1.-BK)                                               KUG  740
      VBK = V(3) + V(4)                                                 KUG  750
      ABK = AVO * ROBK / GEW(1)                                         KUG  760
      IF(QBK .EQ. 0.) GOTO 4                                            KUG  770
      GAMA = GAMA + BETA * ABK * VBK                                    KUG  780
      GOTO 5                                                            KUG  790
    4 IF(VMOD .NE. 0.) GOTO 6                                           KUG  800
      WRITE (6,2005)                                                    KUG  810
      STOP                                                              KUG  820
    6 CONTINUE                                                          KUG  830
      BETA = (VMOD*ASM*V(1)*FF1*V(3)/VCP-GAMA) / (VBK*ABK)              KUG  840
      BK = BETA / (1.+BETA)                                             KUG  850
      GOTO 10                                                           KUG  860
    5 VMOD = GAMA / (ASM*V(1)*FF1*V(3)/VCP)                             KUG  870
      GOTO 10                                                           KUG  880
    1 FF1 = (A(1,3)*V(3)+A(1,4)*V(4)) / ((VMOD*ASM*V(1)-A(1,2)*V(2))*   KUG  890
     1 V(3)/VCP+A(1,3)*V(3))                                            KUG  900
      ROSM = ALFA * FF1                                                 KUG  910
      GOTO 10                                                           KUG  920
    2 IF(ROSM .EQ. 0.) ROSM = ALFA * FF1                                KUG  930
      IF(FF1 .EQ. 0.) FF1 = ROSM / ALFA                                 KUG  940
      IF(R1 .EQ. 0.) GOTO 3                                             KUG  950
      V(4) = ((VMOD*ASM*V(1)-A(1,2)*V(2))*V(3)*FF1/VCP-A(1,3)*V(3)*     KUG  960
     1 (1.-FF1)) / A(1,4)                                               KUG  970
      BETA = V(4) + V(3)                                                KUG  980
      R2 = RAD(BETA)                                                    KUG  990
      GOTO 10                                                           KUG 1000
    3 ALFA = VOL(0.,R2)                                                 KUG 1010
      V(3) = A(1,4) * ALFA / ((VMOD*ASM*V(1)-A(1,2)*V(2))*FF1/VCP+A(1,4)KUG 1020
     1 -A(1,3)*(1.-FF1))                                                KUG 1030
      R1 = RAD(V(3))                                                    KUG 1040
      V(4) = VOL(0.,R2) - V(3)                                          KUG 1050
   10 CONTINUE                                                          KUG 1060
      ALFA = FF1 * V(3) / VCP                                           KUG 1070
      GSM = ALFA * GSMCP                                                KUG 1080
      GSP = ALFA * GSPCP                                                KUG 1090
      VKUGEL = V(3) + V(4)                                              KUG 1100
      RKUGEL = R2                                                       KUG 1110
      WRITE (6,2009) GSM,GSP                                            KUG 1120
      RSH = R1                                                          KUG 1130
      IF(QR0 .LE. 0.) GOTO 21                                           KUG 1140
      RSH = (R1**3.+QR0**3.)**(1./3.)                                   KUG 1150
      WRITE (6,1999) QR0                                                KUG 1160
   21 CONTINUE                                                          KUG 1170
      WRITE (6,2002) RSH,R2,ROSM,FF1,VMOD                               KUG 1180
      IF(QINDBK .NE. 0.) WRITE (6,2006) BK                              KUG 1190
      ZKUKGS = 1000. / GSM                                              KUG 1200
C                                                                       KUG 1210
C     FUELLFAKTOR DER CP IN BRENNSTOFF -ZONE                            KUG 1220
C                                                                       KUG 1230
      AAA(73) = FF1                                                     KUG 1240
      RETURN 1                                                          KUG 1250
C                                                                       KUG 1260
 1001 FORMAT (6E12.5)                                                   KUG 1270
 1999 FORMAT (10X,'SHELL BALL, R0            (CM) =',E13.5)             KUG 1280
 2000 FORMAT (10X,'SPHERICAL ELEMENTS'/)                                KUG 1290
 2001 FORMAT (10X,'DENSITY C-MATRIX        (G/CC) =',E12.5/10X,'DENSITY KUG 1300
     1C-SHELL         (G/CC) =',E12.5/10X,'DENSITY C-DUMMY ELEMENTS(G/CCKUG 1310
     2) =',E12.5/)                                                      KUG 1320
 2002 FORMAT (10X,'RADIUS OF FUEL-ZONE       (CM) =',E13.5/10X,'RADIUS OKUG 1330
     1F BALL            (CM) =',E13.5/10X,'DENSITY HM IN FUEL-ZONE (G/CCKUG 1340
     2) =',E13.5/10X,'FFACTOR CP IN FUEL-ZONE        =',E13.5/10X,'MODERKUG 1350
     3ATION RATIO  (C-AT/HM-AT) =',E13.5)                               KUG 1360
 2003 FORMAT (10X,'VOLUME:                 MATRIX =',E13.5/10X,'        KUG 1370
     1                 SHELL =',E13.5/10X,'GRAPHITE PER BALL           GKUG 1380
     2R =',E13.5/10X,'BALLS PER KG-HM                =',E13.5/10X,'ASSUMKUG 1390
     4ED FIMA                   =',E13.5/)                              KUG 1400
 2004 FORMAT (10X,'SYSTEM WITH DUMMY BALLS'/)                           KUG 1410
 2005 FORMAT (1H /1H /1H ,'IN INPUT VMOD OR BK IS MISSING')             KUG 1420
 2006 FORMAT (10X,'FRACT.OF DUMMY BALLS (NDB/NTOT)=',E13.5/)            KUG 1430
 2009 FORMAT (10X,'ONE BALL CONTAINS ',E12.5,'  GR. HM AND ',E12.5,'  GRKUG 1440
     1. FISSILE MATERIAL'/)                                             KUG 1450
      END                                                               KUG 1460
      SUBROUTINE STAB(NFOLG,*)                                          STA   10
C                                                                       STA   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             STA   30
C                                                                       STA   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  STA   50
C                                                                       STA   60
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             STA   70
C                                                                       STA   80
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, STA   90
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     STA  100
C                                                                       STA  110
      COMMON /AAAA/ AAA(100)                                            STA  120
C                                                                       STA  130
      COMMON /UI/ FF3,VMOD1,VMOD3                                       STA  140
C                                                                       STA  150
      CHARACTER*4 ANUKL                                                 STA  160
C                                                                       STA  170
C                                                                       STA  180
      VOL(X1,X2) = PI * (X2**2-X1**2)                                   STA  190
      ITZAEL = 0                                                        STA  200
      ITFUEL = 0                                                        STA  210
      QR0 = 0.                                                          STA  220
      RSH = 0.                                                          STA  230
   22 CONTINUE                                                          STA  240
      DO 98 I=3,10                                                      STA  250
        V(I) = 0.                                                       STA  260
        DO 98 J=1,10                                                    STA  270
          A(J,I) = 0.                                                   STA  280
   98 CONTINUE                                                          STA  290
      IF(ITFUEL .NE. 0) GOTO 99                                         STA  300
      WRITE (6,2000)                                                    STA  310
      IF(NFOLG .EQ. 0) GOTO 99                                          STA  320
      DO 1 I=1,5                                                        STA  330
        R(I) = 0.                                                       STA  340
    1 CONTINUE                                                          STA  350
C                                                                       STA  360
CARD DZ 10                                                              STA  370
C                                                                       STA  380
      READ (5,1000) (R(I),I=1,6)                                        STA  390
C                                                                       STA  400
CARD DZ 11                                                              STA  410
C                                                                       STA  420
      READ (5,1000) FF1,VMOD,BETA,GKAN,FFUEL,ACTIV                      STA  430
C                                                                       STA  440
CARD DZ 12                                                              STA  450
C                                                                       STA  460
      READ (5,1000) QROSM,ROMTX,ROSTR,ROHR                              STA  470
C                                                                       STA  480
      IF(FF1 .GE. 0.) GOTO 20                                           STA  490
      ITFUEL = ITFUEL + 1                                               STA  500
      ITZAEL = ITFUEL                                                   STA  510
      FF1 = ABS(FF1)                                                    STA  520
      VMOD1 = VMOD                                                      STA  530
      VMOD = 0.                                                         STA  540
   20 CONTINUE                                                          STA  550
      BETA = 1. / (1.-BETA)                                             STA  560
      IF(FFUEL .EQ. 0.) GOTO 99                                         STA  570
      R(2) = R(1) + R(2)                                                STA  580
      R(3) = R(2) + R(3)                                                STA  590
      R(4) = SQRT(R(3)**2+FFUEL/PI)                                     STA  600
      R(5) = R(4) + R(5)                                                STA  610
      R(6) = R(5) + R(6)                                                STA  620
   99 CONTINUE                                                          STA  630
      IF(R(4) .EQ. 0. .OR. R(6) .EQ. 0.) GOTO 8000                      STA  640
      FFUEL = PI * (R(4)**2-R(3)**2)                                    STA  650
      ROSM = QROSM                                                      STA  660
      ALFA = FAKMOL * ROBR * V(1) / VCP                                 STA  670
      IF(ROSM .EQ. 0.) ROSM = ALFA * FF1                                STA  680
      IF(FF1 .EQ. 0.) FF1 = ROSM / ALFA                                 STA  690
      ALFA = AVO / GEW(1)                                               STA  700
      X1 = 0.                                                           STA  710
      DO 2 I=1,6                                                        STA  720
        X2 = R(I)                                                       STA  730
        IF(X2 .EQ. 0.) GOTO 13                                          STA  740
        V(I+2) = VOL(X1,X2)                                             STA  750
        IF(I .EQ. 2 .OR. I .EQ. 6) GOTO 13                              STA  760
        A(1,I+2) = ALFA * ROHR                                          STA  770
        IF(I .EQ. 4) A(1,I+2) = ALFA * ROMTX * (1.-FF1)                 STA  780
   13   X1 = X2                                                         STA  790
    2 CONTINUE                                                          STA  800
      IF(ITFUEL .GT. 0) GOTO 23                                         STA  810
      IF(NFOLG .NE. 0) WRITE (6,2001) ROMTX,ROSTR,ROHR                  STA  820
      IF(R(1) .EQ. 0.) GOTO 3                                           STA  830
      WRITE (6,2002)                                                    STA  840
      WRITE (6,2020) R(1)                                               STA  850
      IF(R(2) .EQ. 0.) GOTO 6                                           STA  860
      WRITE (6,2021) R(2)                                               STA  870
      WRITE (6,2022) R(3)                                               STA  880
      GOTO 6                                                            STA  890
    3 IF(R(2) .EQ. 0.) GOTO 4                                           STA  900
      WRITE (6,2003)                                                    STA  910
      WRITE (6,2021) R(2)                                               STA  920
      WRITE (6,2022) R(3)                                               STA  930
      GOTO 6                                                            STA  940
    4 IF(R(3) .EQ. 0.) GOTO 5                                           STA  950
      WRITE (6,2004)                                                    STA  960
      WRITE (6,2020) R(3)                                               STA  970
      GOTO 6                                                            STA  980
    5 WRITE (6,2005)                                                    STA  990
    6 WRITE (6,2023) R(4),R(5),R(6),FFUEL                               STA 1000
   23 VSTR = V(3) + V(5) + V(7)                                         STA 1010
      VSTAB = V(3) + V(4) + V(5) + V(6) + V(7)                          STA 1020
      A(1,6) = A(1,6) + (A(1,1)*V(1)+A(1,2)*V(2)) * FF1 / VCP           STA 1030
      FAKTOR = V(1) * FF1 / VCP                                         STA 1040
      DO 7 I=2,8                                                        STA 1050
        IF(NIS(I) .EQ. 0) GOTO 7                                        STA 1060
        A(I,6) = A(I,1) * FAKTOR                                        STA 1070
    7 CONTINUE                                                          STA 1080
      IF(NIS(9) .NE. 0) A(9,6) = A(9,1) * FAKTOR                        STA 1090
      IF(NIS(10) .NE. 0) A(10,6) = A(10,2) * V(2) * FF1 / VCP           STA 1100
      SUM = 0.                                                          STA 1110
      DO 9 I=2,8                                                        STA 1120
        SUM = SUM + A(I,6)                                              STA 1130
    9 CONTINUE                                                          STA 1140
      SUM = V(6) * SUM                                                  STA 1150
      SUMC = 0.                                                         STA 1160
      DO 10 J=3,7                                                       STA 1170
        SUMC = SUMC + A(1,J) * V(J)                                     STA 1180
   10 CONTINUE                                                          STA 1190
      A(1,9) = A(1,7) * ROSTR / ROHR                                    STA 1200
      IF(VMOD .EQ. 0.) GOTO 11                                          STA 1210
      V(9) = (VMOD*SUM-SUMC) * BETA / A(1,9)                            STA 1220
      VZELL = V(8) + VSTAB + V(9)                                       STA 1230
      GKAN = 1.E+04 / VZELL                                             STA 1240
      GOTO 12                                                           STA 1250
   11 VZELL = 1.E+04 / GKAN                                             STA 1260
      V(9) = VZELL - VSTAB - V(8)                                       STA 1270
      VMOD = (SUMC+A(1,9)*V(9)/BETA) / SUM                              STA 1280
   12 CONTINUE                                                          STA 1290
      IF(ITFUEL .LE. 0) GOTO 30                                         STA 1300
      VM = ABS((VMOD-VMOD1)/VMOD1)                                      STA 1310
      IF(VM .GT. 0.0001) GOTO 24                                        STA 1320
      ITFUEL = -1                                                       STA 1330
      VMOD = 0.                                                         STA 1340
      GOTO 22                                                           STA 1350
   24 FF = FF1                                                          STA 1360
      IF(ITFUEL .EQ. 1) GOTO 25                                         STA 1370
      VVERH = (VMOD1-VMOD) / (VMOD3-VMOD)                               STA 1380
      FF1 = FF + (FF3-FF) * VVERH                                       STA 1390
   25 FF3 = FF                                                          STA 1400
      VMOD3 = VMOD                                                      STA 1410
      VMOD = 0.                                                         STA 1420
      IF(ITFUEL .EQ. 1) FF1 = FF1 * 0.9                                 STA 1430
      ITFUEL = ITFUEL + 1                                               STA 1440
      ITZAEL = ITFUEL                                                   STA 1450
      GOTO 22                                                           STA 1460
   30 A(1,9) = A(1,9) / BETA                                            STA 1470
      V(10) = VZELL                                                     STA 1480
      RZELL = SQRT(VZELL/PI)                                            STA 1490
      R(7) = RZELL                                                      STA 1500
      WRITE (6,2006) ROSM,FF1,VMOD,RZELL,GKAN                           STA 1510
      AAA(73) = FF1                                                     STA 1520
      IF(ITZAEL .GT. 0) WRITE (6,3000) ITZAEL                           STA 1530
      WRITE (6,2007)                                                    STA 1540
      DO 8 I=1,10                                                       STA 1550
        IF(NIS(I) .EQ. 0) GOTO 8                                        STA 1560
        WRITE (6,2008) NGAM(I),ANUKL(I),(A(I,J),J=6,7),A(I,9)           STA 1570
    8 CONTINUE                                                          STA 1580
      WRITE (6,2009) V(6),VSTR                                          STA 1590
      GSM = GSMCP                                                       STA 1600
      GSP = GSPCP                                                       STA 1610
      RETURN 1                                                          STA 1620
 8000 WRITE (6,2800)                                                    STA 1630
      STOP                                                              STA 1640
C                                                                       STA 1650
 1000 FORMAT (6E12.5)                                                   STA 1660
 2000 FORMAT (1H /1H ,10X,'PRISMATIC FUEL ELEMENTS, TYPE DRAGON'///)    STA 1670
 2001 FORMAT (10X,'C-DENSITIES: MATRIX =',E12.5,4X,'MODERATOR =',E12.5, STA 1680
     1 4X,'ENVELOPE PIPE =',E12.5//)                                    STA 1690
 2002 FORMAT (10X,'HOLLOW ROD WITH CORE'//)                             STA 1700
 2003 FORMAT (10X,'HOLLOW ROD'//)                                       STA 1710
 2004 FORMAT (10X,'ANNULAR ROD'//)                                      STA 1720
 2005 FORMAT (10X,'SOLID ROD'//)                                        STA 1730
 2006 FORMAT (10X,'DENSITY HM IN FUEL-ZONE.(G/CC) =',E13.5/10X,'FFACTOR STA 1740
     1CP IN FUEL-ZONE        =',E13.5/10X,'MODERATION RATIO  (C-AT/HM-ATSTA 1750
     2) =',E13.5/10X,'CELL RADIUS               (CM) =',E13.5/10X,'NUMBESTA 1760
     3R OF CHANNELS/M**2        =',E13.5)                               STA 1770
 2007 FORMAT (1H /1H ,8X,'GAMNO  ISOTOP   FUEL-ZONE     ENV. PIPE     MOSTA 1780
     1DERATOR'//)                                                       STA 1790
 2008 FORMAT (I14,2X,A4,3E14.5)                                         STA 1800
 2009 FORMAT (1H /1H ,11X,'VOLUME  ',2E14.5//)                          STA 1810
 2020 FORMAT (10X,'RADIUS OF CORE            (CM) =',E13.5)             STA 1820
 2021 FORMAT (10X,'RADIUS OF INNER CHANNEL   (CM) =',E13.5)             STA 1830
 2022 FORMAT (10X,'RADIUS OF INNER ENV.PIPE  (CM) =',E13.5)             STA 1840
 2023 FORMAT (10X,'RADIUS OF FUEL ZONE       (CM) =',E13.5/10X,'RADIUS OSTA 1850
     1F EXTERN.ENV.PIPE (CM) =',E13.5/10X,'RADIUS OF EXTERN. CHANNEL (CMSTA 1860
     2) =',E13.5/10X,'FUEL - AREA (CM**2)            =',E13.5)          STA 1870
 2800 FORMAT (1H /1H /1H ,10X,'INPUT-ERROR IN SUBROUTINE "STAB"'/10X,'R(STA 1880
     14) OR R(6) IS 0.')                                                STA 1890
 3000 FORMAT (/' (',I3,' ITERATION STEPS NEEDED TO CALCULATE THE FILLINGSTA 1900
     1 FACTOR FOR ABOVE NAMED MODERATION RATIO)')                       STA 1910
      END                                                               STA 1920
      SUBROUTINE GAM1(NTYP)                                             GAM   10
C                                                                       GAM   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             GAM   30
C                                                                       GAM   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  GAM   50
C                                                                       GAM   60
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, GAM   70
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     GAM   80
C                                                                       GAM   90
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             GAM  100
C                                                                       GAM  110
      COMMON /GAMD/ CEG(33),AL1(33),IDSATZ(12),NOID,NTBD,NRES,IDRES,WTP,GAM  120
     1 WSTU,WAG,NOAG                                                    GAM  130
C                                                                       GAM  140
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO                                 GAM  150
C                                                                       GAM  160
      COMMON /AAAA/ AAA(100)                                            GAM  170
C                                                                       GAM  180
      COMMON /RVERH/ ROVERH                                             GAM  190
C                                                                       GAM  200
      CHARACTER*4 ANUKL                                                 GAM  210
C                                                                       GAM  220
      DIMENSION AGAM(10)                                                GAM  230
C                                                                       GAM  240
C                                                                       GAM  250
      WRITE (6,2000)                                                    GAM  260
      GOTO(1,2,3),NTYP                                                  GAM  270
    1 CONTINUE                                                          GAM  280
      A(1,3) = A(1,3) * (1.-FF1)                                        GAM  290
      DO 10 I=1,10                                                      GAM  300
        AGAM(I) = 0.                                                    GAM  310
        IF(NIS(I) .EQ. 0) GOTO 10                                       GAM  320
        DO 11 J=1,2                                                     GAM  330
          AGAM(I) = AGAM(I) + A(I,J) * V(J)                             GAM  340
   11   CONTINUE                                                        GAM  350
        AGAM(I) = AGAM(I) * FF1 * V(3) / VCP                            GAM  360
        DO 12 J=3,4                                                     GAM  370
          AGAM(I) = AGAM(I) + A(I,J) * V(J)                             GAM  380
   12   CONTINUE                                                        GAM  390
        AGAM(I) = AGAM(I) * FF2 / VKUGEL                                GAM  400
        AGAM(I) = AGAM(I) * (1.-BK)                                     GAM  410
        IF(I .EQ. 1) AGAM(I) = AGAM(I) + A(1,4) * FF2 * BK * ROVERH     GAM  420
   10 CONTINUE                                                          GAM  430
      A(1,3) = A(1,3) / (1.-FF1)                                        GAM  440
      GOTO 100                                                          GAM  450
    2 CONTINUE                                                          GAM  460
      VZELL = V(10)                                                     GAM  470
      DO 20 I=1,10                                                      GAM  480
        AGAM(I) = 0.                                                    GAM  490
        IF(NIS(I) .EQ. 0) GOTO 20                                       GAM  500
        DO 21 J=3,9                                                     GAM  510
          AGAM(I) = AGAM(I) + A(I,J) * V(J) / VZELL                     GAM  520
   21   CONTINUE                                                        GAM  530
   20 CONTINUE                                                          GAM  540
      GOTO 100                                                          GAM  550
    3 RETURN                                                            GAM  560
  100 CONTINUE                                                          GAM  570
      WRITE (6,2003)                                                    GAM  580
      DO 200 I=1,10                                                     GAM  590
        IF(NIS(I) .EQ. 0) GOTO 200                                      GAM  600
        WRITE (6,2004) NGAM(I),ANUKL(I),AGAM(I)                         GAM  610
C                                                                       GAM  620
C     DIE GAM-NUMMERN IN REAL-DARSTELLUNG UND DIE GAM-DICHTEN           GAM  630
C                                                                       GAM  640
        AAA(I) = FLOAT(NGAM(I))                                         GAM  650
        AAA(I+10) = AGAM(I)                                             GAM  660
  200 CONTINUE                                                          GAM  670
      RETURN                                                            GAM  680
C                                                                       GAM  690
 2000 FORMAT (// 10X,'G A M - 1  -  D A T A'/)                          GAM  700
 2003 FORMAT (10X,'GAMNO  ISOTOP   AT/CM*BARN')                         GAM  710
 2004 FORMAT (I13,4X,A4,E15.5)                                          GAM  720
      END                                                               GAM  730
      SUBROUTINE THERMD(NTYP)                                           THE   10
C                                                                       THE   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             THE   30
C                                                                       THE   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  THE   50
C                                                                       THE   60
      COMMON /CPDAT/ ANR,INDBS,NCT,NSIC1,NSIC2,KQ,PU(4),DCT(5),ROCT(5), THE   70
     1 RK,ROBR1,ROBR2,BETA                                              THE   80
C                                                                       THE   90
      COMMON /STADAT/ R(7),DUMY1,ROMTX,DUMY3,DUMY4,DUMY5,DUMY6          THE  100
C                                                                       THE  110
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, THE  120
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     THE  130
C                                                                       THE  140
      COMMON /THER/ MTBL(20),LIM(30),ID,NX,NHET,NTHGR,MX,IDC,IDO,TTEMP, THE  150
     1 MMX                                                              THE  160
C                                                                       THE  170
      COMMON /AAAA/ AAA(100)                                            THE  180
C                                                                       THE  190
      COMMON /RVERH/ ROVERH,CBK                                         THE  200
C                                                                       THE  210
      CHARACTER*4 ANUKL                                                 THE  220
C                                                                       THE  230
      DIMENSION NISG(10),NIST(10),VVV(5),ATHER(10,5),D(5)               THE  240
C                                                                       THE  250
C                                                                       THE  260
      RAD(VV) = EXP(ALOG(VV*0.75/PI)/3.)                                THE  270
      WRITE (6,2000)                                                    THE  280
      IF(NHET .NE. 0) WRITE (6,2002)                                    THE  290
      NN = 0                                                            THE  300
      MSHK = 0                                                          THE  310
      N90 = 90                                                          THE  320
      IBZ1 = 0                                                          THE  330
      MMZ = 0                                                           THE  340
      FAKTOR = 1.                                                       THE  350
      DO 5 I=1,10                                                       THE  360
        IF(NIS(I) .EQ. 0)GOTO 5                                         THE  370
        NN = NN + 1                                                     THE  380
        NISG(NN) = NGAM(I)                                              THE  390
        NIST(NN) = NGAM(I)                                              THE  400
        IF(I .EQ. 1) NIST(NN) = IDC                                     THE  410
        IF(I .EQ. 9) NIST(NN) = IDO                                     THE  420
    5 CONTINUE                                                          THE  430
      NTHER = 1                                                         THE  440
 8000 CONTINUE                                                          THE  450
      DO 6 I=1,10                                                       THE  460
        DO 6 J=1,5                                                      THE  470
          ATHER(I,J) = 0.                                               THE  480
    6 CONTINUE                                                          THE  490
      IF(NHET .NE. 0 .AND. NTHER .EQ. 1) WRITE (6,2003)                 THE  500
      IF(NHET .NE. 0 .AND. NTHER .EQ. 2) WRITE (6,2004)                 THE  510
C                                                                       THE  520
C     ATOMKONZENTRATIONEN                                               THE  530
C                                                                       THE  540
      IF(NHET .GT. 0 .AND. NTHER .EQ. 1) GOTO 114                       THE  550
C                                                                       THE  560
C     EHR,2DHR                                                          THE  570
C                                                                       THE  580
      GOTO(1,2,3),NTYP                                                  THE  590
    3 RETURN                                                            THE  600
    1 CONTINUE                                                          THE  610
      FAKTOR = FF1 * V(1) / VCP                                         THE  620
      MOLD = 0                                                          THE  630
      IF(NHET .GT. 0) MOLD = 1                                          THE  640
      MNEW = MOLD                                                       THE  650
      IBZ = 1                                                           THE  660
      GOTO 100                                                          THE  670
    2 MOLD = 0                                                          THE  680
      MNEW = 0                                                          THE  690
      IBZ = 1                                                           THE  700
      IF(R(3) .NE. 0. .OR. R(1) .NE. 0.) IBZ = 2                        THE  710
      IF(NHET .EQ. 0) GOTO 100                                          THE  720
      MOLD = 1                                                          THE  730
      MNEW = IBZ                                                        THE  740
  100 IBZ1 = IBZ + 1                                                    THE  750
      DO 10 I=2,10                                                      THE  760
        IF(I .EQ. 9) GOTO 10                                            THE  770
        IF(NIS(I) .EQ. 0) GOTO 10                                       THE  780
        GOTO(101,102),NTYP                                              THE  790
  101   ATHER(I,1) = A(I,1) * FAKTOR                                    THE  800
        IF(I .EQ. 10) ATHER(I,1) = A(I,2) * FF1 * V(2) / VCP            THE  810
        GOTO 10                                                         THE  820
  102   ATHER(I,IBZ) = A(I,6)                                           THE  830
   10 CONTINUE                                                          THE  840
      IF(NIS(9) .EQ. 0) GOTO 11                                         THE  850
      IF(IDC .LT. IDO ) GOTO 11                                         THE  860
      NIDO = 0                                                          THE  870
   12 CONTINUE                                                          THE  880
      GOTO(104,105),NTYP                                                THE  890
  104 ATHER(9,1) = A(9,1) * FAKTOR                                      THE  900
      GOTO 13                                                           THE  910
  105 ATHER(9,IBZ) = A(9,6)                                             THE  920
   13 IF(NIDO .EQ. 1) GOTO 14                                           THE  930
   11 CONTINUE                                                          THE  940
      GOTO(107,108),NTYP                                                THE  950
  107 ATHER(1,1) = A(1,1) * FAKTOR + A(1,2) * FF1 * V(2) / VCP + A(1,3) THE  960
     1 * (1.-FF1)                                                       THE  970
      ATHER(1,2) = A(1,4)                                               THE  980
      GOTO 9                                                            THE  990
  108 IF(IBZ .EQ. 2) ATHER(1,1) = (A(1,3)*V(3)+A(1,5)*V(5)) / (V(3)+V(4)THE 1000
     1 +V(5))                                                           THE 1010
      ATHER(1,IBZ) = A(1,6)                                             THE 1020
      ATHER(1,IBZ1) = (A(1,7)*V(7)+A(1,9)*V(9)) / (V(7)+V(8)+V(9))      THE 1030
    9 IF(NIS(9) .EQ. 0) GOTO 14                                         THE 1040
      NIDO = 1                                                          THE 1050
      GOTO 12                                                           THE 1060
   14 CONTINUE                                                          THE 1070
C                                                                       THE 1080
C     GEOMETRIE EHR,2DHR                                                THE 1090
C                                                                       THE 1100
      GOTO(110,111),NTYP                                                THE 1110
  110 D(1) = RSH - QR0                                                  THE 1120
C                                                                       THE 1130
C     BLINKUGELN ZUR SCHALE ZUSCHLAGEN                                  THE 1140
C                                                                       THE 1150
      VCELL = VKUGEL / (1.-BK)                                          THE 1160
      IF(CBK .EQ. 1.) GO TO 120                                         THE 1170
C                                                                       THE 1180
C     EVTL. SCHALENKUGEL                                                THE 1190
C                                                                       THE 1200
      IF(QR0 .GT. 0.) MSHK = 1                                          THE 1210
      AAA(N90+1) = 0                                                    THE 1220
      AAA(N90+2) = QR0                                                  THE 1230
      AAA(N90+3) = RSH                                                  THE 1240
      AAA(N90+4) = RAD(VCELL)                                           THE 1250
      AAA(N90+5) = BK                                                   THE 1260
      MMX = IBZ1 + MSHK                                                 THE 1270
      DO 32 J=1,MMX                                                     THE 1280
        JM = MMX + 1 - J                                                THE 1290
        D(JM) = AAA(N90+5-J) - AAA(N90+4-J)                             THE 1300
        VVV(JM) = AAA(N90+5-J)**3. - AAA(N90+4-J)**3.                   THE 1310
        JK = JM - MSHK                                                  THE 1320
        IF(JK .EQ. 0) JK = MMX                                          THE 1330
        DO 31 I=1,10                                                    THE 1340
          ATHER(I,JM) = ATHER(I,JK)                                     THE 1350
   31   CONTINUE                                                        THE 1360
   32 CONTINUE                                                          THE 1370
      GOTO 30                                                           THE 1380
  120 CONTINUE                                                          THE 1390
C                                                                       THE 1400
C     BLINDKUGELN MIT ANDERER GRAPHITDICHTE ALS IN SCHALE               THE 1410
C                                                                       THE 1420
      MMX = IBZ1 + 1                                                    THE 1430
      MMZ = MMX                                                         THE 1440
      AAA(N90+1) = 0.                                                   THE 1450
      AAA(N90+2) = RSH                                                  THE 1460
      AAA(N90+3) = R2                                                   THE 1470
      AAA(N90+4) = RAD(VCELL)                                           THE 1480
      AAA(N90+5) = BK                                                   THE 1490
      DO 51 J=1,MMX                                                     THE 1500
        JM = MMX + 1 - J                                                THE 1510
        D(JM) = AAA(N90+5-J) - AAA(N90+4-J)                             THE 1520
        VVV(JM) = AAA(N90+5-J)**3. - AAA(N90+4-J)**3.                   THE 1530
   51 CONTINUE                                                          THE 1540
      DO 52 I=1,10                                                      THE 1550
        ATHER(I,MMX) = ATHER(I,IBZ1) * ROVERH                           THE 1560
   52 CONTINUE                                                          THE 1570
      GOTO 30                                                           THE 1580
  111 CONTINUE                                                          THE 1590
      AAA(N90+2) = R(3)                                                 THE 1600
      AAA(N90+3) = R(4)                                                 THE 1610
      AAA(N90+4) = R(7)                                                 THE 1620
      GOTO(112,113),IBZ                                                 THE 1630
  112 D(1) = R(4)                                                       THE 1640
      D(2) = R(7) - R(4)                                                THE 1650
      VVV(1) = R(4)**2                                                  THE 1660
      VVV(2) = R(7)**2 - VVV(1)                                         THE 1670
      GOTO 30                                                           THE 1680
  113 D(1) = R(3)                                                       THE 1690
      D(2) = R(4) - R(3)                                                THE 1700
      D(3) = R(7) - R(4)                                                THE 1710
      VVV(1) = R(3)**2                                                  THE 1720
      VVV(2) = R(4)**2 - VVV(1)                                         THE 1730
      VVV(3) = R(7)**2 - VVV(2) - VVV(1)                                THE 1740
      GOTO 30                                                           THE 1750
  114 CONTINUE                                                          THE 1760
C                                                                       THE 1770
C     1DHR                                                              THE 1780
C                                                                       THE 1790
      VCELL = VCP / FF1                                                 THE 1800
      DO 16 I=2,10                                                      THE 1810
        IF(I .EQ. 9) GOTO 16                                            THE 1820
        IF(NIS(I) .EQ. 0) GOTO 16                                       THE 1830
        ATHER(I,1) = A(I,1)                                             THE 1840
        IF(I .EQ. 10) ATHER(I,2) = A(I,2) * V(2) / (VCELL-V(1))         THE 1850
   16 CONTINUE                                                          THE 1860
      GOTO 17                                                           THE 1870
   19 CONTINUE                                                          THE 1880
      ATHER(9,1) = A(9,1)                                               THE 1890
      IF(NIDO .EQ. 1) GOTO 18                                           THE 1900
   17 CONTINUE                                                          THE 1910
      ATHER(1,1) = A(1,1)                                               THE 1920
      GOTO(115,116),NTYP                                                THE 1930
  115 ATHER(1,2) = (A(1,2)*V(2)+A(1,3)*(VCELL-VCP)) / (VCELL-V(1))      THE 1940
      GOTO 117                                                          THE 1950
  116 ATHER(1,2) = (A(1,2)*V(2)+(AVO*ROMTX/GEW(1))*(VCELL-VCP)) / (VCELLTHE 1960
     1 -V(1))                                                           THE 1970
  117 CONTINUE                                                          THE 1980
      IF(NIS(9) .EQ. 0) GOTO 18                                         THE 1990
      NIDO = 1                                                          THE 2000
      GOTO 19                                                           THE 2010
   18 CONTINUE                                                          THE 2020
C                                                                       THE 2030
C     GEOMETRIE 1DHR                                                    THE 2040
C                                                                       THE 2050
      D(1) = RK                                                         THE 2060
      D(2) = RAD(VCELL) - D(1)                                          THE 2070
      VVV(1) = RK**3                                                    THE 2080
      VVV(2) = RAD(VCELL)                                               THE 2090
      VVV(2) = VVV(2)**3 - VVV(1)                                       THE 2100
      GOTO 30                                                           THE 2110
 8001 NTHER = 2                                                         THE 2120
      GOTO 8000                                                         THE 2130
C                                                                       THE 2140
C     DATEN AUSSCHREIBEN                                                THE 2150
C                                                                       THE 2160
   30 WRITE (6,2005)                                                    THE 2170
      MMX = IBZ1 + MSHK                                                 THE 2180
      IF(CBK .EQ. 1.) MMX = MMZ                                         THE 2190
      DO 40 J=1,MMX                                                     THE 2200
        K = 0                                                           THE 2210
        DO 40 I=1,10                                                    THE 2220
          CONCI = 0.0                                                   THE 2230
          DO 200 JJ=1,MMX                                               THE 2240
            CONCI = CONCI + ATHER(I,JJ) * VVV(JJ)                       THE 2250
  200     CONTINUE                                                      THE 2260
          IF(CONCI .NE. 0.0) CONCI = ATHER(I,J) * VVV(J) / CONCI        THE 2270
          IF(ATHER(I,J) .EQ. 0.) GOTO 40                                THE 2280
          IDNR = NGAM(I)                                                THE 2290
          K = K + 1                                                     THE 2300
          IF(K .EQ. 1) GOTO 41                                          THE 2310
          WRITE (6,2006) ANUKL(I),IDNR,ATHER(I,J),CONCI                 THE 2320
          GOTO 39                                                       THE 2330
   41     WRITE (6,2007) J,D(J),ANUKL(I),IDNR,ATHER(I,J),CONCI          THE 2340
   39     AAA(10+J*10+I) = CONCI                                        THE 2350
   40 CONTINUE                                                          THE 2360
      IF(NHET .NE. 0 .AND. NTHER .EQ. 1) GOTO 8001                      THE 2370
      RETURN                                                            THE 2380
C                                                                       THE 2390
 2000 FORMAT (//9X,'T H E R M O S  -  D A T A'/)                        THE 2400
 2002 FORMAT (10X,'DOUBLE-HETEROGENEOUS'//)                             THE 2410
 2003 FORMAT (10X,'1DHR - DATA'/10X,'==========='//)                    THE 2420
 2004 FORMAT (1H /1H /1H , 9X,'2DHR - DATA'/10X,'==========='//)        THE 2430
 2005 FORMAT (9X,'ZONE  THICKNESS(CM) ISOTOP   IDNO   ATOMS/CM*BARN   N(THE 2440
     1J)*V(J)/SUM(N(J)*V(J))')                                          THE 2450
 2006 FORMAT (30X,A4,I8,4E14.5)                                         THE 2460
 2007 FORMAT (10X,I2,E14.5,4X,A4,I8,2E14.5)                             THE 2470
      END                                                               THE 2480
      SUBROUTINE ZUT(NTYP,NRET,*)                                       ZUT   10
C                                                                       ZUT   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             ZUT   30
C                                                                       ZUT   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  ZUT   50
C                                                                       ZUT   60
      COMMON /CPDAT/ ANR,INDBS,NCT,NSIC1,NSIC2,K,PU(4),DCT(5),ROCT(5),  ZUT   70
     1 RK,ROBR1,ROBR2,DUMY                                              ZUT   80
C                                                                       ZUT   90
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, ZUT  100
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2             ZUT  110
C                                                                       ZUT  120
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             ZUT  130
C                                                                       ZUT  140
CFZJ035                                                       14.09.04  ZUT  150
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO,SPPU2,SPU5,SPPU0                ZUT  160
C                                                                       ZUT  170
      COMMON /AAAA/ AAA(90),RADIU                                       ZUT  180
C                                                                       ZUT  190
      COMMON /RVERH/ ROVERH                                             ZUT  200
C                                                                       ZUT  210
      COMMON /COSTS/ S(31),IU8TH                                        ZUT  220
C                                                                       ZUT  230
      CHARACTER*4 ANUKL                                                 ZUT  240
C                                                                       ZUT  250
C                                                                       ZUT  260
      IU81 = 0                                                          ZUT  270
      IF(IU8TH .EQ. 0) IU81 = 1                                         ZUT  280
      SPSI = 3.                                                         ZUT  290
      H = 0.                                                            ZUT  300
      SI5 = 0.                                                          ZUT  310
      RCP = RK                                                          ZUT  320
      ALPH = 0.                                                         ZUT  330
      FF3 = 0.                                                          ZUT  340
      R4 = 0.                                                           ZUT  350
      R5 = 0.                                                           ZUT  360
      SI4 = 0.                                                          ZUT  370
      SIGM1 = 0.                                                        ZUT  380
      SIGM2 = 0.                                                        ZUT  390
      DIQU1 = 0.                                                        ZUT  400
      DIQU2 = 0.                                                        ZUT  410
      DO 100 I=1,NCT                                                    ZUT  420
        RCP = RCP + DCT(I)                                              ZUT  430
  100 CONTINUE                                                          ZUT  440
      IF(IU8TH .EQ. 0) IU8TH = 1                                        ZUT  450
      GOTO(201,202,203,204,206),IU8TH                                   ZUT  460
  201 CONTINUE                                                          ZUT  470
      J = 2                                                             ZUT  480
      SIGPZ = SPTH                                                      ZUT  490
      GOTO 205                                                          ZUT  500
  202 CONTINUE                                                          ZUT  510
      J = 4                                                             ZUT  520
      SIGPZ = SPU8                                                      ZUT  530
      GOTO 205                                                          ZUT  540
  203 CONTINUE                                                          ZUT  550
      J = 8                                                             ZUT  560
      SIGPZ = SPPU2                                                     ZUT  570
      GOTO 205                                                          ZUT  580
  204 CONTINUE                                                          ZUT  590
      J = 3                                                             ZUT  600
      SIGPZ = SPU5                                                      ZUT  610
CFZJ035                                                       14.09.04  ZUT  620
  206 CONTINUE                                                          ZUT  630
      J = 6                                                             ZUT  640
      SIGPZ = SPPU0                                                     ZUT  650
  205 CONTINUE                                                          ZUT  660
      ABAR = RK                                                         ZUT  670
      DZERO = A(J,1)                                                    ZUT  680
      IF(DZERO .LE. 0.) GOTO 5                                          ZUT  690
      SIGM1 = SPC * A(1,1) / DZERO                                      ZUT  700
      SIGM2 = SPO * A(9,1) / DZERO                                      ZUT  710
      DIQU1 = A(1,1) / DZERO                                            ZUT  720
      DIQU2 = A(9,1) / DZERO                                            ZUT  730
    5 CONTINUE                                                          ZUT  740
      SI2 = A(1,2) * SPC + A(10,2) * SPSI                               ZUT  750
      GOTO(1,2,3),NTYP                                                  ZUT  760
    1 CONTINUE                                                          ZUT  770
      FF3 = FF1                                                         ZUT  780
      SI4 = A(1,4) * SPC                                                ZUT  790
      SI5 = SI4 * ROVERH                                                ZUT  800
      ALPH = A(1,3) / (A(1,2)+A(10,2))                                  ZUT  810
      R4 = R1                                                           ZUT  820
      R5 = R2                                                           ZUT  830
      H = BK                                                            ZUT  840
      GOTO 10                                                           ZUT  850
    2 CONTINUE                                                          ZUT  860
      SI4 = 0.                                                          ZUT  870
      R4 = R(4)                                                         ZUT  880
      R5 = 0.                                                           ZUT  890
      FF3 = FF1 * V(6) / (V(3)+V(4)+V(5)+V(6))                          ZUT  900
      AVS = (A(1,3)*V(3)+A(1,5)*V(5)+AVO*ROMTX*V(6)*(1.-FF1)/GEW(1)) /  ZUT  910
     1 (V(3)+V(4)+V(5)+V(6)*(1.-FF1))                                   ZUT  920
      ALPH = AVS / (A(1,2)+A(10,2))                                     ZUT  930
      GOTO 10                                                           ZUT  940
    3 CONTINUE                                                          ZUT  950
   10 CONTINUE                                                          ZUT  960
      IF(IU81 .EQ. 1) GOTO 1000                                         ZUT  970
      WRITE (6,2000)                                                    ZUT  980
      WRITE (6,2001) SIGPZ,ABAR,DZERO,SIGM1,SIGM2,RK,RCP,R4,R5,FF3,H,SI2ZUT  990
     1 ,SI4,SI5,ALPH,DIQU1,DIQU2                                        ZUT 1000
 1000 CONTINUE                                                          ZUT 1010
      AAA(74) = SIGPZ                                                   ZUT 1020
      AAA(75) = ABAR                                                    ZUT 1030
      AAA(76) = DZERO                                                   ZUT 1040
      AAA(77) = SIGM1                                                   ZUT 1050
      AAA(78) = SIGM2                                                   ZUT 1060
      AAA(79) = RK                                                      ZUT 1070
      AAA(80) = RCP                                                     ZUT 1080
      AAA(81) = R4                                                      ZUT 1090
      AAA(82) = R5                                                      ZUT 1100
      AAA(83) = FF3                                                     ZUT 1110
      AAA(84) = H                                                       ZUT 1120
      AAA(85) = SI2                                                     ZUT 1130
      AAA(86) = SI4                                                     ZUT 1140
      AAA(87) = SI5                                                     ZUT 1150
      AAA(88) = ALPH                                                    ZUT 1160
      AAA(89) = DIQU1                                                   ZUT 1170
      AAA(90) = DIQU2                                                   ZUT 1180
      IF(NRET .NE. 0) RETURN                                            ZUT 1190
      RETURN 1                                                          ZUT 1200
C                                                                       ZUT 1210
 2000 FORMAT (//10X,'Z U T - D G L  -  D A T A  (DOUBLE HET.)'/)        ZUT 1220
 2001 FORMAT (10X,'SIGPZ =',E12.5,10X,'ABAR  =',E12.5/10X,'DZERO =',    ZUT 1230
     1 E12.5,10X,'SIGM1 =',E12.5/10X,'SIGM2 =',E12.5,10X,'R1    =',E12.5ZUT 1240
     2 /10X,'R2    =',E12.5,10X,'R4    =',E12.5/10X,'R5    =',E12.5,10X,ZUT 1250
     3 'F     =',E12.5/10X,'H     =',E12.5,10X,'SI2   =',E12.5/10X,'SI4 ZUT 1260
     4  =',E12.5,10X,'SI5   =',E12.5/10X,'ALPH  =',E12.5,10X,'DIQU1 =', ZUT 1270
     5 E12.5/10X,'DIQU2 =',E12.5)                                       ZUT 1280
      END                                                               ZUT 1290