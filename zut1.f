      PROGRAM ZUT_99_05                                                 UT    10
C     (REPORT: JUEL - 4189, OCTOBER 2005)                               UT    20
C                                                                       UT    30
      CHARACTER*8 MODE,MODE1/'data2zut'/,MODE2/'zutalone'/              UT    40
C                                                                       UT    50
      DIMENSION EE(36),W(20),FIELD(10),U(5),RRR(5),FX(5),DATE(2),       UT    60
     1 FLAG(16),HEAD(24),IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UT    70
     2 UNIT(2),AZERO(500),G(500),SIGPZ(500),TEMP(500),EZERO(500),       UT    80
     3 GAMN(500),GMGM(500),R(500),S(500),SOLVE(50),ABAR(50),C(50),      UT    90
     4 DZERO(50),AMOD1(50),SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),      UT   100
     5 AMU(505),CHI(505),DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),  UT   110
     6 GAMMA(500),PSI(505),RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),   UT   120
     7 SUB(501),TERM(3),X(505),XI(500),INDEX(1000,4),BVAL(1001,3),      UT   130
     8 ENULL(500),CORINT(500),LOESCH(9),NFT(30),NRMAF(30),DEN(30),      UT   140
     9 IMA(30)                                                          UT   150
C                                                                       UT   160
      COMMON /GAGRU/ EGO(69),SIRA(69),FF,FL,M6,NGGR,NGGR1,EOE,EERSTE,   UT   170
     1 ELETZT,IO,IU,RIJS,MMEES,IIII,JRJR,ITUZ,IGRUG,M5,NEUI,NALTI,      UT   180
     2 UAPS(501),N5                                                     UT   190
C                                                                       UT   200
      COMMON /DIR25/ NAE25,NEI25,LIBSAE,KENNNR,LENNNR,IFALNR,NUCLID,    UT   210
     1 INHALT(34,2),SISP(68),LOPUTZ(9),MAXSAE,LIBIN                     UT   220
C                                                                       UT   230
      COMMON /GAM/ N6,NGAM,IDENT,IDSATZ,IDNUCL,ENULL,CORINT,NUM1,LOESCH,UT   240
     1 M9                                                               UT   250
C                                                                       UT   260
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          UT   270
C                                                                       UT   280
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        UT   290
C                                                                       UT   300
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            UT   310
C                                                                       UT   320
      COMMON IRVT,JC,K,MAIM,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          UT   330
C                                                                       UT   340
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           UT   350
C                                                                       UT   360
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           UT   370
C                                                                       UT   380
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       UT   390
C                                                                       UT   400
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        UT   410
C                                                                       UT   420
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            UT   430
C                                                                       UT   440
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     UT   450
C                                                                       UT   460
      COMMON /COMRIG/ RIGES,RIGESW,RIGET,RIGETW                         UT   470
C                                                                       UT   480
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     UT   490
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   UT   500
     2 ESIG2,EDIQU2,KI,IEING5,TESTA,ENERGU,ENERGO,NNRESO,NNDATA,NNSIGA, UT   510
     3 JI,DENHOM                                                        UT   520
C                                                                       UT   530
      COMMON /DATA2/ NEI29,NXT29,FUTYP,DATA(14),IWOHIN,FTYP             UT   540
C                                                                       UT   550
      COMMON /CL/ A                                                     UT   560
C                                                                       UT   570
      EQUIVALENCE(EE(1),FIELD(1))                                       UT   580
C                                                                       UT   590
      DATA FCP017/2H10/,FCP018/2H11/,FCP019/3H ZU/,FCP020/3HT C/,       UT   600
     1 FCP021/3HROS/,FCP022/3HS S/,FCP023/3HECT/,FCP024/3HION/,         UT   610
     2 FCP025/3HD-S/,FCP026/3HET /,FCP028/3HEND/,FCP029/3HREP/          UT   620
C                                                                       UT   630
  400 FORMAT (A8)                                                       UT   640
  411 FORMAT (/////' ***** "MODE" (Card ZS) OUT OF RANGE *****'//' *****UT   650
     1      PROGRAM STOPS            *****')                            UT   660
  500 FORMAT (24A3)                                                     UT   670
  502 FORMAT (///' END OF CASE(S)')                                     UT   680
  503 FORMAT (/' FUEL TYPE',I6,' NOT AVAILABLE ON DATA-2-LIBRARY. STOP.'UT   690
     1 )                                                                UT   700
  504 FORMAT (/' DATA FROM DATA-2: FUEL TYPE: ',I3,', VARIANT: ',I3/' ',UT   710
     1 46('=')/)                                                        UT   720
  640 FORMAT (I1,I5,I1,7E9.4)                                           UT   730
 5022 FORMAT (12I6)                                                     UT   740
C                                                                       UT   750
C                                                                       UT   760
      CALL WATCH(ENDE)                                                  UT   770
C                                                                       UT   780
      A = ENDE                                                          UT   790
C      OPEN(5,FILE=' ')                                                  UT   800
C      OPEN(6,FILE=' ')                                                  UT   810
	OPEN(5,FILE='zutu8-7g-47pert.i')
	OPEN(6,FILE='zutu8-7g-47pert.o')
C                                                                       UT   820
      CALL INPLIST                                                      UT   830
C                                                                       UT   840
      REWIND 5                                                          UT   850
      OPEN(8,STATUS='SCRATCH')                                          UT   860
      OPEN(9,STATUS='SCRATCH')                                          UT   870
      OPEN(10,STATUS='SCRATCH')                                         UT   880
      OPEN(11,STATUS='SCRATCH')                                         UT   890
      OPEN(19,STATUS='SCRATCH')                                         UT   900
C     OPEN(30,ACCESS='DIRECT',RECL=292,FILE='Libraries\resint')         UT   910
      OPEN(30,ACCESS='DIRECT',RECL=292,FILE='Libraries\resint')   
C                                                                       UT   920
C     CARD ZS (Start)                                                   UT   930
C                                                                       UT   940
      READ (5,400) MODE                                                 UT   950
C                                                                       UT   960
      IF(MODE .EQ. MODE1 .OR. MODE .EQ. MODE2) GOTO 410                 UT   970
      WRITE (6,411)                                                     UT   980
      STOP                                                              UT   990
  410 CONTINUE                                                          UT  1000
C                                                                       UT  1010
      IF(MODE .EQ. MODE1) CALL ZDATA2                                   UT  1020
C                                                                       UT  1030
      CALL PAGE(IP$,JP$,9999)                                           UT  1040
C                                                                       UT  1050
    1 CALL COVZUT                                                       UT  1060
C                                                                       UT  1070
      NEI29 = 29                                                        UT  1080
      NEI25 = 30                                                        UT  1090
      FUTYP = 0.                                                        UT  1100
      FTYP = 0.                                                         UT  1110
      IWOHIN = 0                                                        UT  1120
      IZAEHL = 0                                                        UT  1130
      RIGES = 0                                                         UT  1140
      RIGESW = 0                                                        UT  1150
      RIGET = 0.                                                        UT  1160
      RIGETW = 0.                                                       UT  1170
      IFALNR = 0                                                        UT  1180
      NGAM = 0                                                          UT  1190
      IDNUCL = 0                                                        UT  1200
      IDENT=0                                                           UT  1210
      M5 = 5                                                            UT  1220
      N5 = M5                                                           UT  1230
      M6 = 6                                                            UT  1240
      N6 = M6                                                           UT  1250
      ITUZ = 0                                                          UT  1260
      IGRUG = 0                                                         UT  1270
      IRESRE = 0                                                        UT  1280
      NNRESO = 0                                                        UT  1290
      JII = 0                                                           UT  1300
      REWIND 8                                                          UT  1310
      REWIND 9                                                          UT  1320
      REWIND 10                                                         UT  1330
      REWIND 11                                                         UT  1340
      REWIND 19                                                         UT  1350
C                                                                       UT  1360
C     M9 ZWISCHENSPEICHER-EINHEIT FUER ZUTGAM                           UT  1370
C                                                                       UT  1380
      M9 = 9                                                            UT  1390
C                                                                       UT  1400
C     SPEICHERLOESCHUNG FUER DEN COMMON-BEREICH                         UT  1410
C                                                                       UT  1420
      DO 2 I=1,36                                                       UT  1430
        EE(I) = 0.                                                      UT  1440
    2 CONTINUE                                                          UT  1450
      IJK = 0                                                           UT  1460
  600 IJK = IJK + 1                                                     UT  1470
      U(IJK) = 0                                                        UT  1480
      IF(IJK-21128) 600,601,601                                         UT  1490
  601 CONTINUE                                                          UT  1500
C                                                                       UT  1510
C     Z U T - RESOLVED RESONANCE ABSORPTION CALCULATION - G.F.KUNCIR    UT  1520
C     MAIN PROCESSING PROGRAM                                           UT  1530
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        UT  1540
C     BEI MEHREREN FAELLEN HINTEREINANDER                               UT  1550
C                                                                       UT  1560
C     STORAGE ALLOCATION CONSTANTS                                      UT  1570
C     ITEMS PER RESOLVED RESONANCE PARAMETER SET                        UT  1580
C                                                                       UT  1590
C     AENDERUNG IN DER REIHENFOLGE DER EINGABEDATEN (FOLGEFAELLE)       UT  1600
C     (GILT NUR FUER RESONANZDATEN VON KARTE)                           UT  1610
C     -----------------------------------------------------------       UT  1620
C     1.) TEXT-KARTE                                                    UT  1630
C     2.) KARTEN MIT DEN RESONANZEN EZERO(I)                            UT  1640
C     3.) LEERKARTE                                                     UT  1650
C     DIE UEBRIGEN DATENKARTEN FOLGEN IN DER BISHERIGEN REIHENFOLGE.    UT  1660
C     NACH JEDEM FALL IST EINE KARTE MIT "8" IN COL.1 ANZUFUEGEN.       UT  1670
C     DIE RESONANZDATEN BRAUCHEN (FALLS GLEICHBLEIBEND) NICHT NOCH EIN- UT  1680
C     MAL EINGEGEBEN WERDEN (SIE SIND AUF DER UNIT 8 GESPEICHERT).      UT  1690
C                                                                       UT  1700
C     HERSTELLUNG VON UNIT 8                                            UT  1710
C                                                                       UT  1720
 5020 CONTINUE                                                          UT  1730
C                                                                       UT  1740
      READ (N5,640) IEND,JI,KI,(FIELD(LI),LI=1,KI)                      UT  1750
C                                                                       UT  1760
      IF(JI .EQ. -30) JII = JI                                          UT  1770
C                                                                       UT  1780
      IF(IEND .EQ. 9) CALL EINGA9(MODE,MODE1)                           UT  1790
C                                                                       UT  1800
      IF(NNRESO .EQ. 0) GOTO 6001                                       UT  1810
      JEND = IEND                                                       UT  1820
C                                                                       UT  1830
      CALL RESODA(IZAEHL,JEND)                                          UT  1840
C                                                                       UT  1850
      IEND = JEND                                                       UT  1860
      IF(IWOHIN .LT. 0) GOTO 5020                                       UT  1870
 6001 CONTINUE                                                          UT  1880
      IF(JI .EQ. -30) FUTYP = FIELD(1)                                  UT  1890
      IF(FUTYP .GT. 0.) FTYP = FUTYP                                    UT  1900
      IF(IEND .NE. 6) GOTO 5019                                         UT  1910
C                                                                       UT  1920
      CALL TUZ                                                          UT  1930
C                                                                       UT  1940
      GOTO 5020                                                         UT  1950
 5019 CONTINUE                                                          UT  1960
      IF(FUTYP .LE. 0.) GOTO 5023                                       UT  1970
      MFUTP = IFIX(FUTYP)                                               UT  1980
      II = MFUTP + 1                                                    UT  1990
      IF(MFUTP .LE. 100) GOTO 5025                                      UT  2000
      NXT29 = 2                                                         UT  2010
      READ (NEI29,REC=NXT29) N30,(NFT(I),I=1,N30)                       UT  2020
      DO 5024 I=1,N30                                                   UT  2030
        II = I                                                          UT  2040
        IF(MFUTP .EQ. NFT(I)) GOTO 5025                                 UT  2050
 5024 CONTINUE                                                          UT  2060
      WRITE (M6,503) MFUTP                                              UT  2070
      STOP                                                              UT  2080
 5025 CONTINUE                                                          UT  2090
      NXT29 = II                                                        UT  2100
      READ (NEI29,REC=NXT29) NISO,(NRMAF(I),DEN(I),I=1,NISO),(DATA(I),I=UT  2110
     1 1,14),NFU,NVR,(IMA(NRMAF(I)),I=1,NISO)                           UT  2120
      WRITE (M6,504) NFU,NVR                                            UT  2130
      FUTYP = 0.                                                        UT  2140
 5023 CONTINUE                                                          UT  2150
      IF(NGAM .GT. 0 .OR. JI .GE. 0) GOTO 5021                          UT  2160
      NGAM = IABS(JI)                                                   UT  2170
      IDSA = 0                                                          UT  2180
      IF(JII .NE. -30) GOTO 5026                                        UT  2190
      IDSATZ = -180                                                     UT  2200
      IDNUCL = 170                                                      UT  2210
      GOTO 5027                                                         UT  2220
 5026 CONTINUE                                                          UT  2230
      READ (5,5022) IDSATZ,IDNUCL,(LOESCH(LO),LO=1,9)                   UT  2240
 5027 CONTINUE                                                          UT  2250
      IF(IDSATZ .GT. 0) IDENT = 180                                     UT  2260
      DENHOM = 0.                                                       UT  2270
      IF(NNDATA .LE. 0.) GOTO 301                                       UT  2280
      DO 300 I=1,NISO                                                   UT  2290
        IF(IDNUCL .NE. IMA(NRMAF(I))) GOTO 300                          UT  2300
        DENHOM = DEN(I)                                                 UT  2310
  300 CONTINUE                                                          UT  2320
  301 CONTINUE                                                          UT  2330
      IF(IDSATZ .GT. 0) IDSA = IDSATZ                                   UT  2340
      IDSATZ = IABS(IDSATZ)                                             UT  2350
      IF(LOESCH(1) .EQ. 0 .AND. IDSA .GT. 0) LOESCH(1) = IDSA           UT  2360
      IF(NGAM .NE. NEI25) GOTO 2000                                     UT  2370
      LENNNR = IDENT                                                    UT  2380
      IFALNR = IDSATZ                                                   UT  2390
      NUCLID = IDNUCL                                                   UT  2400
      DO 2501 LO=1,9                                                    UT  2410
        LOPUTZ(LO) = LOESCH(LO)                                         UT  2420
 2501 CONTINUE                                                          UT  2430
C                                                                       UT  2440
C     VORBEREITUNG EINER GAM-GRUPPEN-LIBRARY                            UT  2450
C                                                                       UT  2460
      CALL GRUGA                                                        UT  2470
C                                                                       UT  2480
      IGRUG = IGRUG + 1                                                 UT  2490
      NGAM = 0                                                          UT  2500
      IDNUCL = 0                                                        UT  2510
      GOTO 5020                                                         UT  2520
 2000 CONTINUE                                                          UT  2530
      IF(IDENT .GT. 0) GOTO 5020                                        UT  2540
      IDENT = IABS(IDENT)                                               UT  2550
C                                                                       UT  2560
      CALL GAMDAT(IDENT,IDSATZ,IDNUCL,NGAM,M6)                          UT  2570
C                                                                       UT  2580
      NGAM = 0                                                          UT  2590
      GOTO 5020                                                         UT  2600
 5021 CONTINUE                                                          UT  2610
      IF(IEND .EQ. 9) GOTO 5020                                         UT  2620
      IF(IEND .GT. 0) IRESRE = IRESRE + 1                               UT  2630
      WRITE (8,640) IEND,JI,KI,(FIELD(LI),LI=1,KI)                      UT  2640
      IF(IEND) 5020,5030,5020                                           UT  2650
 5030 CONTINUE                                                          UT  2660
  999 IRSV = 9                                                          UT  2670
      IDEN(1) = 1                                                       UT  2680
      REWIND 8                                                          UT  2690
C                                                                       UT  2700
C     ITEMS PER RESONANCE CONFIGURATION SET                             UT  2710
C                                                                       UT  2720
      ICON = 8                                                          UT  2730
C                                                                       UT  2740
C     MAXIMUM INDEPENDENT RESOLVED RESONANCE PARAMETER SETS             UT  2750
C                                                                       UT  2760
      MRSV = 500                                                        UT  2770
C                                                                       UT  2780
C     MAXIMUM INDEPENDENT RESONANCE CONFIGURATION SETS                  UT  2790
C                                                                       UT  2800
      MCON = 50                                                         UT  2810
C                                                                       UT  2820
C     MAXIMUM ACCEPTABLE INPUT SETS                                     UT  2830
C                                                                       UT  2840
      MAIM = 1000                                                       UT  2850
C                                                                       UT  2860
C     MAXIMUM MESH SIZE                                                 UT  2870
C                                                                       UT  2880
      MMSH = 501                                                        UT  2890
C                                                                       UT  2900
C     MAXIMUM INTEGRATION BACK VALUES                                   UT  2910
C                                                                       UT  2920
      MIBV = 1001                                                       UT  2930
C                                                                       UT  2940
C     SET FORTRAN LOGICAL UNIT DESIGNATIONS                             UT  2950
C                                                                       UT  2960
      NINP = 5                                                          UT  2970
      NOUT = 6                                                          UT  2980
      NOXT = 10                                                         UT  2990
      UNIT(1) = FCP017                                                  UT  3000
      NNXT = 11                                                         UT  3010
      UNIT(2) = FCP018                                                  UT  3020
C                                                                       UT  3030
C     SET UNIT IDENTIFICATION WORDS                                     UT  3040
C                                                                       UT  3050
      TFLAG(1) = FCP019                                                 UT  3060
      TFLAG(2) = FCP020                                                 UT  3070
      TFLAG(3) = FCP021                                                 UT  3080
      TFLAG(4) = FCP022                                                 UT  3090
      TFLAG(5) = FCP023                                                 UT  3100
      TFLAG(6) = FCP024                                                 UT  3110
      TFLAG(7) = FCP025                                                 UT  3120
      TFLAG(8) = (+FCP026)                                              UT  3130
C                                                                       UT  3140
C     READ DATE, NOXT LABEL, NNXT LABEL, AND COMMENT                    UT  3150
C                                                                       UT  3160
      READ (NINP,500,END=501) (HEAD(I),I=1,24)                          UT  3170
C                                                                       UT  3180
      IF(HEAD(1) .EQ. FCP028) GOTO 501                                  UT  3190
      IF(HEAD(1) .EQ. FCP029) GOTO 1                                    UT  3200
      IF(IDEN(2)) 30,30,40                                              UT  3210
   30 CONTINUE                                                          UT  3220
C                                                                       UT  3230
C     REWIND NNXT                                                       UT  3240
C                                                                       UT  3250
   40 IF(IDEN(1)) 50,50,60                                              UT  3260
   50 REWIND NOXT                                                       UT  3270
C                                                                       UT  3280
C     IDENTIFY UNIT NOXT, READ UNIT NOXT RESOLVED RESONANCE INDEX       UT  3290
C                                                                       UT  3300
      CALL TIDIX(1)                                                     UT  3310
C                                                                       UT  3320
C     READ AND STORE INPUT, PREPARE SEQUENCE INDICES                    UT  3330
C                                                                       UT  3340
   60 CALL INPUT                                                        UT  3350
C                                                                       UT  3360
      IF(NEND .EQ. 5) CALL STUEZP                                       UT  3370
C                                                                       UT  3380
      IF(ITUZ .EQ. 0) GOTO 9                                            UT  3390
C                                                                       UT  3400
      CALL TUTS                                                         UT  3410
C                                                                       UT  3420
    9 CONTINUE                                                          UT  3430
      IF(IRESRE .EQ. 0) GOTO 181                                        UT  3440
      IF(NAIN) 200,200,70                                               UT  3450
C                                                                       UT  3460
C     COMPUTE MESH NEEDED FOR CROSS SECTION SETS                        UT  3470
C                                                                       UT  3480
   70 CALL GRID                                                         UT  3490
C                                                                       UT  3500
C     PRINT UNIT CONDITIONS, WRITE UNIT NNXT IDENTIFICATION AND INDEX   UT  3510
C                                                                       UT  3520
      CALL TIDIX(2)                                                     UT  3530
C                                                                       UT  3540
C     RESOLVED RESONANCE CROSS SECTION GENERATION LOOP                  UT  3550
C                                                                       UT  3560
      K = 1                                                             UT  3570
      DO 180 L=1,IRVN                                                   UT  3580
        I = INDEX(L,3)                                                  UT  3590
        IF(I-IRVO) 80,80,110                                            UT  3600
   80   IF(AZERO(I)) 90,90,100                                          UT  3610
C                                                                       UT  3620
C     SKIP UNIT NOXT RECORD                                             UT  3630
C                                                                       UT  3640
   90   READ (NOXT)                                                     UT  3650
        GOTO 180                                                        UT  3660
C                                                                       UT  3670
C     READ UNIT NOXT CROSS SECTION RECORD                               UT  3680
C                                                                       UT  3690
  100   READ (NOXT) DATE(1),DATE(2),IM,(AMU(M+2),SIGAZ(M+2),SIGSZ(M+2),MUT  3700
     1   =1,IM)                                                         UT  3710
        GOTO 130                                                        UT  3720
C                                                                       UT  3730
C     COMPUTE RESOLVED RESONANCE CROSS SECTION DATA                     UT  3740
C                                                                       UT  3750
  110   IF(MMSH-MESH(I)) 180,120,120                                    UT  3760
  120   CONTINUE                                                        UT  3770
C                                                                       UT  3780
        CALL CROSS                                                      UT  3790
C                                                                       UT  3800
C     WRITE UNIT NNXT CROSS SECTION RECORD                              UT  3810
C                                                                       UT  3820
  130   IF(IDEN(2)) 140,140,170                                         UT  3830
  140   IF(I-IRVO) 150,150,160                                          UT  3840
  150   CONTINUE                                                        UT  3850
  160   CONTINUE                                                        UT  3860
C                                                                       UT  3870
C     CALCULATE RESOLVED RESONANCE INTEGRALS AND WING CORRECTIONS       UT  3880
C                                                                       UT  3890
  170   CALL RESOL                                                      UT  3900
C                                                                       UT  3910
  180 CONTINUE                                                          UT  3920
  181 CONTINUE                                                          UT  3930
C                                                                       UT  3940
C     VERBUCHEN AUF GAM-GRUPPEN-LIBRARY                                 UT  3950
C                                                                       UT  3960
      IF(IFALNR .NE. 0) CALL GRUGA3                                     UT  3970
C                                                                       UT  3980
      IF(IRESRE .EQ. 0) CALL TERMINAT                                   UT  3990
      IF(IDEN(1)+IDEN(2)-2) 190,240,240                                 UT  4000
C                                                                       UT  4010
C     CALCULATE TOTAL RESOLVED RESONANCE INTEGRALS                      UT  4020
C                                                                       UT  4030
  190 CALL TOTAL                                                        UT  4040
C                                                                       UT  4050
C     END OF CALCULATION                                                UT  4060
C                                                                       UT  4070
  200 IF(IDEN(1)) 210,210,220                                           UT  4080
  210 REWIND NOXT                                                       UT  4090
  220 IF(IDEN(2)) 230,230,240                                           UT  4100
  230 CONTINUE                                                          UT  4110
C                                                                       UT  4120
C     END FILE NNXT, REWIND NNXT                                        UT  4130
C                                                                       UT  4140
  240 IF(IEND-4) 260,250,260                                            UT  4150
  250 I = 2                                                             UT  4160
  260 CONTINUE                                                          UT  4170
C                                                                       UT  4180
C     SPEICHERLOESCHUNG FUER DEN NAECHSTEN FALL                         UT  4190
C                                                                       UT  4200
      IJK = 0                                                           UT  4210
  602 IJK = IJK + 1                                                     UT  4220
      U(IJK) = 0.                                                       UT  4230
      IF(IJK-21128) 602,603,603                                         UT  4240
  603 CONTINUE                                                          UT  4250
C                                                                       UT  4260
      CALL PAGE(IP$,JP$,9999)                                           UT  4270
C                                                                       UT  4280
      IF(IWOHIN .EQ. 9) GOTO 1                                          UT  4290
      GOTO 999                                                          UT  4300
  501 WRITE (NOUT,502)                                                  UT  4310
      CALL TERMINAT                                                     UT  4320
      END                                                               UT  4330
      SUBROUTINE INPUT                                                  NPU   10
C                                                                       NPU   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATA(31),FIELD(10),DATE(2),     NPU   30
     1 FLAG(16),HEAD(24),IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),NPU   40
     2 UNIT(2),AZERO(500),G(500),SIGPZ(500),TEMP(500),EZERO(500),       NPU   50
     3 GAMN(500),GMGM(500),R(500),S(500),SOLVE(50),ABAR(50),C(50),      NPU   60
     4 DZERO(50),AMOD1(50),SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),      NPU   70
     5 AMU(505),CHI(505),DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),  NPU   80
     6 GAMMA(500),PSI(505),RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),   NPU   90
     7 SUB(501),TERM(3),X(505),XI(500),INDEX(1000,4),BVAL(1001,3)       NPU  100
C                                                                       NPU  110
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     NPU  120
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   NPU  130
     2 ESIG2,EDIQU2,KI,IEING5,TESTA,ENERGU,ENERGO,NNRESO,NNDATA,NNSIGA, NPU  140
     3 JI                                                               NPU  150
C                                                                       NPU  160
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          NPU  170
C                                                                       NPU  180
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        NPU  190
C                                                                       NPU  200
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            NPU  210
C                                                                       NPU  220
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          NPU  230
C                                                                       NPU  240
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           NPU  250
C                                                                       NPU  260
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           NPU  270
C                                                                       NPU  280
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       NPU  290
C                                                                       NPU  300
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        NPU  310
C                                                                       NPU  320
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            NPU  330
C                                                                       NPU  340
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     NPU  350
C                                                                       NPU  360
      COMMON /UI/ IM,MI,NI                                              NPU  370
C                                                                       NPU  380
C     READS AND STORES INPUT TO RESOLVED RESONANCE CALCULATION          NPU  390
C     PREPARES CALCULATION SEQUENCE INDICES                             NPU  400
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        NPU  410
C     BEI MEHREREN FAELLEN HINTEREINANDER                               NPU  420
C                                                                       NPU  430
C                                                                       NPU  440
      IZAEHL = 0                                                        NPU  450
      IRVN = IRVO                                                       NPU  460
      ESIG1 = 0.                                                        NPU  470
      ESIG2 = 0.                                                        NPU  480
      EDIQU1 = 0.                                                       NPU  490
      EDIQU2 = 0.                                                       NPU  500
      DO 540 II=1,MAIN                                                  NPU  510
   20   CONTINUE                                                        NPU  520
        NIP = NINP                                                      NPU  530
        IF(NNRESO .GT. 0) NIP = NNRESO                                  NPU  540
        IF(NNRESO .LT. 0 .AND. ECDANC .GE. 0.) GOTO 21                  NPU  550
        READ (NIP ,640) IEND,JI,KI,(FIELD(LI),LI=1,KI)                  NPU  560
        IF(IEND .NE. 7) GOTO 21                                         NPU  570
C                                                                       NPU  580
        CALL DANC3(JI)                                                  NPU  590
C                                                                       NPU  600
        GOTO 20                                                         NPU  610
   21   CONTINUE                                                        NPU  620
        IF(NNRESO .EQ. 0) GOTO 19                                       NPU  630
        JEND = IEND                                                     NPU  640
C                                                                       NPU  650
        CALL RESODA(IZAEHL,JEND)                                        NPU  660
C                                                                       NPU  670
        IEND = JEND                                                     NPU  680
   19   CONTINUE                                                        NPU  690
C                                                                       NPU  700
        IF(IEND .EQ. 1) CALL EINGAB                                     NPU  710
C                                                                       NPU  720
        IF(IEND-5) 912,911,912                                          NPU  730
  911   NEND = IEND                                                     NPU  740
C                                                                       NPU  750
        CALL EINGA5                                                     NPU  760
C                                                                       NPU  770
        IEND = 1                                                        NPU  780
  912   CONTINUE                                                        NPU  790
        IF(IEND-8) 920,910,920                                          NPU  800
  910   NINP = IEND                                                     NPU  810
        GOTO 20                                                         NPU  820
  920   IF(IEND) 900,930,900                                            NPU  830
  930   NINP = 5                                                        NPU  840
  900   IF(IEND*(IEND-4)) 40,560,40                                     NPU  850
   40   DO 60 LI=1,KI                                                   NPU  860
          MI = JI + LI - 1                                              NPU  870
          DATA(MI) = FIELD(LI)                                          NPU  880
   60   CONTINUE                                                        NPU  890
        IF(IEND .NE. 2) WRITE (NOUT,641) IEND,JI,KI,(FIELD(LI),LI=1,KI) NPU  900
        IF(IEND-2) 20,80,80                                             NPU  910
   80   IF(IRVN) 240,240,100                                            NPU  920
  100   DO 200 JI=1,IRVN                                                NPU  930
          DO 120 KI=1,IRSV                                              NPU  940
            LI = (KI-1) * MRSV + JI                                     NPU  950
            IF(AZERO(LI)-DATA(KI)) 200,120,200                          NPU  960
  120     CONTINUE                                                      NPU  970
          IF(IEND-3) 180,140,180                                        NPU  980
  140     IF(JI-IRVO) 160,160,20                                        NPU  990
C                                                                       NPU 1000
C     CROSS SECTION RECORD TO BE DELETED FROM UNIT NOXT                 NPU 1010
C                                                                       NPU 1020
  160     AZERO(JI) = -AZERO(JI)                                        NPU 1030
          IDEL = IDEL + 1                                               NPU 1040
          GOTO 20                                                       NPU 1050
C                                                                       NPU 1060
C     INPUT CONTAINED IN RESOLVED RESONANCE INDEX                       NPU 1070
C                                                                       NPU 1080
  180     INDEX(II,1) = JI                                              NPU 1090
          GOTO 280                                                      NPU 1100
  200   CONTINUE                                                        NPU 1110
C                                                                       NPU 1120
C     INPUT NOT CONTAINED IN RESOLVED RESONANCE INDEX                   NPU 1130
C                                                                       NPU 1140
        IF(MRSV-IRVN) 220,220,240                                       NPU 1150
C                                                                       NPU 1160
C     RESOLVED RESONANCE INDEX AT MAXIMUM LIMIT                         NPU 1170
C                                                                       NPU 1180
  220   CALL TITLE(10,REASON)                                           NPU 1190
C                                                                       NPU 1200
        GOTO(480,500,520),MI                                            NPU 1210
C                                                                       NPU 1220
C     RESOLVED RESONANCE INDEX WITHIN MAXIMUM LIMIT                     NPU 1230
C                                                                       NPU 1240
  240   NI = II                                                         NPU 1250
        IRVN = IRVN + 1                                                 NPU 1260
        INDEX(II,1) = IRVN                                              NPU 1270
        DO 260 JI=1,IRSV                                                NPU 1280
          KI = (JI-1) * MRSV + IRVN                                     NPU 1290
          AZERO(KI) = DATA(JI)                                          NPU 1300
  260   CONTINUE                                                        NPU 1310
        DO 261 JI=18,31                                                 NPU 1320
          KU = JI - 17                                                  NPU 1330
          W(KU) = DATA(JI)                                              NPU 1340
  261   CONTINUE                                                        NPU 1350
C                                                                       NPU 1360
C     COMPARE INPUT AND RESONANCE CONFIGURATION INDEX                   NPU 1370
C                                                                       NPU 1380
  280   IF(ICNN) 420,420,300                                            NPU 1390
  300   DO 340 JI=1,ICNN                                                NPU 1400
          DO 320 KI=1,ICON                                              NPU 1410
            LI = (KI-1) * MCON + JI                                     NPU 1420
            MI = KI + IRSV                                              NPU 1430
            IF(SOLVE(LI)-DATA(MI)) 340,320,340                          NPU 1440
  320     CONTINUE                                                      NPU 1450
C                                                                       NPU 1460
C     INPUT CONTAINED IN RESONANCE CONFIGURATION INDEX                  NPU 1470
C                                                                       NPU 1480
          INDEX(II,2) = JI                                              NPU 1490
          GOTO 460                                                      NPU 1500
  340   CONTINUE                                                        NPU 1510
C                                                                       NPU 1520
C     INPUT NOT CONTAINED IN RESONANCE CONFIGURATION INDEX              NPU 1530
C                                                                       NPU 1540
        IF(MCON-ICNN) 360,360,420                                       NPU 1550
C                                                                       NPU 1560
C     RESONANCE CONFIGURATION INDEX AT MAXIMUM LIMIT                    NPU 1570
C                                                                       NPU 1580
  360   CALL TITLE(10,REASON)                                           NPU 1590
C                                                                       NPU 1600
        IF(II-NI) 400,380,400                                           NPU 1610
C                                                                       NPU 1620
C     EXTENSION OF RESOLVED RESONANCE INDEX INVALID                     NPU 1630
C                                                                       NPU 1640
  380   IRVN = IRVN - 1                                                 NPU 1650
  400   GOTO(480,500,520),MI                                            NPU 1660
C                                                                       NPU 1670
C     RESONANCE CONFIGURATION INDEX WITHIN MAXIMUM LIMIT                NPU 1680
C                                                                       NPU 1690
  420   ICNN = ICNN + 1                                                 NPU 1700
        INDEX(II,2) = ICNN                                              NPU 1710
        DO 440 JI=1,ICON                                                NPU 1720
          KI = (JI-1) * MCON + ICNN                                     NPU 1730
          LI = JI + IRSV                                                NPU 1740
          SOLVE(KI) = DATA(LI)                                          NPU 1750
  440   CONTINUE                                                        NPU 1760
  460   NAIN = NAIN + 1                                                 NPU 1770
        GOTO 540                                                        NPU 1780
C                                                                       NPU 1790
C     PRINT INPUT SETS WHICH EXCEED STORAGE LIMITS                      NPU 1800
C                                                                       NPU 1810
  480   CALL PAGE(MI,0,7)                                               NPU 1820
C                                                                       NPU 1830
        WRITE (NOUT,660) (HEAD(JI),JI=1,4),(HEAD(JI),JI=13,24)          NPU 1840
        WRITE (NOUT,680)                                                NPU 1850
        IM = IRSV + ICON                                                NPU 1860
C                                                                       NPU 1870
  500   CALL PAGE(MI,2,6)                                               NPU 1880
C                                                                       NPU 1890
        WRITE (NOUT,700)                                                NPU 1900
        WRITE (NOUT,660) (REASON(JI),JI=1,12)                           NPU 1910
C                                                                       NPU 1920
  520   CALL PAGE(MI,7,11)                                              NPU 1930
C                                                                       NPU 1940
        WRITE (NOUT,700)                                                NPU 1950
        WRITE (NOUT,720) (DATA(JI),JI=1,IM)                             NPU 1960
        GOTO 20                                                         NPU 1970
  540 CONTINUE                                                          NPU 1980
C                                                                       NPU 1990
C     PREPARE SEQUENCE INDICES                                          NPU 2000
C                                                                       NPU 2010
  560 IF(NAIN) 620,620,580                                              NPU 2020
  580 II = MAX0(IRVN,NAIN)                                              NPU 2030
      DO 600 JI=1,II                                                    NPU 2040
        INDEX(JI,3) = JI                                                NPU 2050
        INDEX(JI,4) = JI                                                NPU 2060
  600 CONTINUE                                                          NPU 2070
C                                                                       NPU 2080
C     SORT INDEX(JI,3) FOR INCREASING INPUT(INDEX(JI,3))                NPU 2090
C                                                                       NPU 2100
      CALL SORT(IRVN,S,0,INDEX(1,3))                                    NPU 2110
C                                                                       NPU 2120
      CALL SORT(IRVN,R,0,INDEX(1,3))                                    NPU 2130
C                                                                       NPU 2140
      CALL SORT(IRVN,GMGM,0,INDEX(1,3))                                 NPU 2150
C                                                                       NPU 2160
      CALL SORT(IRVN,GAMN,0,INDEX(1,3))                                 NPU 2170
C                                                                       NPU 2180
      CALL SORT(IRVN,EZERO,0,INDEX(1,3))                                NPU 2190
C                                                                       NPU 2200
      CALL SORT(IRVN,SIGPZ,0,INDEX(1,3))                                NPU 2210
C                                                                       NPU 2220
      CALL SORT(IRVN,G,0,INDEX(1,3))                                    NPU 2230
C                                                                       NPU 2240
      CALL SORT(IRVN,AZERO,0,INDEX(1,3))                                NPU 2250
C                                                                       NPU 2260
      CALL SORT(IRVN,TEMP,0,INDEX(1,3))                                 NPU 2270
C                                                                       NPU 2280
C     SORT INDEX(JI,4) FOR INCREASING INPUT(INDEX(INDEX(JI,4),1))       NPU 2290
C                                                                       NPU 2300
      CALL SORT(NAIN,S,INDEX(1,1),INDEX(1,4))                           NPU 2310
C                                                                       NPU 2320
      CALL SORT(NAIN,R,INDEX(1,1),INDEX(1,4))                           NPU 2330
C                                                                       NPU 2340
      CALL SORT(NAIN,GMGM,INDEX(1,1),INDEX(1,4))                        NPU 2350
C                                                                       NPU 2360
      CALL SORT(NAIN,GAMN,INDEX(1,1),INDEX(1,4))                        NPU 2370
C                                                                       NPU 2380
      CALL SORT(NAIN,EZERO,INDEX(1,1),INDEX(1,4))                       NPU 2390
C                                                                       NPU 2400
      CALL SORT(NAIN,SIGPZ,INDEX(1,1),INDEX(1,4))                       NPU 2410
C                                                                       NPU 2420
      CALL SORT(NAIN,G,INDEX(1,1),INDEX(1,4))                           NPU 2430
C                                                                       NPU 2440
      CALL SORT(NAIN,AZERO,INDEX(1,1),INDEX(1,4))                       NPU 2450
C                                                                       NPU 2460
      CALL SORT(NAIN,TEMP,INDEX(1,1),INDEX(1,4))                        NPU 2470
C                                                                       NPU 2480
  620 CONTINUE                                                          NPU 2490
      RETURN                                                            NPU 2500
C                                                                       NPU 2510
  640 FORMAT (I1,I5,I1,7(0PE9.4))                                       NPU 2520
  641 FORMAT (' ',I1,I5,I1,7E9.4)                                       NPU 2530
  660 FORMAT (3H   ,16A3)                                               NPU 2540
  680 FORMAT (52H0  ** THE FOLLOWING INPUT SETS WERE NOT PROCESSED **)  NPU 2550
  700 FORMAT (1H )                                                      NPU 2560
  720 FORMAT (119H0        A ZERO            G   SIG P ZERO         TEMPNPU 2570
     1       E ZERO      GAMMA N  GAMMA GAMMA            R            S/NPU 2580
     2 2H  ,9(1PE13.5)/119H0      SOLUTION                     A BAR    NPU 2590
     3        C       N ZERO       A MOD1     SIG MOD1       A MOD2     NPU 2600
     4SIG MOD2/2H  ,1PE13.5,13H             ,7(1PE13.5))                NPU 2610
      END                                                               NPU 2620
      SUBROUTINE COVZUT                                                 COV   10
C                                                                       COV   20
      CHARACTER*2 DATG(4)                                               COV   30
C                                                                       COV   40
      CHARACTER*4 DATH                                                  COV   50
C                                                                       COV   60
      CHARACTER*8 DATF                                                  COV   70
C                                                                       COV   80
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             COV   90
C                                                                       COV  100
C                                                                       COV  110
      WRITE (6,10)                                                      COV  120
      WRITE (6,20)                                                      COV  130
C                                                                       COV  140
      CALL DATE_AND_TIME(DATF)                                          COV  150
C                                                                       COV  160
      READ (DATH,101) JAHR                                              COV  170
      READ (DATG(3),100) MON                                            COV  180
      READ (DATG(4),100) ITAG                                           COV  190
      WRITE (6,1003) ITAG,MON,JAHR                                      COV  200
      WRITE (6,30)                                                      COV  210
C                                                                       COV  220
   10 FORMAT ('1',1H ,9X,12('*'),5X,'***',6X,'***',5X,13('*'),17X,      COV  230
     1 11('*'),7X,10('*'),6X,'***'/11X,12('*'),5X,'***',6X,'***',5X,    COV  240
     2 13('*'),17X,12('*'),5X,12('*'),5X,'***'/19X,'***',2(6X,'***'),10XCOV  250
     3 ,'***',22X,'***',2(5X,'****'),4X,'****',5X,'***'/18X,'***',7X,'**COV  260
     4*',6X,'***',10X,'***',22X,'***',6X,'***',5X,'***',6X,'***',5X,'***COV  270
     5'/17X,'***',8X,'***',6X,'***',10X,'***',22X,'***',6X,'***',5X,'***COV  280
     6',14X,'***'/16X,'***',9X,'***',6X,'***',10X,'***',9X,9('*'),4X,'**COV  290
     7*',6X,'***',5X,'***',14X,'***'/15X,'***',10X,'***',6X,'***',10X,'*COV  300
     8**',9X,9('*'),4X,'***',6X,'***',5X,'***',2X,7('*'),5X,'***'/14X,'*COV  310
     9**',11X,'***',6X,'***',10X,'***',22X,'***',6X,'***',5X,'***',2X,  COV  320
     X 7('*'),5X,'***'/13X,'***',12X,'***',6X,'***',10X,'***',22X,'***',COV  330
     Y 6X,'***',5X,'***',6X,'***',5X,'***'/12X,'***',13X,'****',4X,'****COV  340
     Z',10X,'***',22X,'***',2(5X,'****'),4X,'****',5X,'***'/11X,12('*'),COV  350
     Z 6X,10('*'),11X,'***',22X,12('*'),2(5X,12('*'))/11X,12('*'),7X,   COV  360
     Z 8('*'),12X,'***',22X,11('*'),7X,10('*'),6X,12('*'))              COV  370
   20 FORMAT (//////37X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/37X,'COV  380
     1**',5X,'**',3(6X,'**',5X,'**')/37X,'**',5X,'**',6X,'**',13X,'**', COV  390
     2 5X,'**',6X,8('*')/38X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,'COV  400
     3**'/39X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**',   COV  410
     4 2(2X,'**'),9X,'**'/40X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),COV  420
     5 3X,'**',2X,'**',9X,'**')                                         COV  430
   30 FORMAT (//////////17X,'OCT. 2005',64X,'REPORT: V.S.O.P.(99/05)'/98COV  440
     1X,'JUEL - 4189'/98X,'SECTION 5.3'/'1')                            COV  450
  100 FORMAT (4I2)                                                      COV  460
  101 FORMAT (I4)                                                       COV  470
 1003 FORMAT (/////59X,'( ',I2,'.',I2,'.',I4,' )')                      COV  480
      RETURN                                                            COV  490
      END                                                               COV  500
      SUBROUTINE GRID                                                   GRI   10
C                                                                       GRI   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      GRI   30
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),AZERO(500)GRI   40
     2 ,G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),GMGM(500),     GRI   50
     3 R(500),S(500),SOLVE(50),ABAR(50),C(50),DZERO(50),AMOD1(50),      GRI   60
     4 SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),AMU(505),CHI(505),        GRI   70
     5 DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),GAMMA(500),PSI(505),GRI   80
     6 RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),SUB(501),TERM(3),X(505)GRI   90
     7 ,XI(500),INDEX(1000,4),BVAL(1001,3)                              GRI  100
C                                                                       GRI  110
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          GRI  120
C                                                                       GRI  130
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        GRI  140
C                                                                       GRI  150
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            GRI  160
C                                                                       GRI  170
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          GRI  180
C                                                                       GRI  190
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           GRI  200
C                                                                       GRI  210
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           GRI  220
C                                                                       GRI  230
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       GRI  240
C                                                                       GRI  250
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        GRI  260
C                                                                       GRI  270
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            GRI  280
C                                                                       GRI  290
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     GRI  300
C                                                                       GRI  310
      COMMON /UI/ IDUM(3),NG                                            GRI  320
C                                                                       GRI  330
C     COMPUTES MESH NEEDED FOR CROSS SECTION CALCULATION                GRI  340
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        GRI  350
C     BEI MEHREREN FAELLEN HINTEREINANDER                               GRI  360
C                                                                       GRI  370
C                                                                       GRI  380
C     THE CONSTANT 8.6164E-5 DEFINES KAPPA                              GRI  390
C     THE CONSTANT 273.0 DEFINES THE MESH REFERENCE TEMPERATURE TZERO   GRI  400
C                                                                       GRI  410
      IG = 0                                                            GRI  420
      DO 320 JG=1,NAIN                                                  GRI  430
        KG = INDEX(JG,4)                                                GRI  440
        LG = INDEX(KG,1)                                                GRI  450
        IF(LG-IG) 40,20,40                                              GRI  460
   20   IF(MMSH-MESH(IG)) 240,320,320                                   GRI  470
   40   IG = LG                                                         GRI  480
        GAMMA(IG) = GAMN(IG) + GMGM(IG)                                 GRI  490
        IF(R(IG)) 70,70,60                                              GRI  500
   60   RP = R(IG)                                                      GRI  510
        GOTO 80                                                         GRI  520
   70   R(IG) = 0.                                                      GRI  530
        RP = 5. + 0.5 * SQRT(8.6164E-5*273.0*EZERO(IG)/AZERO(IG)) /     GRI  540
     1   GAMMA(IG)                                                      GRI  550
   80   EPSIL(IG) = 2. / AZERO(IG) / FLOAT(IFIX(0.5*(RP*SQRT(EZERO(IG)/ GRI  560
     1   8.6164E-5/AZERO(IG)/273.0)+1.)))                               GRI  570
        SIGOH(IG) = 2.6E6 * G(IG) * GAMN(IG) / EZERO(IG) / GAMMA(IG)    GRI  580
        XONE = S(IG) * SQRT(SIGOH(IG)/SIGPZ(IG))                        GRI  590
        XI(IG) = GAMMA(IG) / 2. / SQRT(8.6164E-5*EZERO(IG)*TEMP(IG)/    GRI  600
     1   AZERO(IG))                                                     GRI  610
        IF(10./XI(IG)-XONE/2.) 100,100,120                              GRI  620
  100   EONE(IG) = EZERO(IG) + GAMMA(IG) * XONE / 2.                    GRI  630
        GOTO 140                                                        GRI  640
  120   EONE(IG) = EZERO(IG) + 10. * GAMMA(IG) / XI(IG)                 GRI  650
  140   ZONE = (EONE(IG)-EZERO(IG)) / EZERO(IG)                         GRI  660
        IF(ZONE-0.57142857E0) 160,160,180                               GRI  670
  160   MESH(IG) = 1 + 2 * IFIX(0.5*(ALOG(EONE(IG)/(2.*EZERO(IG)-       GRI  680
     1   EONE(IG)))/EPSIL(IG)+1.))                                      GRI  690
        EONE(IG) = 2. * EZERO(IG) / (1.+EXP(-EPSIL(IG)*FLOAT(MESH(IG)-1)GRI  700
     1   ))                                                             GRI  710
        GOTO 200                                                        GRI  720
  180   MESH(IG) = 1 + 2 * IFIX(0.5*(ALOG((7./3.)*EONE(IG)/EZERO(IG))/  GRI  730
     1   EPSIL(IG)+1.))                                                 GRI  740
        EONE(IG) = EZERO(IG) * EXP(EPSIL(IG)*FLOAT(MESH(IG)-1)) /       GRI  750
     1   (7./3.)                                                        GRI  760
  200   IF(MMSH-MESH(IG)) 220,320,320                                   GRI  770
C                                                                       GRI  780
C     MESH EXCEEDS STORAGE LIMITS                                       GRI  790
C                                                                       GRI  800
  220   CONTINUE                                                        GRI  810
        NG = 1                                                          GRI  820
        IEXM = IEXM + 1                                                 GRI  830
        WRITE (NOUT,361) EZERO(IG),R(IG),RP,MESH(IG)                    GRI  840
  240   MG = INDEX(KG,2)                                                GRI  850
C                                                                       GRI  860
        CALL TITLE(11,REASON)                                           GRI  870
C                                                                       GRI  880
        GOTO(260,280,300),NG                                            GRI  890
C                                                                       GRI  900
C     PRINT SETS WHICH REQUIRE MESH EXCEEDING THE STORAGE LIMITS        GRI  910
C                                                                       GRI  920
  260   CALL PAGE(NG,0,7)                                               GRI  930
C                                                                       GRI  940
        WRITE (NOUT,340) (HEAD(NG),NG=1,4),(HEAD(NG),NG=13,24)          GRI  950
        WRITE (NOUT,360)                                                GRI  960
C                                                                       GRI  970
  280   CALL PAGE(NG,2,6)                                               GRI  980
C                                                                       GRI  990
        WRITE (NOUT,380)                                                GRI 1000
        WRITE (NOUT,340) (REASON(NG),NG=1,12)                           GRI 1010
C                                                                       GRI 1020
  300   CALL PAGE(NG,7,11)                                              GRI 1030
C                                                                       GRI 1040
        WRITE (NOUT,380)                                                GRI 1050
        WRITE (NOUT,400) AZERO(IG),G(IG),SIGPZ(IG),TEMP(IG),EZERO(IG),  GRI 1060
     1   GAMN(IG),GMGM(IG),R(IG),S(IG),SOLVE(MG),ABAR(MG),C(MG),        GRI 1070
     2   DZERO(MG),AMOD1(MG),SIGM1(MG),AMOD2(MG),SIGM2(MG)              GRI 1080
  320 CONTINUE                                                          GRI 1090
      RETURN                                                            GRI 1100
C                                                                       GRI 1110
  340 FORMAT (3H   ,16A3)                                               GRI 1120
  360 FORMAT (52H0  ** THE FOLLOWING INPUT SETS WERE NOT PROCESSED **)  GRI 1130
  361 FORMAT (' FOR THE FOLLOWING RESONANCE ARE  EZERO(IG),R(IG),RP,MESHGRI 1140
     1(IG)  EQUAL:'/3E15.6,I6)                                          GRI 1150
  380 FORMAT (1H )                                                      GRI 1160
  400 FORMAT (119H0        A ZERO            G   SIG P ZERO         TEMPGRI 1170
     1       E ZERO      GAMMA N  GAMMA GAMMA            R            S/GRI 1180
     2 2H  ,9(1PE13.5)/119H0      SOLUTION                     A BAR    GRI 1190
     3        C       N ZERO       A MOD1     SIG MOD1       A MOD2     GRI 1200
     4SIG MOD2/2H  ,1PE13.5,13H             ,7(1PE13.5))                GRI 1210
      END                                                               GRI 1220
      SUBROUTINE CROSS                                                  CRO   10
C                                                                       CRO   20
      EXTERNAL SHAPE                                                    CRO   30
C                                                                       CRO   40
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      CRO   50
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),AZERO(500)CRO   60
     2 ,G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),GMGM(500),     CRO   70
     3 R(500),S(500),SOLVE(50),ABAR(50),C(50),DZERO(50),AMOD1(50),      CRO   80
     4 SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),AMU(505),CHI(505),        CRO   90
     5 DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),GAMMA(500),PSI(505),CRO  100
     6 RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),SUB(501),TERM(3),X(505)CRO  110
     7 ,XI(500),INDEX(1000,4),BVAL(1001,3)                              CRO  120
C                                                                       CRO  130
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          CRO  140
C                                                                       CRO  150
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        CRO  160
C                                                                       CRO  170
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            CRO  180
C                                                                       CRO  190
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          CRO  200
C                                                                       CRO  210
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           CRO  220
C                                                                       CRO  230
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           CRO  240
C                                                                       CRO  250
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       CRO  260
C                                                                       CRO  270
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        CRO  280
C                                                                       CRO  290
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            CRO  300
C                                                                       CRO  310
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     CRO  320
C                                                                       CRO  330
C     COMPUTES REQUIRED CROSS SECTION DATA                              CRO  340
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        CRO  350
C     BEI MEHREREN FAELLEN HINTEREINANDER                               CRO  360
C                                                                       CRO  370
C                                                                       CRO  380
      IC = MESH(I) + 4                                                  CRO  390
      DO 200 JC=1,IC                                                    CRO  400
C                                                                       CRO  410
C     CALCULATE MU AND X                                                CRO  420
C                                                                       CRO  430
        AMU(JC) = FLOAT(JC-3) * EPSIL(I)                                CRO  440
        X(JC) = 2. * (EONE(I)*EXP(-AMU(JC))-EZERO(I)) / GAMMA(I)        CRO  450
C                                                                       CRO  460
C     CALCULATE PSI(X,XI)                                               CRO  470
C                                                                       CRO  480
        IF(ABS(X(JC))-20./XI(I)) 20,20,60                               CRO  490
   20   ETA(1) = ATAN(X(JC)-12.64911/XI(I))                             CRO  500
        ETA(2) = ATAN(X(JC)-4./XI(I))                                   CRO  510
        ETA(3) = ATAN(X(JC))                                            CRO  520
        ETA(4) = ATAN(X(JC)+4./XI(I))                                   CRO  530
        ETA(5) = ATAN(X(JC)+12.64911/XI(I))                             CRO  540
        PSI(JC) = 0.                                                    CRO  550
        DO 40 KC=1,4                                                    CRO  560
C                                                                       CRO  570
          CALL GAUSS(ETA(KC),ETA(KC+1),1,SHAPE,PPSI)                    CRO  580
C                                                                       CRO  590
C     ONE TEN-POINT QUADRATURE FROM ETA(KC) TO ETA(KC+1)                CRO  600
C                                                                       CRO  610
          PSI(JC) = PSI(JC) + PPSI                                      CRO  620
   40   CONTINUE                                                        CRO  630
        PSI(JC) = XI(I) * PSI(JC) / 3.5449078E0                         CRO  640
        GOTO 80                                                         CRO  650
   60   PSI(JC) = (1.+6./XI(I)**2/(X(JC)**2)) / (1.+(X(JC)**2))         CRO  660
   80   IF(JC-5) 200,100,100                                            CRO  670
C                                                                       CRO  680
C     CALCULATE CHI(X,XI)                                               CRO  690
C                                                                       CRO  700
  100   IF(XI(I)*SQRT(TEMP(I)/273.)-0.1) 140,140,120                    CRO  710
  120   CHI(JC-2) = 2. * X(JC-2) * PSI(JC-2) + (8.*(PSI(JC-3)-PSI(JC-1))CRO  720
     1   +PSI(JC)-PSI(JC-4)) / 3. / EPSIL(I) / XI(I)**2 / (2.*EZERO(I)/ CRO  730
     2   GAMMA(I)+X(JC-2))                                              CRO  740
C                                                                       CRO  750
C     CALCULATE CROSS SECTIONS                                          CRO  760
C                                                                       CRO  770
  140   SIGAZ(JC-2) = SIGOH(I) * GMGM(I) * SQRT(EZERO(I)/EONE(I)) *     CRO  780
     1   EXP(0.5*AMU(JC-2)) * PSI(JC-2) / GAMMA(I)                      CRO  790
        IF(XI(I)*SQRT(TEMP(I)/273.)-0.1) 160,160,180                    CRO  800
  160   SIGSZ(JC-2) = PSI(JC-2) * (GAMN(I)*SIGOH(I)/GAMMA(I)) + SIGPZ(I)CRO  810
        GOTO 200                                                        CRO  820
  180   SIGSZ(JC-2) = PSI(JC-2) * (GAMN(I)*SIGOH(I)/GAMMA(I)) + SIGPZ(I)CRO  830
     1   + CHI(JC-2) * SQRT(G(I)*SIGPZ(I)*(GAMN(I)*SIGOH(I)/GAMMA(I)))  CRO  840
  200 CONTINUE                                                          CRO  850
      RETURN                                                            CRO  860
      END                                                               CRO  870
      SUBROUTINE TIDIX(IT)                                              TID   10
C                                                                       TID   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      TID   30
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),AZERO(500)TID   40
     2 ,G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),GMGM(500),     TID   50
     3 R(500),S(500),SIGOH(500),SIGSZ(505),SUB(501),TERM(3),X(505),     TID   60
     4 XI(500),INDEX(1000,4)                                            TID   70
C                                                                       TID   80
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          TID   90
C                                                                       TID  100
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        TID  110
C                                                                       TID  120
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            TID  130
C                                                                       TID  140
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          TID  150
C                                                                       TID  160
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           TID  170
C                                                                       TID  180
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        TID  190
C                                                                       TID  200
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            TID  210
C                                                                       TID  220
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     TID  230
C                                                                       TID  240
C     READS, WRITES, AND CHECKS UNIT IDENTIFICATION                     TID  250
C     READS, WRITES, AND PRINTS UNIT INDEX                              TID  260
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        TID  270
C     BEI MEHREREN FAELLEN HINTEREINANDER                               TID  280
C                                                                       TID  290
C                                                                       TID  300
      GOTO(20,160),IT                                                   TID  310
C                                                                       TID  320
C     CHECK UNIT NOXT IDENTIFICATION                                    TID  330
C                                                                       TID  340
   20 READ (NOXT,760) (FLAG(JT),JT=1,16),IRVO                           TID  350
      DO 120 JT=1,12                                                    TID  360
        IF(JT-9) 40,60,60                                               TID  370
   40   IF(FLAG(JT)-TFLAG(JT)) 100,120,100                              TID  380
   60   IF(FLAG(JT)-HEAD(JT-4)) 100,120,100                             TID  390
C                                                                       TID  400
C     WRONG UNIT ON NOXT                                                TID  410
C                                                                       TID  420
  100   WRITE (NOUT,780) UNIT(1),(HEAD(M1),M1=5,8),(FLAG(M2),M2=9,12)   TID  430
        PRINT 780,UNIT(1),(HEAD(M1),M1=5,8),(FLAG(M2),M2=9,12)          TID  440
        REWIND NOXT                                                     TID  450
C                                                                       TID  460
        CALL EXIT                                                       TID  470
C                                                                       TID  480
  120 CONTINUE                                                          TID  490
C                                                                       TID  500
C     READ RESOLVED RESONANCE INDEX                                     TID  510
C                                                                       TID  520
      DO 140 JT=1,IRVO                                                  TID  530
        READ (NOXT) AZERO(JT),G(JT),SIGPZ(JT),TEMP(JT),EZERO(JT),       TID  540
     1   GAMN(JT),GMGM(JT),R(JT),S(JT)                                  TID  550
  140 CONTINUE                                                          TID  560
      GOTO 740                                                          TID  570
  160 IF(IDEN(1)+IDEN(2)-2) 180,740,740                                 TID  580
C                                                                       TID  590
C     PRINT UNIT NOXT INDEX                                             TID  600
C                                                                       TID  610
  180 IF(IRVO) 400,400,200                                              TID  620
C                                                                       TID  630
  200 CALL PAGE(JT,0,8)                                                 TID  640
C                                                                       TID  650
      WRITE (NOUT,800) UNIT(1),(HEAD(M1),M1=1,4)                        TID  660
      WRITE (NOUT,820) (HEAD(M1),M1=5,8),(FLAG(M2),M2=13,16)            TID  670
      DO 360 JT=1,IRVO                                                  TID  680
        IF(1-JT) 220,260,260                                            TID  690
  220   DO 240 KT=1,4                                                   TID  700
          LT = JT + (KT-1) * MRSV                                       TID  710
          IF(ABS(AZERO(LT))-ABS(AZERO(LT-1))) 260,240,260               TID  720
  240   CONTINUE                                                        TID  730
C                                                                       TID  740
        CALL PAGE(KT,1,10)                                              TID  750
C                                                                       TID  760
        GOTO(280,300),KT                                                TID  770
C                                                                       TID  780
  260   CALL PAGE(KT,6,10)                                              TID  790
C                                                                       TID  800
  280   ATOM = ABS(AZERO(JT))                                           TID  810
        WRITE (NOUT,860) ATOM,G(JT),SIGPZ(JT),TEMP(JT)                  TID  820
        WRITE (NOUT,880)                                                TID  830
  300   IF(AZERO(JT)) 320,320,340                                       TID  840
  320   WRITE (NOUT,900) EZERO(JT),GAMN(JT),GMGM(JT),R(JT),S(JT)        TID  850
        GOTO 360                                                        TID  860
  340   WRITE (NOUT,920) EZERO(JT),GAMN(JT),GMGM(JT),R(JT),S(JT)        TID  870
  360 CONTINUE                                                          TID  880
      IF(IDEL) 400,400,380                                              TID  890
C                                                                       TID  900
  380 CALL PAGE(JT,2,6)                                                 TID  910
C                                                                       TID  920
      WRITE (NOUT,940) UNIT(2)                                          TID  930
C                                                                       TID  940
C     PREPARE FOR WRITING UNIT NNXT IDENTIFICATION                      TID  950
C                                                                       TID  960
  400 IRVT = IRVN - IDEL - IEXM                                         TID  970
      IF(IDEN(2)) 420,420,440                                           TID  980
  420 IF(IRVT) 480,480,540                                              TID  990
C                                                                       TID 1000
C     UNIT NNXT WAS NOT ASSIGNED                                        TID 1010
C                                                                       TID 1020
  440 IF(IDEL) 740,740,460                                              TID 1030
  460 WRITE (NOUT,960) UNIT(2)                                          TID 1040
      GOTO 740                                                          TID 1050
C                                                                       TID 1060
C     NO ACCEPTABLE RESONANCE SET REMAINS FOR UNIT NNXT                 TID 1070
C                                                                       TID 1080
  480 CALL PAGE(JT,0,2)                                                 TID 1090
C                                                                       TID 1100
      WRITE (NOUT,980) UNIT(2)                                          TID 1110
      IF(IDEN(1)) 500,500,520                                           TID 1120
  500 REWIND NOXT                                                       TID 1130
  520 CONTINUE                                                          TID 1140
      JT = 2                                                            TID 1150
  540 CONTINUE                                                          TID 1160
C                                                                       TID 1170
C     PRINT AND WRITE UNIT NNXT INDEX                                   TID 1180
C                                                                       TID 1190
      CALL PAGE(JT,0,8)                                                 TID 1200
C                                                                       TID 1210
      WRITE (NOUT,800) UNIT(2),(HEAD(M1),M1=1,4)                        TID 1220
      WRITE (NOUT,840) (HEAD(M1),M1=9,12)                               TID 1230
      NT = 0                                                            TID 1240
      DO 720 JT=1,IRVN                                                  TID 1250
        KT = INDEX(JT,3)                                                TID 1260
        IF(AZERO(KT)) 720,720,560                                       TID 1270
  560   IF(MMSH-MESH(KT)) 720,580,580                                   TID 1280
  580   IF(KU) 660,660,600                                              TID 1290
  600   DO 620 LT=1,4                                                   TID 1300
          IU = KU + (LT-1) * MRSV                                       TID 1310
          JU = KT + (LT-1) * MRSV                                       TID 1320
          IF(AZERO(JU)-AZERO(IU)) 660,620,660                           TID 1330
  620   CONTINUE                                                        TID 1340
        IF(NT) 660,660,640                                              TID 1350
C                                                                       TID 1360
  640   CALL PAGE(NT,1,10)                                              TID 1370
C                                                                       TID 1380
        GOTO(680,700),NT                                                TID 1390
  660   KU = KT                                                         TID 1400
C                                                                       TID 1410
        CALL PAGE(NT,6,10)                                              TID 1420
C                                                                       TID 1430
  680   WRITE (NOUT,860) AZERO(KT),G(KT),SIGPZ(KT),TEMP(KT)             TID 1440
        WRITE (NOUT,880)                                                TID 1450
  700   WRITE (NOUT,920) EZERO(KT),GAMN(KT),GMGM(KT),R(KT),S(KT)        TID 1460
  720 CONTINUE                                                          TID 1470
  740 RETURN                                                            TID 1480
C                                                                       TID 1490
  760 FORMAT (16A3)                                                     TID 1500
  780 FORMAT (1H0/21H0** OPERATOR ERROR **/1H0/1H0/34H0 WRONG TAPE MOUNTTID 1510
     1ED ON TAPE UNIT ,1A3/1H0/27H0 RERUN USING SPECIAL TAPE ,4A3,12H-  TID 1520
     1NOT TAPE ,4A3/1H0)                                                TID 1530
  800 FORMAT (20H   UNIT CONDITION - ,1A3,5H-    ,4A3)                  TID 1540
  820 FORMAT (25H0    TABLE OF CONTENTS - ,4A3,15H     GENERATED ,4A3)  TID 1550
  840 FORMAT (25H0    TABLE OF CONTENTS - ,4A3)                         TID 1560
  860 FORMAT (62H0             A ZERO             G    SIG P ZERO       TID 1570
     1   TEMP/6H      ,6(1PE14.5))                                      TID 1580
  880 FORMAT (90H0                           E ZERO       GAMMA N   GAMMTID 1590
     1A GAMMA             R             S)                              TID 1600
  900 FORMAT (20H                    ,5(1PE14.5),5H   **)               TID 1610
  920 FORMAT (20H                    ,5(1PE14.5))                       TID 1620
  940 FORMAT (74H0           NOTE - THOSE ENTRIES FOLLOWED BY ** WILL BETID 1630
     1 DELETED FROM UNIT ,1A3)                                          TID 1640
  960 FORMAT (42H0 **    ASSIGNMENT WAS NOT GIVEN FOR UNIT ,1A6,2H**)   TID 1650
  980 FORMAT (72H1    NO ACCEPTABLE RESOLVED RESONANCE SETS REMAIN TO BETID 1660
     1 WRITTEN ON UNIT ,1A3)                                            TID 1670
      END                                                               TID 1680
      SUBROUTINE RESOL                                                  RES   10
C                                                                       RES   20
      EXTERNAL CYL,CYLS,SLB,SLBS                                        RES   30
C                                                                       RES   40
      DIMENSION W(20),U(5),RRR(5),FX(5),EXTRA(8),DATE(2),FLAG(16),      RES   50
     1 HEAD(24),IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2), RES   60
     2 AZERO(500),G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),     RES   70
     3 GMGM(500),R(500),S(500),SOLVE(50),ABAR(50),C(50),DZERO(50),      RES   80
     4 AMOD1(50),SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),AMU(505),       RES   90
     5 CHI(505),DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),GAMMA(500),RES  100
     6 PSI(505),RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),SUB(501),     RES  110
     7 TERM(3),X(505),XI(500),INDEX(1000,4),BVAL(1001,3)                RES  120
C                                                                       RES  130
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          RES  140
C                                                                       RES  150
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        RES  160
C                                                                       RES  170
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            RES  180
C                                                                       RES  190
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          RES  200
C                                                                       RES  210
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           RES  220
C                                                                       RES  230
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           RES  240
C                                                                       RES  250
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       RES  260
C                                                                       RES  270
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        RES  280
C                                                                       RES  290
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            RES  300
C                                                                       RES  310
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     RES  320
C                                                                       RES  330
      COMMON /FLUSS/ FUB(501),VAZVG                                     RES  340
C                                                                       RES  350
      COMMON /GAGRU/ EGO(69),SIRA(69),FF,FL,M6,NGGR,NGGR1,EOE,EERSTE,   RES  360
     1 ELETZT,IO,IU,RIJS,MMEES,IIII,JRJR,ITUZ,IGRUG,M5,NEUI,NALTI,      RES  370
     2 UAPS(501)                                                        RES  380
C                                                                       RES  390
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          RES  400
C                                                                       RES  410
C	NEW VARIABLES
	INTEGER TMMEES,TNME,TIGRUP, AIMG/57/
	REAL PERT/0.0/

C     COMPUTES RESOLVED RESONANCE INTEGRAL AND WING CORRECTION          RES  420
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        RES  430
C     BEI MEHREREN FAELLEN HINTEREINANDER                               RES  440
C                                                                       RES  450
C                                                                       RES  460
      KFAOP$ = 6                                                        RES  470
      L = K                                                             RES  480
C	perturb SIGAZ group-wisely, like what GRUGA2 do
	TMMEES = MESH(I)
	DO 111 TNME=1, TMMEES
		TUONE = ALOG(1.0E7/EONE(I)) * 4. + (TNME-1) *  4. * EPSIL(I)
		TIGRUP = IFIX(TUONE) + 1
		IF (TIGRUP - AIMG) 112,113,112
  113		SIGAZ(TNME+2) = SIGAZ(TNME+2) * (1 + PERT)
  112		CONTINUE
C		WRITE(6,*) TUONE   
  111 CONTINUE



      DO 2120 IR=L,NAIN                                                 RES  490
        K = IR                                                          RES  500
        JR = INDEX(IR,4)                                                RES  510
        IF(I-INDEX(JR,1)) 20,40,20                                      RES  520
   20   JR = INDEX(JR,1)                                                RES  530
        IF(MMSH-MESH(JR)) 2120,2140,2140                                RES  540
   40   KR = INDEX(JR,2)                                                RES  550
        LR = 0                                                          RES  560
        IF(LR-I) 60,80,60                                               RES  570
C                                                                       RES  580
C     COMPUTE RESONANCE PARAMETERS                                      RES  590
C                                                                       RES  600
   60   ALPHA(1) = 4. * AZERO(I) / (AZERO(I)+1.)**2                     RES  610
        XONE = 2. * (EONE(I)-EZERO(I)) / GAMMA(I)                       RES  620
        ZONE = (EONE(I)-EZERO(I)) / EZERO(I)                            RES  630
C                                                                       RES  640
C     ANALYZE METHOD OF SOLUTION                                        RES  650
C     GEOMETRY                                                          RES  660
C                                                                       RES  670
   80   MR = 0.5 + SOLVE(KR)                                            RES  680
        IGEOM = 1 + MR / 10000                                          RES  690
C                                                                       RES  700
C     ABSORBER TERM                                                     RES  710
C                                                                       RES  720
        MR = MR - 10000 * (IGEOM-1)                                     RES  730
        LCP000 = MR / 1000                                              RES  740
C                                                                       RES  750
C     FIRST MODERATOR TERM                                              RES  760
C                                                                       RES  770
        MR = MR - 1000 * LCP000                                         RES  780
        IMOD1 = 1 + MR / 100                                            RES  790
C                                                                       RES  800
C     SECOND MODERATOR TERM                                             RES  810
C                                                                       RES  820
        MR = MR - 100 * (IMOD1-1)                                       RES  830
        IMOD2 = 1 + MR / 10                                             RES  840
C                                                                       RES  850
C     OUTPUT PROCEDURE                                                  RES  860
C                                                                       RES  870
        MR = MR - 10 * (IMOD2-1)                                        RES  880
        IOUT = 1 + MR                                                   RES  890
C                                                                       RES  900
C     CALCULATE QUANTITIES DEPENDENT UPON SOLVE(KR)                     RES  910
C                                                                       RES  920
        TERM(3) = 0.                                                    RES  930
C                                                                       RES  940
C     ABSORBER TERM                                                     RES  950
C                                                                       RES  960
        GOTO(100,120,140),LCP000                                        RES  970
C                                                                       RES  980
C     INTEGRAL FORM                                                     RES  990
C                                                                       RES 1000
  100   IVAL(1) = 1.5 + 4. / AZERO(I) / EPSIL(I)                        RES 1010
        GOTO 160                                                        RES 1020
C                                                                       RES 1030
C     NARROW RESONANCE FORM                                             RES 1040
C                                                                       RES 1050
  120   IVAL(1) = 0                                                     RES 1060
        TERM(3) = TERM(3) + SIGPZ(I)                                    RES 1070
        GOTO 160                                                        RES 1080
C                                                                       RES 1090
C     INFINITE MASS FORM                                                RES 1100
C                                                                       RES 1110
  140   IVAL(1) = 0                                                     RES 1120
C                                                                       RES 1130
C     FIRST MODERATOR TERM                                              RES 1140
C                                                                       RES 1150
  160   GOTO(180,200,260),IMOD1                                         RES 1160
C                                                                       RES 1170
C     TERM ABSENT                                                       RES 1180
C                                                                       RES 1190
  180   IVAL(2) = 0                                                     RES 1200
        SGM1 = 0.                                                       RES 1210
        GOTO 280                                                        RES 1220
C                                                                       RES 1230
C     INTEGRAL FORM                                                     RES 1240
C                                                                       RES 1250
  200   ALFA = 4. * AMOD1(KR) / (AMOD1(KR)+1.)**2                       RES 1260
        IVAL(2) = 1. - ALOG(1.-ALFA) / EPSIL(I)                         RES 1270
        MR = 1 + MOD(IVAL(2),2)                                         RES 1280
        GOTO(220,240),MR                                                RES 1290
  220   IVAL(2) = IVAL(2) + 1                                           RES 1300
  240   ALPHA(2) = 1. - EXP(-EPSIL(I)*FLOAT(IVAL(2)))                   RES 1310
        SGM1 = SIGM1(KR) * ALPHA(2) / ALFA                              RES 1320
        IF(IVAL(2) .LT. MIBV) GOTO 280                                  RES 1330
        IMOD1 = 2                                                       RES 1340
C                                                                       RES 1350
C     ASYMPTOTIC FORM                                                   RES 1360
C                                                                       RES 1370
  260   IVAL(2) = 0                                                     RES 1380
        SGM1 = SIGM1(KR)                                                RES 1390
        TERM(3) = TERM(3) + SGM1                                        RES 1400
C                                                                       RES 1410
C     SECOND MODERATOR TERM                                             RES 1420
C                                                                       RES 1430
  280   GOTO(300,320,380),IMOD2                                         RES 1440
C                                                                       RES 1450
C     TERM ABSENT                                                       RES 1460
C                                                                       RES 1470
  300   IVAL(3) = 0                                                     RES 1480
        SGM2 = 0.                                                       RES 1490
        GOTO 400                                                        RES 1500
C                                                                       RES 1510
C     INTEGRAL FORM                                                     RES 1520
C                                                                       RES 1530
  320   ALFA = 4. * AMOD2(KR) / (AMOD2(KR)+1.)**2                       RES 1540
        IVAL(3) = 1. - ALOG(1.-ALFA) / EPSIL(I)                         RES 1550
        MR = 1 + MOD(IVAL(3),2)                                         RES 1560
        GOTO(340,360),MR                                                RES 1570
  340   IVAL(3) = IVAL(3) + 1                                           RES 1580
  360   ALPHA(3) = 1. - EXP(-EPSIL(I)*FLOAT(IVAL(3)))                   RES 1590
        SGM2 = SIGM2(KR) * ALPHA(3) / ALFA                              RES 1600
        IF(IVAL(3) .LT. MIBV) GOTO 400                                  RES 1610
        IMOD2 = 2                                                       RES 1620
C                                                                       RES 1630
C     ASYMPTOTIC FORM                                                   RES 1640
C                                                                       RES 1650
  380   IVAL(3) = 0                                                     RES 1660
        SGM2 = SIGM2(KR)                                                RES 1670
        TERM(3) = TERM(3) + SGM2                                        RES 1680
C                                                                       RES 1690
C     GEOMETRY                                                          RES 1700
C                                                                       RES 1710
        IF(NEND-5) 400,401,400                                          RES 1720
  401   IGEOM = 1                                                       RES 1730
  400   GOTO(420,440,460,480),IGEOM                                     RES 1740
C                                                                       RES 1750
C     NO GEOMETRY                                                       RES 1760
C                                                                       RES 1770
  420   PZERO = 0.                                                      RES 1780
        SIGM = SGM1 + SGM2                                              RES 1790
        GOTO 520                                                        RES 1800
C                                                                       RES 1810
C     CYLINDRICAL GEOMETRY                                              RES 1820
C                                                                       RES 1830
  440   RBAR = 2. * ABAR(KR)                                            RES 1840
        GOTO 500                                                        RES 1850
C                                                                       RES 1860
C     SLAB GEOMETRY                                                     RES 1870
C                                                                       RES 1880
  460   RBAR = 4. * ABAR(KR)                                            RES 1890
        GOTO 500                                                        RES 1900
C                                                                       RES 1910
C     SPERICAL GEOMETRY                                                 RES 1920
C                                                                       RES 1930
  480   RBAR = 1.33333333E0 * ABAR(KR)                                  RES 1940
  500   SIGM = SGM1 + SGM2 + (1.-C(KR)) / RBAR / DZERO(KR)              RES 1950
C                                                                       RES 1960
C     CALCULATE CONSTITUENT INTEGRAND BACK VALUES                       RES 1970
C                                                                       RES 1980
  520   MR = MAX0(IVAL(1),IVAL(2),IVAL(3))                              RES 1990
        IF(MR*(MIBV-MR+1)) 540,600,560                                  RES 2000
C                                                                       RES 2010
C     NUMBER OF BACK VALUES EXCEEDS STORAGE CAPACITY                    RES 2020
C                                                                       RES 2030
  540   RI(JR) = -0.                                                    RES 2040
        DRI(JR) = -0.                                                   RES 2050
        GOTO 1260                                                       RES 2060
  560   DO 580 NR=1,MR                                                  RES 2070
          COEFT = EXP(-EPSIL(I)*FLOAT(NR)) / EONE(I)                    RES 2080
          BVAL(NR,1) = SIGPZ(I) * COEFT                                 RES 2090
          BVAL(NR,2) = SGM1 * COEFT                                     RES 2100
          BVAL(NR,3) = SGM2 * COEFT                                     RES 2110
  580   CONTINUE                                                        RES 2120
C                                                                       RES 2130
C     RESONANCE ABSORPTION LOOP                                         RES 2140
C                                                                       RES 2150
  600   MR = MESH(I)                                                    RES 2160
        DO 1100 NR=1,MR                                                 RES 2170
          SIGT = SIGAZ(NR+2) + SIGSZ(NR+2) + SGM1 + SGM2                RES 2180
          ARG = ABAR(KR) * DZERO(KR) * SIGT                             RES 2190
C                                                                       RES 2200
C     COMPUTE PZERO                                                     RES 2210
C     THE CONSTANT 0.577216 DEFINES EULERS NUMBER                       RES 2220
C                                                                       RES 2230
          GOTO(940,620,720,820),IGEOM                                   RES 2240
C                                                                       RES 2250
C     CYLINDRICAL GEOMETRY                                              RES 2260
C                                                                       RES 2270
  620     CONTINUE                                                      RES 2280
          IF(ARG-0.1) 660,680,640                                       RES 2290
  640     IF(ARG-5.98) 680,680,700                                      RES 2300
C                                                                       RES 2310
C     ARG LESS THAN 0.1                                                 RES 2320
C                                                                       RES 2330
  660     PZERO = 1. - 1.33333333E0 * ARG + 0.5 * (ALOG(2./ARG)+1.25-   RES 2340
     1     0.577216) * ARG**2                                           RES 2350
          GOTO 920                                                      RES 2360
C                                                                       RES 2370
C     ARG GREATER THAN OR EQUAL TO 0.1, LESS THAN OR EQUAL TO 6.0       RES 2380
C                                                                       RES 2390
  680     CALL NEWTON(ARG,PC,CYL,CYLS)                                  RES 2400
C                                                                       RES 2410
          PZERO = 1. - PC                                               RES 2420
          GOTO 920                                                      RES 2430
C                                                                       RES 2440
C     ARG GREATER THAN 6.0                                              RES 2450
C                                                                       RES 2460
  700     PZERO = 0.5 / ARG - 0.09375 / ARG**3                          RES 2470
          GOTO 920                                                      RES 2480
C                                                                       RES 2490
C     SLAB GEOMETRY                                                     RES 2500
C                                                                       RES 2510
  720     IF(ARG-0.05) 760,780,740                                      RES 2520
  740     IF(ARG-5.) 780,780,800                                        RES 2530
C                                                                       RES 2540
C     ARG LESS THAN 0.05                                                RES 2550
C                                                                       RES 2560
  760     PZERO = 1. + ARG * (ALOG(2.*ARG)-0.66666667E0*ARG+0.577216-1.5RES 2570
     1     )                                                            RES 2580
          GOTO 920                                                      RES 2590
C                                                                       RES 2600
C     ARG GREATER THAN OR EQUAL TO 0.05, LESS THAN OR EQUAL TO 5.0      RES 2610
C                                                                       RES 2620
  780     CALL NEWTON(ARG,PC,SLB,SLBS)                                  RES 2630
C                                                                       RES 2640
          PZERO = 1. - PC                                               RES 2650
          GOTO 920                                                      RES 2660
C                                                                       RES 2670
C     ARG GREATER THAN 5.0                                              RES 2680
C                                                                       RES 2690
  800     PZERO = 0.25 / ARG                                            RES 2700
          GOTO 920                                                      RES 2710
C                                                                       RES 2720
C     SPHERICAL GEOMETRY                                                RES 2730
C                                                                       RES 2740
  820     IF(ARG-0.02) 860,860,840                                      RES 2750
  840     IF(ARG-5.) 880,900,900                                        RES 2760
C                                                                       RES 2770
C     ARG LESS THAN OR EQUAL TO 0.02                                    RES 2780
C                                                                       RES 2790
  860     PZERO = 1. - 0.75 * ARG                                       RES 2800
          GOTO 920                                                      RES 2810
C                                                                       RES 2820
C     ARG GREATER THAN 0.02, LESS THAN 5.0                              RES 2830
C                                                                       RES 2840
  880     PZERO = 3. * ((2.*ARG)*ARG-1.+(1.+(2.*ARG))*EXP(-(2.*ARG))) / RES 2850
     1     (2.*ARG)**3                                                  RES 2860
          GOTO 920                                                      RES 2870
C                                                                       RES 2880
C     ARG GREATER THAN OR EQUAL TO 5.0                                  RES 2890
C                                                                       RES 2900
  900     PZERO = 3. * ((2.*ARG)*ARG-1.) / (2.*ARG)**3                  RES 2910
C                                                                       RES 2920
C     APPLY DANCOFF CORRECTION TO PZERO                                 RES 2930
C                                                                       RES 2940
  920     CONTINUE                                                      RES 2950
          PZERO = PZERO * (1.-C(KR)) / (1.-C(KR)+C(KR)*DZERO(KR)*PZERO* RES 2960
     1     RBAR*SIGT)                                                   RES 2970
  940     CONTINUE                                                      RES 2980
          IF(NEND-5) 949,941,949                                        RES 2990
  941     CONTINUE                                                      RES 3000
          EW(12) = (SGM1+SGM2) * DZERO(KR)                              RES 3010
          EW(14) = C(KR)                                                RES 3020
          EW(16) = DZERO(KR)                                            RES 3030
          EW(17) = ABAR(KR)                                             RES 3040
          EW(18) = EW(11)                                               RES 3050
          EW(11) = SIGSZ(NR+2)                                          RES 3060
          ARGU = ARG                                                    RES 3070
C                                                                       RES 3080
          CALL PINT(ARGU,PZEROU)                                        RES 3090
C                                                                       RES 3100
          PZERO = PZEROU                                                RES 3110
  949     TERM(1) = 0.                                                  RES 3120
C                                                                       RES 3130
C     CALCULATE COLLISION DENSITY                                       RES 3140
C                                                                       RES 3150
          TERM(2) = 0.                                                  RES 3160
C                                                                       RES 3170
C     ABSORBER TERM                                                     RES 3180
C                                                                       RES 3190
          GOTO(960,1000,980),LCP000                                     RES 3200
C                                                                       RES 3210
C     INTEGRAL FORM                                                     RES 3220
C                                                                       RES 3230
  960     TERM(1) = TERM(1) + SIMPS(1,IVAL(1),BVAL(1,1)) / ALPHA(1)     RES 3240
C                                                                       RES 3250
          TERM(2) = TERM(2) + EPSIL(I) * SIGSZ(NR+2) / 3. / ALPHA(1)    RES 3260
          GOTO 1000                                                     RES 3270
C                                                                       RES 3280
C     INFINITE MASS FORM                                                RES 3290
C                                                                       RES 3300
  980     TERM(2) = TERM(2) + SIGSZ(NR+2)                               RES 3310
C                                                                       RES 3320
C     FIRST MODERATOR TERM                                              RES 3330
C                                                                       RES 3340
 1000     GOTO(1040,1020,1040),IMOD1                                    RES 3350
C                                                                       RES 3360
C     INTEGRAL FORM                                                     RES 3370
C                                                                       RES 3380
 1020     TERM(1) = TERM(1) + SIMPS(1,IVAL(2),BVAL(1,2)) / ALPHA(2)     RES 3390
C                                                                       RES 3400
          TERM(2) = TERM(2) + EPSIL(I) * SGM1 / 3. / ALPHA(2)           RES 3410
C                                                                       RES 3420
C     SECOND MODERATOR TERM                                             RES 3430
C                                                                       RES 3440
 1040     GOTO(1080,1060,1080),IMOD2                                    RES 3450
C                                                                       RES 3460
C     INTEGRAL FORM                                                     RES 3470
C                                                                       RES 3480
 1060     TERM(1) = TERM(1) + SIMPS(1,IVAL(3),BVAL(1,3)) / ALPHA(3)     RES 3490
C                                                                       RES 3500
          TERM(2) = TERM(2) + EPSIL(I) * SGM2 / 3. / ALPHA(3)           RES 3510
 1080     F(NR) = ((1.-PZERO)*EPSIL(I)*TERM(1)+((1.-PZERO)*TERM(3)+PZERORES 3520
     1     *SIGT)*EXP(AMU(NR+2))/EONE(I)) / (1.-(1.-PZERO)*TERM(2)/SIGT)RES 3530
C                                                                       RES 3540
C     CALCULATE CONSTITUENT AND RESONANCE INTEGRANDS                    RES 3550
C                                                                       RES 3560
          BVAL(1,1) = F(NR) * SIGSZ(NR+2) / SIGT                        RES 3570
          BVAL(1,2) = F(NR) * SGM1 / SIGT                               RES 3580
          BVAL(1,3) = F(NR) * SGM2 / SIGT                               RES 3590
          FUB(NR) = F(NR) * EXP(-AMU(NR+2)) / SIGT                      RES 3600
          SUB(NR) = FUB(NR) * SIGAZ(NR+2)                               RES 3610
 1100   CONTINUE                                                        RES 3620
C                                                                       RES 3630
C     CALCULATE RESONANCE INTEGRAL                                      RES 3640
C                                                                       RES 3650
        EOE = EONE(I) * EPSIL(I)                                        RES 3660
        MMEES = MESH(I)                                                 RES 3670
        ELETZT = EZERO(I)                                               RES 3680
C                                                                       RES 3690
        CALL GRUGA1                                                     RES 3700
C                                                                       RES 3710
        RI(JR) = RIJS                                                   RES 3720
C                                                                       RES 3730
C     CALCULATE WING CORRECTION                                         RES 3740
C                                                                       RES 3750
        FF = 0.5                                                        RES 3760
        FL = 0.5                                                        RES 3770
        IF(ZONE-0.1) 1120,1180,1140                                     RES 3780
 1120   IF(ZONE-2./AZERO(I)) 1240,1240,1220                             RES 3790
 1140   IF(ZONE-0.57142857E0) 1180,1160,1160                            RES 3800
 1160   GAMM = 0.                                                       RES 3810
        FF = 1.                                                         RES 3820
        FL = 0.                                                         RES 3830
        GOTO 1200                                                       RES 3840
 1180   GAMM = (1.-3.*ZONE) / ZONE / (SQRT(1.-ZONE)) - 0.44055 + 1.5 *  RES 3850
     1   ALOG((1.+(SQRT(1.-ZONE)))/(1.-(SQRT(1.-ZONE))))                RES 3860
 1200   GAMP = (1.+3.*ZONE) / ZONE / (SQRT(1.+ZONE)) - 1.5 *            RES 3870
     1   ALOG(((SQRT(1.+ZONE))+1.)/((SQRT(1.+ZONE))-1.))                RES 3880
        DRI(JR) = GMGM(I) * GAMMA(I) * SIGOH(I) * (GAMM+GAMP) / 4. /    RES 3890
     1   EZERO(I)**2                                                    RES 3900
        IDRI = 1200                                                     RES 3910
        GOTO 1260                                                       RES 3920
C                                                                       RES 3930
C     NARROW RESONANCE WING CORRECTION                                  RES 3940
C                                                                       RES 3950
 1220   BETA = (SIGPZ(I)+SIGM) / SIGOH(I)                               RES 3960
        GAM = SQRT(GAMN(I)*SIGPZ(I)/G(I)/GAMMA(I)/SIGOH(I))             RES 3970
        DRI(JR) = GMGM(I) * SIGOH(I) * BETA * (1.5707963E0-0.5*         RES 3980
     1   (ATAN(((BETA*XONE)+GAM)/(SQRT(BETA*(1.+BETA)-GAM**2)))+        RES 3990
     2   ATAN(((BETA*XONE)-GAM)/(SQRT(BETA*(1.+BETA)-GAM**2))))) /      RES 4000
     3   EZERO(I) / (SQRT(BETA*(1.+BETA)-GAM**2))                       RES 4010
        IDRI = 1220                                                     RES 4020
        GOTO 1260                                                       RES 4030
C                                                                       RES 4040
C     INFINITE MASS WING CORRECTION                                     RES 4050
C                                                                       RES 4060
 1240   BETAP = GAMMA(I) * SIGM / GMGM(I) / SIGOH(I)                    RES 4070
        DRI(JR) = GMGM(I) * SIGOH(I) * BETAP * (1.5707963E0-ATAN(BETAP* RES 4080
     1   XONE/(SQRT(BETAP*(1.+BETAP))))) / EZERO(I) / (SQRT(BETAP*(1.+  RES 4090
     2   BETAP)))                                                       RES 4100
C                                                                       RES 4110
C     EXTRA RESOLVED RESONANCE CALCULATION OUTPUT                       RES 4120
C                                                                       RES 4130
        IDRI = 1240                                                     RES 4140
 1260   CONTINUE                                                        RES 4150
        IIII = I                                                        RES 4160
        JRJR = JR                                                       RES 4170
C                                                                       RES 4180
        CALL GRUGA2                                                     RES 4190
C                                                                       RES 4200
        GOTO(2120,1280,1520,1520,1280,1280,1520,1280),IOUT              RES 4210
C                                                                       RES 4220
C     PRINT CROSS SECTION DATA                                          RES 4230
C                                                                       RES 4240
 1280   IF(I-LR) 1300,1480,1300                                         RES 4250
 1300   LR = 1 + MESH(I) / 2                                            RES 4260
C                                                                       RES 4270
        CALL PAGE(MR,0,20)                                              RES 4280
C                                                                       RES 4290
        IF(I-IRVO) 1320,1320,1340                                       RES 4300
 1320   WRITE (NOUT,3000) DATE(1),DATE(2)                               RES 4310
        GOTO 1360                                                       RES 4320
 1340   WRITE (NOUT,3000) (HEAD(M1),M1=1,4)                             RES 4330
 1360   WRITE (NOUT,3020)                                               RES 4340
        WRITE (NOUT,3040)                                               RES 4350
        WRITE (NOUT,3080) AZERO(I),G(I),SIGPZ(I),TEMP(I)                RES 4360
        WRITE (NOUT,3060)                                               RES 4370
        WRITE (NOUT,3100) EZERO(I),GAMN(I),GMGM(I),R(I),S(I)            RES 4380
        WRITE (NOUT,3120) XI(I)                                         RES 4390
        WRITE (NOUT,3140)                                               RES 4400
        DO 1460 MR=1,LR                                                 RES 4410
C                                                                       RES 4420
          CALL PAGE(NR,1,7)                                             RES 4430
C                                                                       RES 4440
          GOTO(1380,1400),NR                                            RES 4450
 1380     WRITE (NOUT,3140)                                             RES 4460
 1400     NR = LR + MR                                                  RES 4470
          IF(MESH(I)-NR) 1420,1440,1440                                 RES 4480
 1420     WRITE (NOUT,3080) AMU(MR+2),SIGAZ(MR+2),SIGSZ(MR+2)           RES 4490
          GOTO 1460                                                     RES 4500
 1440     WRITE (NOUT,3080) AMU(MR+2),SIGAZ(MR+2),SIGSZ(MR+2),AMU(NR+2),RES 4510
     1     SIGAZ(NR+2),SIGSZ(NR+2)                                      RES 4520
 1460   CONTINUE                                                        RES 4530
        LR = I                                                          RES 4540
 1480   IF(IDEN(1)+IDEN(2)-2) 1500,1520,1520                            RES 4550
 1500   GOTO(2120,2120,1520,1520,1520,1520,1520,1520),IOUT              RES 4560
C                                                                       RES 4570
C     PRINT RESONANCE SUMMARY                                           RES 4580
C     SOLUTION TITLES                                                   RES 4590
C     GEOMETRY                                                          RES 4600
C                                                                       RES 4610
 1520   CONTINUE                                                        RES 4620
        GOTO(1540,1560,1580,1600),IGEOM                                 RES 4630
C                                                                       RES 4640
 1540   CALL TITLE(1,REASON)                                            RES 4650
C                                                                       RES 4660
        GOTO 1620                                                       RES 4670
C                                                                       RES 4680
 1560   CALL TITLE(2,REASON)                                            RES 4690
C                                                                       RES 4700
        GOTO 1620                                                       RES 4710
C                                                                       RES 4720
 1580   CALL TITLE(3,REASON)                                            RES 4730
C                                                                       RES 4740
        GOTO 1620                                                       RES 4750
 1600   CALL TITLE(4,REASON)                                            RES 4760
C                                                                       RES 4770
C     ABSORBER                                                          RES 4780
C                                                                       RES 4790
 1620   GOTO(1640,1660,1680),LCP000                                     RES 4800
C                                                                       RES 4810
 1640   CALL TITLE(5,REASON)                                            RES 4820
C                                                                       RES 4830
        GOTO 1700                                                       RES 4840
C                                                                       RES 4850
 1660   CALL TITLE(6,REASON)                                            RES 4860
C                                                                       RES 4870
        GOTO 1700                                                       RES 4880
C                                                                       RES 4890
 1680   CALL TITLE(7,REASON)                                            RES 4900
C                                                                       RES 4910
C     FIRST MODERATOR                                                   RES 4920
C                                                                       RES 4930
 1700   GOTO(1720,1740,1760),IMOD1                                      RES 4940
C                                                                       RES 4950
 1720   CALL TITLE(8,REASON)                                            RES 4960
C                                                                       RES 4970
        GOTO 1780                                                       RES 4980
C                                                                       RES 4990
 1740   CALL TITLE(5,REASON)                                            RES 5000
C                                                                       RES 5010
        GOTO 1780                                                       RES 5020
C                                                                       RES 5030
 1760   CALL TITLE(9,REASON)                                            RES 5040
C                                                                       RES 5050
C     SECOND MODERATOR                                                  RES 5060
C                                                                       RES 5070
 1780   GOTO(1800,1820,1840),IMOD2                                      RES 5080
C                                                                       RES 5090
 1800   CALL TITLE(8,REASON)                                            RES 5100
C                                                                       RES 5110
        GOTO 1860                                                       RES 5120
C                                                                       RES 5130
 1820   CALL TITLE(5,REASON)                                            RES 5140
C                                                                       RES 5150
        GOTO 1860                                                       RES 5160
C                                                                       RES 5170
 1840   CALL TITLE(9,REASON)                                            RES 5180
C                                                                       RES 5190
 1860   SRI = RI(JR) + DRI(JR)                                          RES 5200
C                                                                       RES 5210
        CALL PAGE(MR,-1,37)                                             RES 5220
C                                                                       RES 5230
        WRITE (NOUT,3160) (HEAD(MR),MR=1,4),(HEAD(MR),MR=13,24)         RES 5240
        WRITE (NOUT,3020)                                               RES 5250
        WRITE (NOUT,3180) AZERO(I),G(I),SIGPZ(I),TEMP(I),EZERO(I),      RES 5260
     1   GAMN(I),GMGM(I),R(I),S(I),SOLVE(KR),ABAR(KR),C(KR),DZERO(KR),  RES 5270
     2   AMOD1(KR),SIGM1(KR),AMOD2(KR),SIGM2(KR)                        RES 5280
        WRITE (NOUT,3200) (REASON(MR),MR=1,32)                          RES 5290
        WRITE (NOUT,3220)                                               RES 5300
        IF(SRI) 1880,1880,1900                                          RES 5310
 1880   WRITE (NOUT,3240)                                               RES 5320
        GOTO 2080                                                       RES 5330
 1900   WRITE (NOUT,3260) RI(JR),DRI(JR),SRI                            RES 5340
        IF(IDEN(1)+IDEN(2)-2) 1920,1940,1940                            RES 5350
 1920   GOTO(2120,2120,1940,2100,1940,2120,1940,1940),IOUT              RES 5360
C                                                                       RES 5370
C     PRINT COLLISION DENSITY DISTRIBUTION                              RES 5380
C                                                                       RES 5390
 1940   JR = 1 + MESH(I) / 4                                            RES 5400
C                                                                       RES 5410
        CALL PAGE(KR,17,8)                                              RES 5420
C                                                                       RES 5430
        WRITE (NOUT,3280)                                               RES 5440
        WRITE (NOUT,3300)                                               RES 5450
        DO 2060 KR=1,JR                                                 RES 5460
          DO 1960 MR=1,4                                                RES 5470
            NR = KR + JR * (MR-1)                                       RES 5480
            EXTRA(MR) = AMU(NR+2)                                       RES 5490
            EXTRA(MR+4) = F(NR)                                         RES 5500
 1960     CONTINUE                                                      RES 5510
C                                                                       RES 5520
          CALL PAGE(MR,1,7)                                             RES 5530
C                                                                       RES 5540
          GOTO(1980,2000),MR                                            RES 5550
 1980     WRITE (NOUT,3300)                                             RES 5560
 2000     IF(MESH(I)-NR) 2020,2040,2040                                 RES 5570
 2020     WRITE (NOUT,3320) (EXTRA(MR),EXTRA(MR+4),MR=1,3)              RES 5580
          GOTO 2060                                                     RES 5590
 2040     WRITE (NOUT,3320) (EXTRA(MR),EXTRA(MR+4),MR=1,4)              RES 5600
 2060   CONTINUE                                                        RES 5610
 2080   GOTO(2120,2120,2120,2100,2120,2100,2100,2100),IOUT              RES 5620
 2100   CONTINUE                                                        RES 5630
 2120 CONTINUE                                                          RES 5640
 2140 CONTINUE                                                          RES 5650
      RETURN                                                            RES 5660
C                                                                       RES 5670
 3000 FORMAT (47H   RESOLVED RESONANCE CROSS SECTIONS GENERATED ,4A3)   RES 5680
 3020 FORMAT (22H0  INPUT PARAMETERS  -)                                RES 5690
 3040 FORMAT (76H0                           A ZERO             G    SIGRES 5700
     1 P ZERO          TEMP)                                            RES 5710
 3060 FORMAT (104H0                                         E ZERO      RES 5720
     1 GAMMA N   GAMMA GAMMA             R             S)               RES 5730
 3080 FORMAT (20H                    ,6(1PE14.5))                       RES 5740
 3100 FORMAT (34H                                  ,6(1PE14.5))         RES 5750
 3120 FORMAT (21H0  CALCULATED DATA  -/23H0                 XI  #,      RES 5760
     1 1PE11.5)                                                         RES 5770
 3140 FORMAT (104H0                               MU    SIG A ZERO    SIRES 5780
     1G S ZERO            MU    SIG A ZERO    SIG S ZERO)               RES 5790
 3160 FORMAT (3H   ,16A3)                                               RES 5800
 3180 FORMAT (119H0        A ZERO            G   SIG P ZERO         TEMPRES 5810
     1       E ZERO      GAMMA N  GAMMA GAMMA            R            S/RES 5820
     2 2H  ,9(1PE13.5)/119H0      SOLUTION                     A BAR    RES 5830
     3        C       N ZERO       A MOD1     SIG MOD1       A MOD2     RES 5840
     4SIG MOD2/2H  ,1PE13.5,13H             ,7(1PE13.5))                RES 5850
 3200 FORMAT (32H0  COLLISION DENSITY EQUATION  -/24H         GEOMETRY  RES 5860
     1   - ,8A3,18H   ABSORBER     - ,8A3/24H         MODERATOR 1  - ,  RES 5870
     2 8A3,18H   MODERATOR 2  - ,8A3)                                   RES 5880
 3220 FORMAT (29H0  RESOLVED RESONANCE DATA  -)                         RES 5890
 3240 FORMAT (50H         INTEGRAL AND WING CORRECTION NOT COMPUTED/62H RES 5900
     1        REQUIRED INTEGRATION BACK VALUES EXCEED STORAGE LIMIT)    RES 5910
 3260 FORMAT (41H         RESONANCE INTEGRAL              ,1PE13.5/41H  RES 5920
     1       WING CORRECTION                 ,1PE13.5/54H            SUMRES 5930
     2                                       ,1PE13.5)                  RES 5940
 3280 FORMAT (36H0  COLLISION DENSITY DISTRIBUTION  -)                  RES 5950
 3300 FORMAT (118H0                 MU    F%MU<                 MU    F%RES 5960
     1MU<                 MU    F%MU<                 MU    F%MU<     ) RES 5970
 3320 FORMAT (6H      ,8(1PE14.5))                                      RES 5980
      END                                                               RES 5990
      SUBROUTINE TOTAL                                                  TOT   10
C                                                                       TOT   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      TOT   30
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),AZERO(500)TOT   40
     2 ,G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),GMGM(500),     TOT   50
     3 R(500),S(500),SOLVE(50),ABAR(50),C(50),DZERO(50),AMOD1(50),      TOT   60
     4 SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),AMU(505),CHI(505),        TOT   70
     5 DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),GAMMA(500),PSI(505),TOT   80
     6 RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),SUB(501),TERM(3),X(505)TOT   90
     7 ,XI(500),ENULL(500),CORINT(500),INDEX(1000,4),BVAL(1001,3),      TOT  100
     8 LOESCH(9)                                                        TOT  110
C                                                                       TOT  120
      COMMON /GAM/ M6,NGAM,IDENT,IDSATZ,IDNUCL,ENULL,CORINT,NUM1,LOESCH,TOT  130
     1 M9                                                               TOT  140
C                                                                       TOT  150
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          TOT  160
C                                                                       TOT  170
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        TOT  180
C                                                                       TOT  190
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            TOT  200
C                                                                       TOT  210
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          TOT  220
C                                                                       TOT  230
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           TOT  240
C                                                                       TOT  250
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           TOT  260
C                                                                       TOT  270
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       TOT  280
C                                                                       TOT  290
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        TOT  300
C                                                                       TOT  310
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            TOT  320
C                                                                       TOT  330
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     TOT  340
C                                                                       TOT  350
      COMMON /UI/ IDUM(4),KS,LS,MS                                      TOT  360
C                                                                       TOT  370
      COMMON /ENDIV/ NWING0                                             TOT  380
C                                                                       TOT  390
      DATA FCP016/3H   /,FCP017/3H IN/,FCP018/3H   /,FCP019/3H NR/,     TOT  400
     1 FCP020/3H   /,FCP021/3H IM/,FCP022/3H000/,FCP023/3H000/,         TOT  410
     2 FCP024/3H000/,FCP025/3H,AB/,FCP026/3H,IN/,FCP027/3H,AS/,         TOT  420
     3 FCP028/3H,AB/,FCP029/3H,IN/,FCP030/3H,AS/                        TOT  430
C                                                                       TOT  440
C     CALCULATES TOTAL RESOLVED RESONANCE INTEGRALS                     TOT  450
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        TOT  460
C     BEI MEHREREN FAELLEN HINTEREINANDER                               TOT  470
C                                                                       TOT  480
C                                                                       TOT  490
      NUM1 = 0                                                          TOT  500
C                                                                       TOT  510
C     REPLACE INDEX(IT,3) BY IGEOM OF RESONANCE CONFIGURATION 9T        TOT  520
C                                                                       TOT  530
      DO 20 IT=1,ICNN                                                   TOT  540
        INDEX(IT,3) = 1 + IFIX(0.5+SOLVE(IT)) / 10000                   TOT  550
   20 CONTINUE                                                          TOT  560
C                                                                       TOT  570
C     SORT INDEX(IT,4) SUCH THAT INPUT(INDEX(INDEX(IT,4),2)) INCREASES  TOT  580
C                                                                       TOT  590
      CALL SORT(NAIN,ABAR,INDEX(1,2),INDEX(1,4))                        TOT  600
C                                                                       TOT  610
      CALL SORT(NAIN,DZERO,INDEX(1,2),INDEX(1,4))                       TOT  620
C                                                                       TOT  630
      CALL SORT(NAIN,SIGM2,INDEX(1,2),INDEX(1,4))                       TOT  640
C                                                                       TOT  650
      CALL SORT(NAIN,AMOD2,INDEX(1,2),INDEX(1,4))                       TOT  660
C                                                                       TOT  670
      CALL SORT(NAIN,SIGM1,INDEX(1,2),INDEX(1,4))                       TOT  680
C                                                                       TOT  690
      CALL SORT(NAIN,AMOD1,INDEX(1,2),INDEX(1,4))                       TOT  700
C                                                                       TOT  710
      CALL SORT(NAIN,C,INDEX(1,2),INDEX(1,4))                           TOT  720
C                                                                       TOT  730
      CALL SORT(NAIN,INDEX(1,3),INDEX(1,2),INDEX(1,4))                  TOT  740
C                                                                       TOT  750
C     PERFORM SUMMATION                                                 TOT  760
C                                                                       TOT  770
      WING = 0.                                                         TOT  780
      RUMF = 0.                                                         TOT  790
      DO 580 IT=1,NAIN                                                  TOT  800
        JT = INDEX(IT,4)                                                TOT  810
        KT = INDEX(JT,1)                                                TOT  820
        IF(AZERO(KT)) 580,580,40                                        TOT  830
   40   IF(MMSH-MESH(KT)) 580,60,60                                     TOT  840
   60   LT = INDEX(JT,2)                                                TOT  850
        MT = INDEX(LT,3)                                                TOT  860
        IF (TRI)160,160,80                                              TOT  870
C                                                                       TOT  880
C     COMPARE DATA SETS (IT) AND (IT-1)                                 TOT  890
C     (AZERO,G,SIGPZ,TEMP)                                              TOT  900
C                                                                       TOT  910
   80   DO 100 IS=1,4                                                   TOT  920
          JS = (IS-1) * MRSV                                            TOT  930
          NT = JS + KT                                                  TOT  940
          NS = JS + KS                                                  TOT  950
          IF(AZERO(NS)-AZERO(NT)) 140,100,140                           TOT  960
  100   CONTINUE                                                        TOT  970
C                                                                       TOT  980
C     (ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2)                            TOT  990
C                                                                       TOT 1000
        DO 120 IS=1,7                                                   TOT 1010
          JS = (IS-1) * MCON                                            TOT 1020
          NT = JS + LT                                                  TOT 1030
          NS = JS + LS                                                  TOT 1040
          IF(ABAR(NS)-ABAR(NT)) 140,120,140                             TOT 1050
  120   CONTINUE                                                        TOT 1060
C                                                                       TOT 1070
C     GEOMETRY                                                          TOT 1080
C                                                                       TOT 1090
        IF(MS-MT) 140,280,140                                           TOT 1100
C                                                                       TOT 1110
C     PRINT AND RE-INITIALIZE TOTAL RESOLVED RESONANCE INTEGRAL         TOT 1120
C                                                                       TOT 1130
  140   CALL PAGE(NT,2,6)                                               TOT 1140
C                                                                       TOT 1150
        WRITE (NOUT,740) TRI                                            TOT 1160
        TRI = 0.                                                        TOT 1170
        WING = 0.                                                       TOT 1180
        RUMF = 0.                                                       TOT 1190
C                                                                       TOT 1200
C     ANALYZE GEOMETRY                                                  TOT 1210
C                                                                       TOT 1220
  160   GOTO(180,200,220,240),MT                                        TOT 1230
C                                                                       TOT 1240
  180   CALL TITLE(1,REASON)                                            TOT 1250
C                                                                       TOT 1260
        GOTO 260                                                        TOT 1270
C                                                                       TOT 1280
  200   CALL TITLE(2,REASON)                                            TOT 1290
C                                                                       TOT 1300
        GOTO 260                                                        TOT 1310
C                                                                       TOT 1320
  220   CALL TITLE(3,REASON)                                            TOT 1330
C                                                                       TOT 1340
        GOTO 260                                                        TOT 1350
C                                                                       TOT 1360
  240   CALL TITLE(4,REASON)                                            TOT 1370
C                                                                       TOT 1380
C     PRINT RESOLVED RESONANCE PARAMETERS                               TOT 1390
C                                                                       TOT 1400
  260   CALL PAGE(NT,0,18)                                              TOT 1410
C                                                                       TOT 1420
        WRITE (NOUT,600) (HEAD(NT),NT=1,4),(HEAD(NT),NT=13,24)          TOT 1430
        WRITE (NOUT,620)                                                TOT 1440
        WRITE (NOUT,640) AZERO(KT),G(KT),AMOD1(LT),AMOD2(LT),DZERO(LT), TOT 1450
     1   (REASON(NT),NT=1,8),TEMP(KT)                                   TOT 1460
        WRITE (NOUT,660) SIGPZ(KT),SIGM1(LT),SIGM2(LT),ABAR(LT),C(LT)   TOT 1470
        IF(NWING0 .GT. 0) GOTO 771                                      TOT 1480
        WRITE (NOUT,680)                                                TOT 1490
        WRITE (NOUT,700)                                                TOT 1500
  771   CONTINUE                                                        TOT 1510
C                                                                       TOT 1520
C     TOTAL CUMULATIVE RESONANCE INTEGRALS                              TOT 1530
C                                                                       TOT 1540
  280   KS = KT                                                         TOT 1550
        LS = LT                                                         TOT 1560
        MS = MT                                                         TOT 1570
        SRI = RI(JT) + DRI(JT)                                          TOT 1580
        TRI = SRI + TRI                                                 TOT 1590
        WING = WING + DRI(JT)                                           TOT 1600
        RUMF = RUMF + RI(JT)                                            TOT 1610
C                                                                       TOT 1620
C     ANALYZE COLLISION DENSITY CONSTITUENTS                            TOT 1630
C     ABSORBER                                                          TOT 1640
C                                                                       TOT 1650
        IS = IFIX(0.5+SOLVE(LS)) - 10000 * (MS-1)                       TOT 1660
        JS = IS / 1000                                                  TOT 1670
        GOTO(300,320,340),JS                                            TOT 1680
  300   REASON(1) = (+FCP016)                                           TOT 1690
        REASON(2) = FCP017                                              TOT 1700
        GOTO 360                                                        TOT 1710
  320   REASON(1) = (+FCP018)                                           TOT 1720
        REASON(2) = FCP019                                              TOT 1730
        GOTO 360                                                        TOT 1740
  340   REASON(1) = (+FCP020)                                           TOT 1750
        REASON(2) = FCP021                                              TOT 1760
C                                                                       TOT 1770
C     FIRST MODERATOR                                                   TOT 1780
C                                                                       TOT 1790
  360   IS = IS - 1000 * JS                                             TOT 1800
        JS = 1 + IS / 100                                               TOT 1810
        GOTO(380,400,420),JS                                            TOT 1820
  380   REASON(3) = FCP025                                              TOT 1830
        REASON(4) = FCP022                                              TOT 1840
        GOTO 440                                                        TOT 1850
  400   REASON(3) = FCP026                                              TOT 1860
        REASON(4) = FCP023                                              TOT 1870
        GOTO 440                                                        TOT 1880
  420   REASON(3) = FCP027                                              TOT 1890
        REASON(4) = FCP024                                              TOT 1900
C                                                                       TOT 1910
C     SECOND MODERATOR                                                  TOT 1920
C                                                                       TOT 1930
  440   IS = IS - 100 * (JS-1)                                          TOT 1940
        JS = 1 + IS / 10                                                TOT 1950
        GOTO(460,480,500),JS                                            TOT 1960
  460   REASON(4) = FCP028                                              TOT 1970
        GOTO 520                                                        TOT 1980
  480   REASON(4) = FCP029                                              TOT 1990
        GOTO 520                                                        TOT 2000
  500   REASON(4) = FCP030                                              TOT 2010
C                                                                       TOT 2020
C     PRINT INDIVIDUAL RESONANCE PARAMETERS AND INTEGRALS               TOT 2030
C                                                                       TOT 2040
  520   IF(NWING0 .GT. 0) GOTO 772                                      TOT 2050
C                                                                       TOT 2060
        CALL PAGE(NS,1,7)                                               TOT 2070
C                                                                       TOT 2080
        GOTO(540,560),NS                                                TOT 2090
  540   WRITE (NOUT,700)                                                TOT 2100
  560   IF(NWING0 .LE. 0) WRITE (NOUT,720) EZERO(KS),GAMN(KS),GMGM(KS), TOT 2110
     1   R(KS),S(KS),(REASON(NS),NS=1,4),RI(JT),SRI                     TOT 2120
  772   CONTINUE                                                        TOT 2130
        NUM1 = NUM1 + 1                                                 TOT 2140
        ENULL(NUM1) = EZERO(KS)                                         TOT 2150
        CORINT(NUM1) = SRI                                              TOT 2160
  580 CONTINUE                                                          TOT 2170
C                                                                       TOT 2180
C     PRINT LAST TOTAL                                                  TOT 2190
C                                                                       TOT 2200
      IF(NWING0 .GT. 0) GOTO 773                                        TOT 2210
C                                                                       TOT 2220
      CALL PAGE(NS,2,6)                                                 TOT 2230
C                                                                       TOT 2240
      WRITE (NOUT,740) TRI                                              TOT 2250
      WRITE (NOUT,750) RUMF                                             TOT 2260
      WRITE (NOUT,760) WING                                             TOT 2270
  773 CONTINUE                                                          TOT 2280
      NUM1 = NUM1 - 1                                                   TOT 2290
      IF(IDNUCL .EQ. 0) GOTO 589                                        TOT 2300
C                                                                       TOT 2310
      CALL ZUTGAM                                                       TOT 2320
C                                                                       TOT 2330
  589 CONTINUE                                                          TOT 2340
      RETURN                                                            TOT 2350
C                                                                       TOT 2360
  600 FORMAT (3H   ,16A3)                                               TOT 2370
  620 FORMAT (22H0  INPUT PARAMETERS  -)                                TOT 2380
  640 FORMAT (119H0        A ZERO            G       A MOD1       A MOD2TOT 2390
     1       N ZERO     GEOMETRY                                   TEMP/TOT 2400
     2 2H  ,5(1PE13.5),3H   ,8A3,12H            ,1PE13.5)               TOT 2410
  660 FORMAT (93H0                 SIG P ZERO     SIG MOD1     SIG MOD2 TOT 2420
     1                    A BAR            C/15H               ,        TOT 2430
     2 3(1PE13.5),13H             ,2(1PE13.5))                          TOT 2440
  680 FORMAT (29H0  RESOLVED RESONANCE DATA  -)                         TOT 2450
  700 FORMAT (119H0        E ZERO      GAMMA N  GAMMA GAMMA            RTOT 2460
     1            S     SOLUTION     INTEGRAL   CORRECTION        TOTAL)TOT 2470
  720 FORMAT (2H  ,5(1PE13.5),1H ,4A3,3(1PE13.5))                       TOT 2480
  740 FORMAT (67H0    TOTAL RESONANCE INTEGRAL (RESOLVED RESONANCES)  = TOT 2490
     1            ,1PE13.5)                                             TOT 2500
  750 FORMAT (67H0                             (           INTEGRAL   = TOT 2510
     1            ,1PE13.5,' )')                                        TOT 2520
  760 FORMAT (67H0                             (         CORRECTION   = TOT 2530
     1            ,1PE13.5,' )')                                        TOT 2540
      END                                                               TOT 2550
      SUBROUTINE GAUSS(A,B,N,FUNC,ANS)                                  GAU   10
C                                                                       GAU   20
      DIMENSION U(5),RRR(5),FX(5)                                       GAU   30
C                                                                       GAU   40
      COMMON U,RRR,FX,LINE,KU                                           GAU   50
C                                                                       GAU   60
C                                                                       GAU   70
C     TEN POINT GAUSSIAN QUADRATURE OVER N INTERVALS                    GAU   80
C                                                                       GAU   90
      IF(U(1)-0.0130467357E0) 1,2,1                                     GAU  100
    1 U(1) = 0.0130467357E0                                             GAU  110
      U(2) = 0.0674683167E0                                             GAU  120
      U(3) = 0.1602952159E0                                             GAU  130
      U(4) = 0.2833023029E0                                             GAU  140
      U(5) = 0.4255628305E0                                             GAU  150
      RRR(1) = 0.03333567215E0                                          GAU  160
      RRR(2) = 0.07472567458E0                                          GAU  170
      RRR(3) = 0.1095431813E0                                           GAU  180
      RRR(4) = 0.1346333597E0                                           GAU  190
      RRR(5) = 0.1477621124E0                                           GAU  200
    2 RANG = B - A                                                      GAU  210
      IF(RANG) 3,6,3                                                    GAU  220
    3 FRAC = RANG / FLOAT(N)                                            GAU  230
      DO 4 J=1,2                                                        GAU  240
        DO 4 K=1,5                                                      GAU  250
          DO 4 L=1,N                                                    GAU  260
            X = A + FRAC *((1.-U(K))*FLOAT(J-1)+U(K)*FLOAT(2-J)+        GAU  270
     1       FLOAT(L-1))                                                GAU  280
            FX(K) = FUNC(X) + FX(K)                                     GAU  290
    4 CONTINUE                                                          GAU  300
      ANS = 0.                                                          GAU  310
      DO 5 K=1,5                                                        GAU  320
        ANS = ANS + RRR(K) * FX(K)                                      GAU  330
        FX(K) = 0.                                                      GAU  340
    5 CONTINUE                                                          GAU  350
      ANS = FRAC * ANS                                                  GAU  360
    6 RETURN                                                            GAU  370
      END                                                               GAU  380
      SUBROUTINE TITLE(IN,REASON)                                       TIT   10
C                                                                       TIT   20
      DIMENSION REASON(32)                                              TIT   30
C                                                                       TIT   40
      DATA FCP016/3H   /,FCP017/3H NO/,FCP018/3HNE /,FCP019/3H CY/,     TIT   50
     1 FCP020/3HLIN/,FCP021/3HDRI/,FCP022/3HCAL/,FCP023/3H SL/,         TIT   60
     2 FCP024/3HAB /,FCP025/3H SP/,FCP026/3HERI/,FCP027/3HCAL/,         TIT   70
     3 FCP028/3H IN/,FCP029/3HTEG/,FCP030/3HRAL/,FCP031/3H FO/,         TIT   80
     4 FCP032/3HRM /,FCP033/3H NA/,FCP034/3HRRO/,FCP035/3HW R/,         TIT   90
     5 FCP036/3HESO/,FCP037/3HNAN/,FCP038/3HCE /,FCP039/3HFOR/,         TIT  100
     6 FCP040/3HM  /,FCP041/3H IN/,FCP042/3HFIN/,FCP043/3HITE/,         TIT  110
     7 FCP044/3H MA/,FCP045/3HSS /,FCP046/3HFOR/,FCP047/3HM  /,         TIT  120
     8 FCP048/3H TE/,FCP049/3HRM /,FCP050/3HABS/,FCP051/3HENT/,         TIT  130
     9 FCP052/3H AS/,FCP053/3HYMP/,FCP054/3HTOT/,FCP055/3HIC /,         TIT  140
     X FCP056/3HFOR/,FCP057/3HM  /,FCP058/3H RE/,FCP059/3HSON/,         TIT  150
     Y FCP060/3HANC/,FCP061/3HE C/,FCP062/3HONF/,FCP063/3HIGU/,         TIT  160
     Z FCP064/3HRAT/,FCP065/3HION/,FCP066/3H IN/,FCP067/3HDEX/,         TIT  170
     Z FCP068/3H FU/,FCP069/3HLL /,FCP070/3H CO/,FCP071/3HMPU/,         TIT  180
     Z FCP072/3HTED/,FCP073/3H ME/,FCP074/3HSH /,FCP075/3HEXC/,         TIT  190
     Z FCP076/3HEED/,FCP077/3HS S/,FCP078/3HTOR/,FCP079/3HAGE/,         TIT  200
     Z FCP080/3H LI/,FCP081/3HMIT/                                      TIT  210
C                                                                       TIT  220
C                                                                       TIT  230
      DO 12 I=1,32                                                      TIT  240
        REASON(I) = (+FCP016)                                           TIT  250
   12 CONTINUE                                                          TIT  260
      GOTO(1,2,3,4,5,6,7,8,9,10,11),IN                                  TIT  270
    1 REASON(1) = (+FCP017)                                             TIT  280
      REASON(2) = (+FCP018)                                             TIT  290
      RETURN                                                            TIT  300
    2 REASON(1) = (+FCP019)                                             TIT  310
      REASON(2) = (+FCP020)                                             TIT  320
      REASON(3) = (+FCP021)                                             TIT  330
      REASON(4) = (+FCP022)                                             TIT  340
      RETURN                                                            TIT  350
    3 REASON(1) = (+FCP023)                                             TIT  360
      REASON(2) = (+FCP024)                                             TIT  370
      RETURN                                                            TIT  380
    4 REASON(1) = (+FCP025)                                             TIT  390
      REASON(2) = (+FCP026)                                             TIT  400
      REASON(3) = (+FCP027)                                             TIT  410
      RETURN                                                            TIT  420
    5 REASON(1) = (+FCP028)                                             TIT  430
      REASON(2) = (+FCP029)                                             TIT  440
      REASON(3) = (+FCP030)                                             TIT  450
      REASON(4) = (+FCP031)                                             TIT  460
      REASON(5) = (+FCP032)                                             TIT  470
      RETURN                                                            TIT  480
    6 REASON(1) = (+FCP033)                                             TIT  490
      REASON(2) = (+FCP034)                                             TIT  500
      REASON(3) = (+FCP035)                                             TIT  510
      REASON(4) = (+FCP036)                                             TIT  520
      REASON(5) = (+FCP037)                                             TIT  530
      REASON(6) = (+FCP038)                                             TIT  540
      REASON(7) = (+FCP039)                                             TIT  550
      REASON(8) = (+FCP040)                                             TIT  560
      RETURN                                                            TIT  570
    7 REASON(1) = (+FCP041)                                             TIT  580
      REASON(2) = (+FCP042)                                             TIT  590
      REASON(3) = (+FCP043)                                             TIT  600
      REASON(4) = (+FCP044)                                             TIT  610
      REASON(5) = (+FCP045)                                             TIT  620
      REASON(6) = (+FCP046)                                             TIT  630
      REASON(7) = (+FCP047)                                             TIT  640
      RETURN                                                            TIT  650
    8 REASON(1) = (+FCP048)                                             TIT  660
      REASON(2) = (+FCP049)                                             TIT  670
      REASON(3) = (+FCP050)                                             TIT  680
      REASON(4) = (+FCP051)                                             TIT  690
      RETURN                                                            TIT  700
    9 REASON(1) = (+FCP052)                                             TIT  710
      REASON(2) = (+FCP053)                                             TIT  720
      REASON(3) = (+FCP054)                                             TIT  730
      REASON(4) = (+FCP055)                                             TIT  740
      REASON(5) = (+FCP056)                                             TIT  750
      REASON(6) = (+FCP057)                                             TIT  760
      RETURN                                                            TIT  770
   10 REASON(1) = (+FCP058)                                             TIT  780
      REASON(2) = (+FCP059)                                             TIT  790
      REASON(3) = (+FCP060)                                             TIT  800
      REASON(4) = (+FCP061)                                             TIT  810
      REASON(5) = (+FCP062)                                             TIT  820
      REASON(6) = (+FCP063)                                             TIT  830
      REASON(7) = (+FCP064)                                             TIT  840
      REASON(8) = (+FCP065)                                             TIT  850
      REASON(9) = (+FCP066)                                             TIT  860
      REASON(10) = (+FCP067)                                            TIT  870
      REASON(11) = (+FCP068)                                            TIT  880
      REASON(12) = (+FCP069)                                            TIT  890
      RETURN                                                            TIT  900
   11 REASON(1) = (+FCP070)                                             TIT  910
      REASON(2) = (+FCP071)                                             TIT  920
      REASON(3) = (+FCP072)                                             TIT  930
      REASON(4) = (+FCP073)                                             TIT  940
      REASON(5) = (+FCP074)                                             TIT  950
      REASON(6) = (+FCP075)                                             TIT  960
      REASON(7) = (+FCP076)                                             TIT  970
      REASON(8) = (+FCP077)                                             TIT  980
      REASON(9) = (+FCP078)                                             TIT  990
      REASON(10) = (+FCP079)                                            TIT 1000
      REASON(11) = (+FCP080)                                            TIT 1010
      REASON(12) = (+FCP081)                                            TIT 1020
      RETURN                                                            TIT 1030
      END                                                               TIT 1040
      SUBROUTINE NEWTON(ARG,FARG,TAB,LTAB)                              NEW   10
C                                                                       NEW   20
      DIMENSION TERM(4)                                                 NEW   30
C                                                                       NEW   40
C                                                                       NEW   50
C     FOUR POINT NEWTONS FORWARD INTERPOLATION COMPUTING FARG           NEW   60
C                                                                       NEW   70
      CALL LTAB(DEL,LONG)                                               NEW   80
C                                                                       NEW   90
      RLOC = ARG / DEL                                                  NEW  100
      IN = RLOC                                                         NEW  110
      JN = IN + 1                                                       NEW  120
      IF(ARG-DEL*FLOAT(IN)) 60,40,60                                    NEW  130
   40 FARG = TAB(JN)                                                    NEW  140
      GOTO 220                                                          NEW  150
C                                                                       NEW  160
C     SET LOCATION OF FIRST TERM                                        NEW  170
C                                                                       NEW  180
   60 IF(IN) 140,140,80                                                 NEW  190
   80 IF(LONG-IN) 100,100,120                                           NEW  200
  100 JN = JN - 2                                                       NEW  210
      GOTO 140                                                          NEW  220
  120 JN = JN - 1                                                       NEW  230
C                                                                       NEW  240
C     PERFORM INTERPOLATION                                             NEW  250
C                                                                       NEW  260
  140 DO 160 IN=1,4                                                     NEW  270
        KN = JN - IN + 4                                                NEW  280
        TERM(IN) = TAB(KN)                                              NEW  290
  160 CONTINUE                                                          NEW  300
      FARG = TERM(4)                                                    NEW  310
      DO 200 IN=2,4                                                     NEW  320
        KN = 5 - IN                                                     NEW  330
        COEFT = RLOC - FLOAT(IN+JN-3)                                   NEW  340
        DO 180 LN=1,KN                                                  NEW  350
          TERM(LN) = COEFT * (TERM(LN)-TERM(LN+1))                      NEW  360
  180   CONTINUE                                                        NEW  370
        FARG = FARG + TERM(KN)                                          NEW  380
  200 CONTINUE                                                          NEW  390
  220 RETURN                                                            NEW  400
      END                                                               NEW  410
      FUNCTION SIMPS(IS,NPTS,BVAL)                                      SIM   10
C                                                                       SIM   20
      DIMENSION BVAL(1)                                                 SIM   30
C                                                                       SIM   40
C                                                                       SIM   50
C     SIMPSONS RULE                                                     SIM   60
C                                                                       SIM   70
      GOTO(1,3),IS                                                      SIM   80
C                                                                       SIM   90
C     MOVE BVAL TABLE                                                   SIM  100
C                                                                       SIM  110
    1 DO 2 JS=1,NPTS                                                    SIM  120
        KS = NPTS - JS + 1                                              SIM  130
        BVAL(KS+1) = BVAL(KS)                                           SIM  140
    2 CONTINUE                                                          SIM  150
    3 SUM = 0.                                                          SIM  160
      DO 4 JS=3,NPTS,2                                                  SIM  170
        SUM = SUM + BVAL(JS-1)                                          SIM  180
    4 CONTINUE                                                          SIM  190
      SUM = 2. * SUM                                                    SIM  200
      DO 5 JS=2,NPTS                                                    SIM  210
        SUM = SUM + BVAL(JS-1) + BVAL(JS)                               SIM  220
    5 CONTINUE                                                          SIM  230
      GOTO(6,7),IS                                                      SIM  240
    6 SUM = SUM - BVAL(1)                                               SIM  250
C                                                                       SIM  260
    7 SIMPS = SUM / 3.                                                  SIM  270
C                                                                       SIM  280
      RETURN                                                            SIM  290
      END                                                               SIM  300
      SUBROUTINE SORT(LX,IDX1,IDX2,IDX3)                                SOR   10
C                                                                       SOR   20
      DIMENSION IDX1(1),IDX2(1),IDX3(1)                                 SOR   30
C                                                                       SOR   40
C                                                                       SOR   50
C     SORTS IDX3(I) SUCH THAT ABSF(IDX1(IDX2(IDX3(I)))) INCREASES       SOR   60
C                                                                       SOR   70
      IF(LX-1) 7,7,1                                                    SOR   80
    1 ID = 0                                                            SOR   90
      DO 6 IX=2,LX                                                      SOR  100
        IX31 = IDX3(IX-1)                                               SOR  110
        IX32 = IDX3(IX)                                                 SOR  120
        IF(IDX2(1)) 2,2,3                                               SOR  130
C                                                                       SOR  140
C     TWO WAY SORT                                                      SOR  150
C                                                                       SOR  160
    2   IX21 = IX31                                                     SOR  170
        IX22 = IX32                                                     SOR  180
        GOTO 4                                                          SOR  190
C                                                                       SOR  200
C     THREE WAY SORT                                                    SOR  210
C                                                                       SOR  220
    3   IX21 = IDX2(IX31)                                               SOR  230
        IX22 = IDX2(IX32)                                               SOR  240
    4   IDX999 = IDX1(IX22)                                             SOR  250
        ID9999 = IDX1(IX21)                                             SOR  260
        IF(IABS(IDX999)-IABS(ID9999)) 5,6,6                             SOR  270
    5   IDX3(IX-1) = IX32                                               SOR  280
        IDX3(IX) = IX31                                                 SOR  290
        ID = 1                                                          SOR  300
    6 CONTINUE                                                          SOR  310
      IF(ID) 7,7,1                                                      SOR  320
    7 RETURN                                                            SOR  330
      END                                                               SOR  340
      SUBROUTINE CYLS(DEL,LONG)                                         CYL   10
C                                                                       CYL   20
C                                                                       CYL   30
      DEL = 0.02                                                        CYL   40
      LONG = 301                                                        CYL   50
      RETURN                                                            CYL   60
      END                                                               CYL   70
      FUNCTION CYL(JN)                                                  YL    10
C                                                                       YL    20
      DIMENSION CY(321),A(160),B(141)                                   YL    30
C                                                                       YL    40
      EQUIVALENCE(CY(1),A(1)),(CY(161),B(1))                            YL    50
C                                                                       YL    60
      DATA A/.00000,.02561,.04967,.07248,.09421,.11498,.13487,.15396,   YL    70
     1 .17231,.18996,.20697,.22336,.23918,.25446,.26923,.28351,.29733,  YL    80
     2 .31070,.32366,.33621,.34838,.36019,.37164,.38276,.39356,.40405,  YL    90
     3 .41424,.42414,.43377,.44314,.45225,.46112,.46975,.47816,.48634,  YL   100
     4 .49432,.50209,.50966,.51704,.52424,.53126,.53811,.54479,.55131,  YL   110
     5 .55767,.56389,.56996,.57588,.58167,.58733,.59285,.59826,.60354,  YL   120
     6 .60870,.61375,.61869,.62352,.62825,.63287,.63740,.64183,.64616,  YL   130
     7 .65041,.65457,.65864,.66263,.66654,.67038,.67413,.67781,.68142,  YL   140
     8 .68495,.68842,.69182,.69516,.69843,.70164,.70479,.70788,.71091,  YL   150
     9 .71389,.71681,.71968,.72250,.72526,.72798,.73065,.73327,.73585,  YL   160
     X .73838,.74087,.74331,.74572,.74808,.75040,.75269,.75493,.75714,  YL   170
     Y .75931,.76145,.76355,.76562,.76765,.76966,.77163,.77357,.77548,  YL   180
     Z .77736,.77921,.78103,.78282,.78459,.78633,.78805,.78974,.79140,  YL   190
     Z .79304,.79465,.79624,.79781,.79936,.80088,.80239,.80387,.80532,  YL   200
     Z .80677,.80818,.80958,.81096,.81233,.81367,.81500,.81630,.81759,  YL   210
     Z .81886,.82012,.82136,.82258,.82379,.82498,.82616,.82732,.82847,  YL   220
     Z .82960,.83071,.83182,.83291,.83399,.83505,.83610,.83714,.83816,  YL   230
     Z .83917,.84017,.84116,.84214,.84310,.84405,.84500,.84593/         YL   240
C                                                                       YL   250
      DATA B/.84685,.84776,.84866,.84955,.85043,.85130,.85216,.85301,   YL   260
     1 .85385,.85468,.85550,.85632,.85712,.85792,.85870,.85948,.86025,  YL   270
     2 .86101,.86177,.86251,.86325,.86398,.86471,.86542,.86613,.86683,  YL   280
     3 .86752,.86821,.86889,.86956,.87023,.87089,.87154,.87219,.87283,  YL   290
     4 .87346,.87409,.87471,.87533,.87594,.87654,.87714,.87773,.87832,  YL   300
     5 .87890,.87947,.88004,.88061,.88117,.88173,.88228,.88282,.88336,  YL   310
     6 .88389,.88443,.88495,.88547,.88598,.88650,.88701,.88751,.88801,  YL   320
     7 .88850,.88899,.88948,.88996,.89044,.89091,.89138,.89184,.89230,  YL   330
     8 .89276,.89321,.89366,.89411,.89455,.89499,.89542,.89585,.89628,  YL   340
     9 .89671,.89713,.89755,.89796,.89837,.89878,.89919,.89959,.89999,  YL   350
     X .90038,.90077,.90116,.90155,.90193,.90231,.90268,.90306,.90343,  YL   360
     Y .90381,.90417,.90453,.90489,.90525,.90560,.90595,.90631,.90665,  YL   370
     Z .90700,.90734,.90768,.90802,.90835,.90868,.90902,.90934,.90967,  YL   380
     Z .90999,.91031,.91063,.91095,.91126,.91157,.91188,.91219,.91250,  YL   390
     Z .91280,.91310,.91340,.91369,.91399,.91429,.91457,.91487,.91515,  YL   400
     Z .91544,.91572,.91600,.91628,.91656,.91684,.91711/                YL   410
C                                                                       YL   420
C                                                                       YL   430
      CYL = CY(JN)                                                      YL   440
C                                                                       YL   450
      RETURN                                                            YL   460
      END                                                               YL   470
      SUBROUTINE SLBS(DEL,LONG)                                         SLB   10
C                                                                       SLB   20
C                                                                       SLB   30
      DEL = 0.01                                                        SLB   40
      LONG = 501                                                        SLB   50
      RETURN                                                            SLB   60
      END                                                               SLB   70
      FUNCTION SLB(JN)                                                  LB    10
C                                                                       LB    20
      DIMENSION SL(521),A(160),B(160),C(160),D(21)                      LB    30
C                                                                       LB    40
      EQUIVALENCE(SL(1),A(1)),(SL(161),B(1)),(SL(321),C(1)),            LB    50
     1 (SL(481),D(1))                                                   LB    60
C                                                                       LB    70
      DATA A/.0000,.0484,.0831,.1127,.1390,.1629,.1849,.2054,.2246,.2427LB    80
     1 ,.2597,.2759,.2913,.3060,.3200,.3335,.3464,.3588,.3707,.3821,    LB    90
     2 .3932,.4039,.4142,.4242,.4339,.4432,.4523,.4611,.4696,.4779,.4859LB   100
     3 ,.4937,.5013,.5087,.5159,.5229,.5298,.5364,.5429,.5492,.5554,    LB   110
     4 .5614,.5673,.5730,.5786,.5841,.5895,.5947,.5998,.6048,.6097,.6145LB   120
     5 ,.6192,.6237,.6282,.6326,.6369,.6411,.6453,.6493,.6533,.6572,    LB   130
     6 .6610,.6647,.6684,.6720,.6755,.6790,.6824,.6857,.6890,.6922,.6954LB   140
     7 ,.6985,.7015,.7045,.7074,.7103,.7132,.7159,.7187,.7214,.7240,    LB   150
     8 .7266,.7292,.7317,.7342,.7366,.7390,.7414,.7437,.7460,.7483,.7505LB   160
     9 ,.7527,.7548,.7569,.7590,.7611,.7631,.7651,.7670,.7690,.7709,    LB   170
     X .7727,.7746,.7764,.7782,.7800,.7817,.7834,.7851,.7868,.7884,.7901LB   180
     Y ,.7917,.7932,.7948,.7963,.7978,.7993,.8008,.8023,.8037,.8051,    LB   190
     Z .8065,.8079,.8093,.8106,.8119,.8132,.8145,.8158,.8171,.8183,.8196LB   200
     Z ,.8208,.8220,.8232,.8243,.8255,.8266,.8278,.8289,.8300,.8311,    LB   210
     Z .8321,.8332,.8342,.8353,.8363,.8373,.8383,.8393,.8403,.8413,.8422LB   220
     Z ,.8432,.8441,.8450/                                              LB   230
C                                                                       LB   240
      DATA B/.8460,.8469,.8478,.8486,.8495,.8504,.8512,.8521,.8529,.8538LB   250
     1 ,.8546,.8554,.8562,.8570,.8578,.8586,.8593,.8601,.8608,.8616,    LB   260
     2 .8623,.8631,.8638,.8645,.8652,.8659,.8666,.8673,.8680,.8687,.8693LB   270
     3 ,.8700,.8707,.8713,.8720,.8726,.8732,.8738,.8745,.8751,.8757,    LB   280
     4 .8763,.8769,.8775,.8781,.8786,.8792,.8798,.8804,.8809,.8815,.8820LB   290
     5 ,.8826,.8831,.8836,.8842,.8847,.8852,.8857,.8863,.8868,.8873,    LB   300
     6 .8878,.8883,.8887,.8892,.8897,.8902,.8907,.8911,.8916,.8921,.8925LB   310
     7 ,.8930,.8934,.8939,.8943,.8948,.8952,.8956,.8961,.8965,.8969,    LB   320
     8 .8973,.8977,.8982,.8986,.8990,.8994,.8998,.9002,.9006,.9010,.9013LB   330
     9 ,.9017,.9021,.9025,.9029,.9032,.9036,.9040,.9043,.9047,.9051,    LB   340
     X .9054,.9058,.9061,.9065,.9068,.9072,.9075,.9078,.9082,.9085,.9089LB   350
     Y ,.9092,.9095,.9098,.9102,.9105,.9108,.9111,.9114,.9117,.9120,    LB   360
     Z .9124,.9127,.9130,.9133,.9136,.9139,.9141,.9144,.9147,.9150,.9153LB   370
     Z ,.9156,.9159,.9162,.9164,.9167,.9170,.9173,.9175,.9178,.9181,    LB   380
     Z .9183,.9186,.9189,.9191,.9194,.9196,.9199,.9202,.9204,.9207,.9209LB   390
     Z ,.9212,.9214,.9217/                                              LB   400
C                                                                       LB   410
      DATA C/.9219,.9221,.9224,.9226,.9229,.9231,.9233,.9236,.9238,.9240LB   420
     1 ,.9243,.9245,.9247,.9249,.9252,.9254,.9256,.9258,.9261,.9263,    LB   430
     2 .9265,.9267,.9269,.9271,.9273,.9276,.9278,.9280,.9282,.9284,.9286LB   440
     3 ,.9288,.9290,.9292,.9294,.9296,.9298,.9300,.9302,.9304,.9306,    LB   450
     4 .9308,.9309,.9311,.9313,.9315,.9317,.9319,.9321,.9323,.9324,.9326LB   460
     5 ,.9328,.9330,.9332,.9333,.9335,.9337,.9339,.9340,.9342,.9344,    LB   470
     6 .9346,.9347,.9349,.9351,.9352,.9354,.9356,.9357,.9359,.9361,.9362LB   480
     7 ,.9364,.9366,.9367,.9369,.9370,.9372,.9373,.9375,.9377,.9378,    LB   490
     8 .9380,.9381,.9383,.9384,.9386,.9387,.9389,.9390,.9392,.9393,.9395LB   500
     9 ,.9396,.9398,.9399,.9401,.9402,.9403,.9405,.9406,.9408,.9409,    LB   510
     X .9410,.9412,.9413,.9415,.9416,.9417,.9419,.9420,.9421,.9423,.9424LB   520
     Y ,.9425,.9427,.9428,.9429,.9431,.9432,.9433,.9434,.9436,.9437,    LB   530
     Z .9438,.9439,.9441,.9442,.9443,.9444,.9446,.9447,.9448,.9449,.9451LB   540
     Z ,.9452,.9453,.9454,.9455,.9457,.9458,.9459,.9460,.9461,.9462,    LB   550
     Z .9464,.9465,.9466,.9467,.9468,.9469,.9470,.9471,.9473,.9474,.9475LB   560
     Z ,.9476,.9477,.9478/                                              LB   570
C                                                                       LB   580
      DATA D/.9479,.9480,.9481,.9482,.9483,.9485,.9486,.9487,.9488,.9489LB   590
     1 ,.9490,.9491,.9492,.9493,.9494,.9495,.9496,.9497,.9498,.9499,    LB   600
     2 .9500/                                                           LB   610
C                                                                       LB   620
C                                                                       LB   630
      SLB = SL(JN)                                                      LB   640
C                                                                       LB   650
      RETURN                                                            LB   660
      END                                                               LB   670
      SUBROUTINE PAGE(IP,JP,KP)                                         PAG   10
C                                                                       PAG   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      PAG   30
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),          PAG   40
     2 INDEX(1000,4)                                                    PAG   50
C                                                                       PAG   60
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          PAG   70
C                                                                       PAG   80
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        PAG   90
C                                                                       PAG  100
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            PAG  110
C                                                                       PAG  120
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          PAG  130
C                                                                       PAG  140
      COMMON NOUT,NOXT                                                  PAG  150
C                                                                       PAG  160
      COMMON /UI/ IDUM(7),IPAGE                                         PAG  170
C                                                                       PAG  180
C                                                                       PAG  190
C     TITLES AND NUMBERS PRINTED PAGES                                  PAG  200
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        PAG  210
C     BEI MEHREREN FAELLEN HINTEREINANDER                               PAG  220
C                                                                       PAG  230
C                                                                       PAG  240
      IF(KP .NE. 9999) GOTO 10                                          PAG  250
      IPAGE = 0                                                         PAG  260
      IP = 0                                                            PAG  270
      JP = 0                                                            PAG  280
      RETURN                                                            PAG  290
   10 CONTINUE                                                          PAG  300
      IF(JP) 3,3,1                                                      PAG  310
    1 IF(LINE+JP-52) 2,2,3                                              PAG  320
    2 IP = 2                                                            PAG  330
      LINE = LINE + JP                                                  PAG  340
      GOTO 6                                                            PAG  350
    3 IP = 1                                                            PAG  360
      LINE = KP                                                         PAG  370
      IPAGE = IPAGE + 1                                                 PAG  380
      WRITE (NOUT,7) IPAGE                                              PAG  390
      IF(JP) 4,5,6                                                      PAG  400
    4 WRITE (NOUT,8)                                                    PAG  410
    5 WRITE (NOUT,9)                                                    PAG  420
    6 RETURN                                                            PAG  430
C                                                                       PAG  440
    7 FORMAT (1H1/1H0,110X,5HZ U T/110X,5HPAGE ,I3)                     PAG  450
    8 FORMAT (1H0/1H0/1H0/1H0/1H0/1H0/1H0)                              PAG  460
    9 FORMAT (33H RESONANCE ABSORPTION CALCULATION)                     PAG  470
      END                                                               PAG  480
      FUNCTION SHAPE(Y)                                                 SHA   10
C                                                                       SHA   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      SHA   30
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),AZERO(500)SHA   40
     2 ,G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),GMGM(500),     SHA   50
     3 R(500),S(500),SOLVE(50),ABAR(50),C(50),DZERO(50),AMOD1(50),      SHA   60
     4 SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),AMU(505),CHI(505),        SHA   70
     5 DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),GAMMA(500),PSI(505),SHA   80
     6 RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),SUB(501),TERM(3),X(505)SHA   90
     7 ,XI(500),INDEX(1000,4),BVAL(1001,3)                              SHA  100
C                                                                       SHA  110
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          SHA  120
C                                                                       SHA  130
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        SHA  140
C                                                                       SHA  150
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            SHA  160
C                                                                       SHA  170
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          SHA  180
C                                                                       SHA  190
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           SHA  200
C                                                                       SHA  210
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           SHA  220
C                                                                       SHA  230
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       SHA  240
C                                                                       SHA  250
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        SHA  260
C                                                                       SHA  270
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            SHA  280
C                                                                       SHA  290
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     SHA  300
C                                                                       SHA  310
C                                                                       SHA  320
C     COMPUTES INTEGRAND OF SHAPE FUNCTION PSI(X,XI)                    SHA  330
C     ERSTE COMMON- UND DIMENSIONKARTE NUR ZUR SPEICHERLOESCHUNG        SHA  340
C     BEI MEHREREN FAELLEN HINTEREINANDER                               SHA  350
C                                                                       SHA  360
C                                                                       SHA  370
      SHAPE = EXP(-(XI(I)*(X(JC)-SIN(Y)/COS(Y))/2.)**2)                 SHA  380
C                                                                       SHA  390
      RETURN                                                            SHA  400
      END                                                               SHA  410
      SUBROUTINE STUEZP                                                 STU   10
C                                                                       STU   20
      DIMENSION FIELD(10),S(200),PFAK(200)                              STU   30
C                                                                       STU   40
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          STU   50
C                                                                       STU   60
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     STU   70
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   STU   80
     2 ESIG2,EDIQU2,KI,IEING5                                           STU   90
C                                                                       STU  100
      EQUIVALENCE(S(1),XTAB(1)),(PFAK(1),FTAB(1))                       STU  110
C                                                                       STU  120
C                                                                       STU  130
      DELS = 4. / EW(13)                                                STU  140
      S(1) = ALOG(EW(16)*EW(11)+EW(12))                                 STU  150
      SFAK = 2.                                                         STU  160
      DO 8 NU=1,200                                                     STU  170
        IF(NU-1) 3,3,2                                                  STU  180
    2   S(NU) = S(NU-1) + DELS * SFAK                                   STU  190
    3   SNU = S(NU)                                                     STU  200
C                                                                       STU  210
        CALL DOPP1(SNU,PNU,DNE,SMK)                                     STU  220
C                                                                       STU  230
        IF(EW(2) .LE. 0. .OR. NU .GT. 1) GOTO 10                        STU  240
        EDZERO = DNE                                                    STU  250
        IF(EAMOD1 .NE. 12.) GOTO 11                                     STU  260
        IF(ESIG1 .EQ. 0.) ESIGM1 = ESIGM1 + SMK                         STU  270
        IF(ESIG1 .GT. 0.) EDIQU1 = EDIQU1 + SMK / ESIG1                 STU  280
   11   CONTINUE                                                        STU  290
        IF(EAMOD2 .NE. 12.) GOTO 12                                     STU  300
        IF(ESIG2 .EQ. 0.) ESIGM2 = ESIGM2 + SMK                         STU  310
        IF(ESIG2 .GT. 0.) EDIQU2 = EDIQU2 + SMK / ESIG2                 STU  320
   12   CONTINUE                                                        STU  330
   10   CONTINUE                                                        STU  340
        PFAK(NU) = PNU                                                  STU  350
        IF(ABS(800.-EW(19))-700.) 4,4,5                                 STU  360
    4   SFAK = 1.                                                       STU  370
        GOTO 6                                                          STU  380
    5   SFAK = 2.                                                       STU  390
    6   EW(20) = FLOAT(NU)                                              STU  400
        IF(EW(19) .LE. 80000.) GOTO 8                                   STU  410
        GOTO 9                                                          STU  420
    8 CONTINUE                                                          STU  430
    9 CONTINUE                                                          STU  440
      ESOLVE = 30000.                                                   STU  450
      IF(EW(4) .EQ. 0.) ESOLVE = 10000.                                 STU  460
      ECDANC = EW(14)                                                   STU  470
      EABAR = EW(3)                                                     STU  480
      RETURN                                                            STU  490
      END                                                               STU  500
      SUBROUTINE PINT(ARG,PNUL)                                         PIN   10
C                                                                       PIN   20
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          PIN   30
C                                                                       PIN   40
C                                                                       PIN   50
      N = IFIX(EW(20))                                                  PIN   60
      SI1 = ARG / EW(17)                                                PIN   70
      X = ALOG(SI1)                                                     PIN   80
C                                                                       PIN   90
      CALL LAGR(X,FX,N,KERR)                                            PIN  100
C                                                                       PIN  110
      PNUL = FX                                                         PIN  120
      RETURN                                                            PIN  130
      END                                                               PIN  140
      SUBROUTINE LAGR(X,FX,N,KERR)                                      LAG   10
C                                                                       LAG   20
      REAL*8 A1,A2,A3,A,B,C,D,B1,B2,B3,C1,C2,C3,D1,D2,D3,FX1,FX2,FX3,FX4LAG   30
C                                                                       LAG   40
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          LAG   50
C                                                                       LAG   60
      COMMON /UI/ IDUM(8),I                                             LAG   70
C                                                                       LAG   80
C                                                                       LAG   90
C     LAGRANGE (INTERPOLATIONSPROGRAMM)                                 LAG  100
C                                                                       LAG  110
      KERR = 0                                                          LAG  120
      IF(XTAB(N)-X) 80,5,5                                              LAG  130
    5 CONTINUE                                                          LAG  140
      DO 10 II=1,N                                                      LAG  150
        I = II                                                          LAG  160
        IF(XTAB(I)-X) 10,20,30                                          LAG  170
   10 CONTINUE                                                          LAG  180
   20 FX = FTAB(I)                                                      LAG  190
      RETURN                                                            LAG  200
   30 I = I - 1                                                         LAG  210
      IF(I-1) 90,40,50                                                  LAG  220
   40 I = I + 1                                                         LAG  230
      GOTO 70                                                           LAG  240
   50 IF(N-I-2) 60,70,70                                                LAG  250
   60 I = I - 1                                                         LAG  260
C                                                                       LAG  270
C     FAKTOREN FUER DEN ZAEHLER                                         LAG  280
C                                                                       LAG  290
   70 A = X - XTAB(I-1)                                                 LAG  300
      B = X - XTAB(I)                                                   LAG  310
      C = X - XTAB(I+1)                                                 LAG  320
      D = X - XTAB(I+2)                                                 LAG  330
C                                                                       LAG  340
C     FAKTOREN FUER DEN NENNER                                          LAG  350
C                                                                       LAG  360
      A1 = XTAB(I-1) - XTAB(I)                                          LAG  370
      A2 = XTAB(I-1) - XTAB(I+1)                                        LAG  380
      A3 = XTAB(I-1) - XTAB(I+2)                                        LAG  390
      B1 = -A1                                                          LAG  400
      B2 = XTAB(I) - XTAB(I+1)                                          LAG  410
      B3 = XTAB(I) - XTAB(I+2)                                          LAG  420
      C1 = -A2                                                          LAG  430
      C2 = -B2                                                          LAG  440
      C3 = XTAB(I+1) - XTAB(I+2)                                        LAG  450
      D1 = -A3                                                          LAG  460
      D2 = -B3                                                          LAG  470
      D3 = -C3                                                          LAG  480
      FX1 = (B*C*D*FTAB(I-1)) / (A1*A2*A3)                              LAG  490
      FX2 = (A*C*D*FTAB(I)) / (B1*B2*B3)                                LAG  500
      FX3 = (A*B*D*FTAB(I+1)) / (C1*C2*C3)                              LAG  510
      FX4 = (A*B*C*FTAB(I+2)) / (D1*D2*D3)                              LAG  520
      FX = FX1 + FX2 + FX3 + FX4                                        LAG  530
      RETURN                                                            LAG  540
C                                                                       LAG  550
C     LINEARE EXTRAPOLATION UND FEHLERAUSGANG KERR = 1                  LAG  560
C                                                                       LAG  570
   80 I = N                                                             LAG  580
      KERR = 1                                                          LAG  590
      FX = FTAB(I-1) + (FTAB(I)-FTAB(I-1)) / (XTAB(I)-XTAB(I-1)) * (X-  LAG  600
     1 XTAB(I-1))                                                       LAG  610
      RETURN                                                            LAG  620
   90 FX = (FTAB(2)*(XTAB(1)-X)-(FTAB(1)*(XTAB(2)-X))) / (XTAB(1)-      LAG  630
     1 XTAB(2))                                                         LAG  640
      KERR = 1                                                          LAG  650
      RETURN                                                            LAG  660
      END                                                               LAG  670
      SUBROUTINE WATCH(ENDE)                                            WAT   10
C                                                                       WAT   20
C     DEFINITION OF TIME                                                WAT   30
C                                                                       WAT   40
      COMMON /HOUMIN/ IH,IM                                             WAT   50
C                                                                       WAT   60
      CHARACTER*2 TIMF(5)                                               WAT   70
C                                                                       WAT   80
      CHARACTER*6 SC                                                    WAT   90
C                                                                       WAT  100
      CHARACTER*8 DATF                                                  WAT  110
C                                                                       WAT  120
      CHARACTER*10 TIMT                                                 WAT  130
C                                                                       WAT  140
      EQUIVALENCE(TIMF(1),TIMT),(TIMF(3),SC)                            WAT  150
C                                                                       WAT  160
C                                                                       WAT  170
      CALL DATE_AND_TIME(DATF,TIMT)                                     WAT  180
C                                                                       WAT  190
      READ (UNIT=TIMF(1),FMT=100) IH                                    WAT  200
      READ (UNIT=TIMF(2),FMT=100) IM                                    WAT  210
      READ (UNIT=SC,FMT=101) SEC                                        WAT  220
      ENDE = FLOAT(IH) * 3600. + FLOAT(IM) * 60. + SEC                  WAT  230
      RETURN                                                            WAT  240
C                                                                       WAT  250
  100 FORMAT (I2)                                                       WAT  260
  101 FORMAT (F6.3)                                                     WAT  270
      END                                                               WAT  280
      SUBROUTINE TERMINAT                                               TER   10
C                                                                       TER   20
      COMMON /CL/ A                                                     TER   30
C                                                                       TER   40
C                                                                       TER   50
      CALL WATCH(ENDE)                                                  TER   60
C                                                                       TER   70
      CPU = ENDE - A                                                    TER   80
      WRITE (6,100) CPU/60.                                             TER   90
      STOP                                                              TER  100
C                                                                       TER  110
  100 FORMAT (///' "VSOP-ZUT": EXECUTION-TIME:',F10.3,' MIN.')          TER  120
      END                                                               TER  130