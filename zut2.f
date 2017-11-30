      SUBROUTINE NUMINT(UND,D,CNUMI)                                    NUM   10
C                                                                       NUM   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         NUM   30
C                                                                       NUM   40
      DIMENSION D(12),V(6),Y(5),X(6),IK(4)                              NUM   50
C                                                                       NUM   60
      DATA KFAOP$/6/                                                    NUM   70
C                                                                       NUM   80
C                                                                       NUM   90
C     NUMERISCHE INTEGRATION                                            NUM  100
C                                                                       NUM  110
      IND = IFIX(SNGL(UND))                                             NUM  120
      CNUMI = 0.D0                                                      NUM  130
      ALF = D(6)                                                        NUM  140
      L = 1                                                             NUM  150
      X(6) = 1.                                                         NUM  160
      X(1) = 0.                                                         NUM  170
      FSIMP = 0.                                                        NUM  180
      D1ALF = DMAX1(D(1)*4.D-6,1.0D-10)                                 NUM  190
      EPSI1 = DMIN1(D1ALF,1.0D-7)                                       NUM  200
      EPSI2 = EPSI1 * 1.0D-2                                            NUM  210
      SCHRIT = 0.25                                                     NUM  220
      DX1 = 0.                                                          NUM  230
      DO 2 I=1,4                                                        NUM  240
        IK(I) = 0                                                       NUM  250
    2 CONTINUE                                                          NUM  260
    1 CONTINUE                                                          NUM  270
      FPROV = 0.                                                        NUM  280
C                                                                       NUM  290
      CALL SIMP2(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIM,     NUM  300
     1 FPROV)                                                           NUM  310
C                                                                       NUM  320
      GOTO(10,3),L                                                      NUM  330
    3 DO 4 I=1,5                                                        NUM  340
        V(I) = 0.D0                                                     NUM  350
        V(I) = X(I)                                                     NUM  360
    4 CONTINUE                                                          NUM  370
C                                                                       NUM  380
C     BERECHNUNG DES INTEGRANDEN                                        NUM  390
C                                                                       NUM  400
      DO 9 I=IA,5,IS                                                    NUM  410
        D(6) = DSQRT(1.D0-V(I))                                         NUM  420
        D(7) = DSQRT(D(4)-V(I))                                         NUM  430
        DEX = D(1) * (ALF*D(7)-D(6))                                    NUM  440
        IF(DEX-130.) 31,31,32                                           NUM  450
   31   D(8) = DEXP(-DEX)                                               NUM  460
        GOTO 33                                                         NUM  470
   32   D(8) = 0.D0                                                     NUM  480
   33   CONTINUE                                                        NUM  490
        DEY = 2.D0 * D(2) * D(6)                                        NUM  500
        IF(DEY-130.) 21,21,22                                           NUM  510
   21   D(9) = D(8) / DEXP(DEY)                                         NUM  520
        GOTO 23                                                         NUM  530
   22   D(9) = 0.D0                                                     NUM  540
   23   CONTINUE                                                        NUM  550
        GOTO(5,6,13,13),IND                                             NUM  560
    5   D(10) = D(8) * D(9)                                             NUM  570
        GOTO 8                                                          NUM  580
   13   CONTINUE                                                        NUM  590
        IF(D(11) .LT. DEX+DEY) GOTO 14                                  NUM  600
        D(8) = 2.D0 * D(12) * D(1) * D(6)                               NUM  610
        D(9) = 0.D0                                                     NUM  620
        GOTO 6                                                          NUM  630
   14   CONTINUE                                                        NUM  640
        IF(D(11) .GT. DEX) D(8) = (1.D0+D(11)-DEX) * D(12)              NUM  650
    6   D(10) = D(8) - D(9)                                             NUM  660
        GOTO(8,8,7,8),IND                                               NUM  670
    7   D(10) = D(10) / DEXP(D(3)*(DSQRT(D(5)-V(I))-D(7)))              NUM  680
    8   Y(I) = D(10)                                                    NUM  690
    9 CONTINUE                                                          NUM  700
      IK(4) = IK(4) + 1                                                 NUM  710
      IF(IK(4)-600) 1,1,11                                              NUM  720
   10 CNUMI = FSIMP                                                     NUM  730
      RETURN                                                            NUM  740
   11 WRITE (KFAOP$,12)                                                 NUM  750
C                                                                       NUM  760
   12 FORMAT (' BEYOND 600 SIMP-INTEGR-ITERATIONS FOR ONE INTEGRAL')    NUM  770
      RETURN                                                            NUM  780
      END                                                               NUM  790
      SUBROUTINE ANINTF(CA)                                             ANI   10
C                                                                       ANI   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         ANI   30
C                                                                       ANI   40
      DIMENSION CA(10)                                                  ANI   50
C                                                                       ANI   60
C                                                                       ANI   70
C     ANALYTISCH INTEGRIERTE FUNKTION                                   ANI   80
C                                                                       ANI   90
      C = 2. * CA(1)                                                    ANI  100
      CQ = C**2                                                         ANI  110
      IF(CA(2) .EQ. 0.) GOTO 1                                          ANI  120
      CQ = CQ * (1.-CA(2))                                              ANI  130
      C = DSQRT(CQ)                                                     ANI  140
    1 CONTINUE                                                          ANI  150
      IF(C .LT. 0.004) GOTO 2                                           ANI  160
      IF(C .GT. 130.) C = -1.                                           ANI  170
      CA(4) = (1.-(1.+C)/DEXP(C)) / (CA(1)**2*2.)                       ANI  180
      CA(5) = 1. - CA(4)                                                ANI  190
      GOTO 3                                                            ANI  200
    2 CONTINUE                                                          ANI  210
      CA(5) = CA(2) + (1.-CA(2)) * (C/6.-CQ/16.+C*CQ/60.) * 4.          ANI  220
      CA(4) = 1. - CA(5)                                                ANI  230
    3 CONTINUE                                                          ANI  240
      RETURN                                                            ANI  250
      END                                                               ANI  260
      SUBROUTINE SIMP2(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIMIMP   10
     1 ,FPROV)                                                          IMP   20
C                                                                       IMP   30
      IMPLICIT REAL*8 (A-H,O-Z)                                         IMP   40
C                                                                       IMP   50
      COMMON /UI/ IDUM(9)                                               IMP   60
C                                                                       IMP   70
      COMMON /U8/ VFAKT                                                 IMP   80
C                                                                       IMP   90
      DIMENSION X(6),Y(5),IK(4)                                         IMP  100
C                                                                       IMP  110
   31 FORMAT (' INTEGRATION STEP WIDTH DX=',D12.6,', EPSI1=',D12.6,' WILIMP  120
     1L BE ENLARGED BY FACTOR 10')                                      IMP  130
C                                                                       IMP  140
C     SIMPSON INTEGRATION                                               IMP  150
C     X = STUETZPUNKT                                                   IMP  160
C     Y = ZUGEHOERIGER FUNKTIONSWERT                                    IMP  170
C     SCHRIT = SCHRITTWEITE                                             IMP  180
C     EPSI1 = MAX. RELATIVER FEHLER, EPSI2 = MIN. RELATIVER FEHLER      IMP  190
C     FSIMP = INTEGRALWERT MIT SCHRITTWEITE DX/2                        IMP  200
C     DIE SHRITTWEITE DES ERSTEN SCHRITTES WIRD GESPEICHERT AUF         IMP  210
C     S C H R I T  UND STEHT BEI DER EVTL. NAECHSTEN KURVE MIT GLEICHEM IMP  220
C     STEIGUNGSVERLAUF WIEDER ZUR VERFUEGUNG.                           IMP  230
C                                                                       IMP  240
C                                                                       IMP  250
      DX = DX1                                                          IMP  260
      MM = IK(1)                                                        IMP  270
      KSI = IK(2)                                                       IMP  280
      LAUF = IK(3)                                                      IMP  290
      K = KSI                                                           IMP  300
      GOTO(111,2),L                                                     IMP  310
  111 L = 2                                                             IMP  320
      K = 1                                                             IMP  330
      LAUF = 1                                                          IMP  340
      FSIMP = 0.                                                        IMP  350
      F1SIM = 0.                                                        IMP  360
      DX = SCHRIT                                                       IMP  370
      MM = 1                                                            IMP  380
      VFAKT = 1.                                                        IMP  390
   14 GOTO(114,214),MM                                                  IMP  400
  214 MM = 1                                                            IMP  410
      EPSI1 = EPSI1 / VFAKT                                             IMP  420
      EPSI2 = EPSI2 / VFAKT                                             IMP  430
      VFAKT = 1.                                                        IMP  440
  114 IA = 1                                                            IMP  450
      IS = 1                                                            IMP  460
      DO 5 I=1,2                                                        IMP  470
        X(I+1) = X(I) + DX                                              IMP  480
        X(I+3) = X(I) + DX / 2.                                         IMP  490
    5 CONTINUE                                                          IMP  500
   88 KSI = K                                                           IMP  510
      DX1 = DX                                                          IMP  520
      IK(1) = MM                                                        IMP  530
      IK(2) = KSI                                                       IMP  540
      IK(3) = LAUF                                                      IMP  550
      IK(4) = IK(4) + 1                                                 IMP  560
      RETURN                                                            IMP  570
    2 D1 = DX / 3.                                                      IMP  580
      D2 = D1 / 2.                                                      IMP  590
      A1 = Y(1) + 4. * Y(2) + Y(3)                                      IMP  600
      A2 = A1 - Y(2) - Y(2) + 4. * (Y(4)+Y(5))                          IMP  610
      F1 = D1 * A1                                                      IMP  620
      F2 = D2 * A2                                                      IMP  630
      ABW = 0.                                                          IMP  640
      IF(F1 .EQ. F2) GOTO 30                                            IMP  650
      F12 = DMAX1(DABS(F1),DABS(F2),DABS(F2+FSIMP),1.0D-7,DABS(FPROV*DX)IMP  660
     1 )                                                                IMP  670
      ABW = DABS((F1-F2)/F12)                                           IMP  680
   30 CONTINUE                                                          IMP  690
      IF(ABW-EPSI1) 10,11,11                                            IMP  700
   11 DX = DX / 2.                                                      IMP  710
      IF(DABS(DX)/2. .GT. 1.0D-11) GOTO 113                             IMP  720
      DX = DX * 2.                                                      IMP  730
      WRITE (6,31) DX,EPSI1                                             IMP  740
      EPSI1 = EPSI1 * 10.                                               IMP  750
      EPSI2 = EPSI2 * 10.                                               IMP  760
      MM = 2                                                            IMP  770
      VFAKT = VFAKT * 10.                                               IMP  780
      GOTO 30                                                           IMP  790
  113 K = 2                                                             IMP  800
      X(3) = X(2)                                                       IMP  810
      Y(3) = Y(2)                                                       IMP  820
      X(2) = X(4)                                                       IMP  830
      Y(2) = Y(4)                                                       IMP  840
      X(4) = X(1) + DX / 2.                                             IMP  850
      X(5) = X(2) + DX / 2.                                             IMP  860
      IA = 4                                                            IMP  870
      IS = 1                                                            IMP  880
      GOTO 88                                                           IMP  890
   10 IF(ABW-EPSI2) 12,12,13                                            IMP  900
   12 GOTO(1,13),K                                                      IMP  910
    1 DX = 2. * DX                                                      IMP  920
      IF((X(6)-X(3))/(DX*(1.+1.0D-14)) .LE. 1.0)  GOTO 9                IMP  930
      X(4) = X(2)                                                       IMP  940
      Y(4) = Y(2)                                                       IMP  950
      X(2) = X(3)                                                       IMP  960
      Y(2) = Y(3)                                                       IMP  970
      X(3) = X(2) + DX                                                  IMP  980
      X(5) = X(2) + DX / 2.                                             IMP  990
      IA = 3                                                            IMP 1000
      IS = 2                                                            IMP 1010
      GOTO 88                                                           IMP 1020
    9 K = 2                                                             IMP 1030
      DX = (X(6)-X(1)) / 2.                                             IMP 1040
      GOTO 14                                                           IMP 1050
   13 K = 1                                                             IMP 1060
      GOTO(130,230),LAUF                                                IMP 1070
  130 LAUF = 2                                                          IMP 1080
      SCHRIT = DX                                                       IMP 1090
  230 FSIMP = FSIMP + F2                                                IMP 1100
      F1SIM = F1SIM + F1                                                IMP 1110
      X(1) = X(3)                                                       IMP 1120
      R = X(6) - X(1)                                                   IMP 1130
      IF(R/(2.0*DX) .GE. 1.0) GOTO 14                                   IMP 1140
      EINS = 1.                                                         IMP 1150
      IF(R*DSIGN(EINS,DX) .LT. 1.0D-14) GOTO 17                         IMP 1160
      DX = R / 2.                                                       IMP 1170
      GOTO 14                                                           IMP 1180
   17 L = 1                                                             IMP 1190
      GOTO 88                                                           IMP 1200
      END                                                               IMP 1210
      SUBROUTINE DOPP1(SNU1,PNU1,DNE,SMI)                               DOP   10
C                                                                       DOP   20
      DIMENSION EW1(20)                                                 DOP   30
C                                                                       DOP   40
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          DOP   50
C                                                                       DOP   60
      EQUIVALENCE(EW(1),EW1(1))                                         DOP   70
C                                                                       DOP   80
      DOUBLE PRECISION EW2(20),SNU2,PNU2,DNEINF,SMIK2E                  DOP   90
C                                                                       DOP  100
C                                                                       DOP  110
      DO 1 I=1,20                                                       DOP  120
        EW2(I) = DBLE(EW1(I))                                           DOP  130
    1 CONTINUE                                                          DOP  140
      SNU2 = DBLE(SNU1)                                                 DOP  150
      PNU2 = DBLE(PNU1)                                                 DOP  160
C                                                                       DOP  170
      CALL DOPP2(EW2,SNU2,PNU2,DNEINF,SMIK2E)                           DOP  180
C                                                                       DOP  190
      DO 2 I=1,20                                                       DOP  200
        EW1(I) = SNGL(EW2(I))                                           DOP  210
    2 CONTINUE                                                          DOP  220
      SNU1 = SNGL(SNU2)                                                 DOP  230
      PNU1 = SNGL(PNU2)                                                 DOP  240
      DNE = SNGL(DNEINF)                                                DOP  250
      SMI = SNGL(SMIK2E)                                                DOP  260
      RETURN                                                            DOP  270
      END                                                               DOP  280
      SUBROUTINE DOPP2(EW,SNU,PNU,DNEINF,SMIK2E)                        OPP   10
C                                                                       OPP   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         OPP   30
C                                                                       OPP   40
      COMMON /UI/ IDUM(9)                                               OPP   50
C                                                                       OPP   60
      COMMON /U8/ DUM,A1,A2,A4,A5,SI3,A8C,W5,A3,A6,A7,A7A,A7B,A7C,A8A,A9OPP   70
     1 ,W8,A8B,SI3S,A8,QSIG3S                                           OPP   80
C                                                                       OPP   90
      DIMENSION SR(10),RRO2(10),CA(10),D(12),EW(20)                     OPP  100
C                                                                       OPP  110
      DATA KFAOP$/6/                                                    OPP  120
C                                                                       OPP  130
      CHARACTER*4 A(26)/'SING','LE  ','DOUB','LE  ','BED ','OF P','EBBL'OPP  140
     1,'ES  ','LATT','ICE ','OF R','ODS ','ABSO','RBER','PART','ICLS',  OPP  150
     2 'COAT','ED P','ARTI','CLES','    ','IN G','RAPH','ITE-','MATR',  OPP  160
     3 'IX  '/                                                          OPP  170
C                                                                       OPP  180
   57 FORMAT (//' FOR A SINGLE HETEROGENEOUS ARRANGEMENT IS: DNEINF= ', OPP  190
     1 E14.6/' MICROSCOPIC SIGM. OF COATING-MOD. PER ABSORBER ATOM SMIK2OPP  200
     2E=',E14.6/)                                                       OPP  210
   97 FORMAT (/' FOR VERY LARGE SIGM.-ABS.  CDANK=',E14.7,' WILL BE THE OPP  220
     1DANCOFF-FACTOR')                                                  OPP  230
   98 FORMAT (22H0ESCAPE PROBABILITY P=,E18.10,43H PROBABILITY TO LEAVE OPP  240
     1A COATED PARTICLE P1=,E12.5)                                      OPP  250
  100 FORMAT ('1',29X,' SPECIAL CALCULATION OF GEOMETRIC ESCAPE PROBABILOPP  260
     1ITY.'//' ',30X,2A4,'HETEROGENEOUS ',4A4/)                         OPP  270
  101 FORMAT (/' ',30X,4A4,6A4)                                         OPP  280
  102 FORMAT ('   INPUT-DATA:'/'      R1            R2            R4    OPP  290
     1        R5            SI2           SI4           SI5         FILLOPP  300
     2.FACTOR  FRACT.DUMMY-BALLS'/' ',9E14.6)                           OPP  310
  103 FORMAT (/' MESH-POINT  ',I3,', SNU=',E14.7,', SIGA=',E14.7,', SI1=OPP  320
     1',E14.7,', SI3=',E14.7/)                                          OPP  330
  197 FORMAT (' ADEQUATE FOR SINGLE HETEROG. ARRANGEMENT :',E14.6)      OPP  340
  310 FORMAT (//' A1, A2, QSIG3,W5, W8, DANC.FACT.W(14)=...'/6E20.10/)  OPP  350
  320 FORMAT (//'0****CAUTION, F IS TOO LARGE'///)                      OPP  360
C                                                                       OPP  370
      NAMELIST /TESTGR/ A1,A2,A3,A4,A5,A6,A7,A8,A7A,A8A,A7B,A8B,A7C,A8C,OPP  380
     1 SI3,SI3S,QSIG3,QSIG3S                                            OPP  390
C                                                                       OPP  400
C                                                                       OPP  410
C     EINGABE                                                           OPP  420
C                                                                       OPP  430
      R1 = EW(1)                                                        OPP  440
      R2 = EW(2)                                                        OPP  450
      R4 = EW(3)                                                        OPP  460
      R5 = EW(4)                                                        OPP  470
      F = EW(5)                                                         OPP  480
      H = EW(6)                                                         OPP  490
      SI2 = EW(8)                                                       OPP  500
      SI4 = EW(9)                                                       OPP  510
      SI5 = EW(10)                                                      OPP  520
      SIGP = EW(11)                                                     OPP  530
      SI1M = EW(12)                                                     OPP  540
      DN = EW(16)                                                       OPP  550
      BLF = 1.                                                          OPP  560
      ALPH = EW(18)                                                     OPP  570
      IFF = 0                                                           OPP  580
      IF(F .GE. 0.) GOTO 2                                              OPP  590
      F = DABS(F)                                                       OPP  600
      IFF = 1                                                           OPP  610
    2 CONTINUE                                                          OPP  620
      EW(15) = EW(15) + 1.                                              OPP  630
      IFALL = IFIX(SNGL(EW(15)))                                        OPP  640
      SI1 = DEXP(SNU)                                                   OPP  650
      SIGA = (SI1-SI1M) / DN - SIGP                                     OPP  660
      EW(19) = SIGA                                                     OPP  670
C                                                                       OPP  680
C     HERSTELLEN DER RECHENGROESSEN                                     OPP  690
C                                                                       OPP  700
      CNUMI = 0.D0                                                      OPP  710
      DO 10 I=1,10                                                      OPP  720
        SR(I) = 0.D0                                                    OPP  730
        CA(I) = 0.D0                                                    OPP  740
        D(I) = 0.D0                                                     OPP  750
        RRO2(I) = 0.D0                                                  OPP  760
   10 CONTINUE                                                          OPP  770
      R3 = R4 - R2                                                      OPP  780
      IF(R2 .NE. 0.0) GOTO 81                                           OPP  790
      SI3 = SI1                                                         OPP  800
   81 CONTINUE                                                          OPP  810
      SR(1) = SI1 * R1                                                  OPP  820
      SR(2) = SI2 * R1                                                  OPP  830
      SR(3) = SI2 * R2                                                  OPP  840
      SR(7) = SI4 * R3                                                  OPP  850
      SR(8) = SI4 * R4                                                  OPP  860
      SR(9) = SI4 * R5                                                  OPP  870
      SR(10) = SI5 * R5                                                 OPP  880
      RRO2(1) = 0.                                                      OPP  890
      QSIG3 = 0.                                                        OPP  900
      IF(R1 .EQ. 0.) GOTO 82                                            OPP  910
      RRO2(1) = (R2/R1)**2                                              OPP  920
   82 CONTINUE                                                          OPP  930
      RRO2(2) = (R5/R3)**2                                              OPP  940
      RRO2(3) = (R5/R4)**2                                              OPP  950
      RRO2(4) = (R4/R3)**2                                              OPP  960
C                                                                       OPP  970
C     HERSTELLEN DER WAHRSCHEINLICHKEITEN A                             OPP  980
C                                                                       OPP  990
      IF(R2 .EQ. 0.) GOTO 16                                            OPP 1000
      H1 = 3. / (4.*SR(1))                                              OPP 1010
      IF(IFF .GT. 0) GOTO 321                                           OPP 1020
      UND = 2.D0                                                        OPP 1030
      D(1) = SR(2)                                                      OPP 1040
      D(2) = SR(1)                                                      OPP 1050
      D(4) = RRO2(1)                                                    OPP 1060
      ALF = BLF - ALPH                                                  OPP 1070
      DALF = DEXP(-SR(3)*2.*ALPH/(3.*F))                                OPP 1080
      D(6) = ALF                                                        OPP 1090
C                                                                       OPP 1100
      CALL NUMINT(UND,D,CNUMA)                                          OPP 1110
C                                                                       OPP 1120
      C1 = CNUMA                                                        OPP 1130
      A2 = H1 * C1 * DALF                                               OPP 1140
      CA(1) = SR(1)                                                     OPP 1150
C                                                                       OPP 1160
      CALL ANINTF(CA)                                                   OPP 1170
C                                                                       OPP 1180
      A1 = H1 * (1.-CA(4)) - A2                                         OPP 1190
C                                                                       OPP 1200
C     JETZT DAS SI3                                                     OPP 1210
C                                                                       OPP 1220
      UND = 1.D0                                                        OPP 1230
      D(6) = ALF                                                        OPP 1240
C                                                                       OPP 1250
      CALL NUMINT(UND,D,CNUMI)                                          OPP 1260
C                                                                       OPP 1270
      CA(1) = SR(3) * ALF                                               OPP 1280
      CA(2) = 1.D0 / RRO2(1)                                            OPP 1290
C                                                                       OPP 1300
      CALL ANINTF(CA)                                                   OPP 1310
C                                                                       OPP 1320
      CA(6) = CNUMI * DALF**2                                           OPP 1330
      CA(7) = CNUMA * DALF                                              OPP 1340
      CA(4) = CA(4) * DALF**2                                           OPP 1350
      CA(5) = 1. - CA(4)                                                OPP 1360
      CA(8) = CA(4) + CA(6) * CA(2)                                     OPP 1370
      CA(9) = CA(5) - CA(6) * CA(2)                                     OPP 1380
      CA(7) = CA(9) - CA(7) * CA(2)                                     OPP 1390
      CA(10) = DLOG(1.D0/CA(8))                                         OPP 1400
      A7C = CA(7)                                                       OPP 1410
      A8C = CA(8)                                                       OPP 1420
      A9C = CA(9)                                                       OPP 1430
      QSIG3 = 1. - C1 * DALF * CA(2) / A9C                              OPP 1440
      SI3 = CA(10) * 3. * F / (4.*R2)                                   OPP 1450
      GOTO 16                                                           OPP 1460
  321 CONTINUE                                                          OPP 1470
      ELR = 2. / (3.*F)                                                 OPP 1480
      IF(ELR .LT. 1.) WRITE (KFAOP$,320)                                OPP 1490
      ZE2 =  DEXP(-SR(2)*ELR)                                           OPP 1500
      IF(ELR .LT. (2.*R2/R1)) GOTO 324                                  OPP 1510
      ZE1 = ZE2                                                         OPP 1520
      GOTO 323                                                          OPP 1530
  324 CONTINUE                                                          OPP 1540
      ZE1 = DEXP(SR(3))                                                 OPP 1550
      ZE2 = ZE2**2 / ZE1                                                OPP 1560
  323 CONTINUE                                                          OPP 1570
C                                                                       OPP 1580
      A8C = ZE1 * ZE2 * ANI(2.*(SR(1)-SR(2)))                           OPP 1590
C                                                                       OPP 1600
      A1 = ANI(-SR(2)) - ANI(2.*SR(1)-SR(2))                            OPP 1610
C                                                                       OPP 1620
      A7C = 1. - A8C - ZE1 * A1                                         OPP 1630
      A2 = H1 * ZE2 * A1                                                OPP 1640
C                                                                       OPP 1650
      A1 = H1 * (1.-ANI(2.*SR(1))) - A2                                 OPP 1660
C                                                                       OPP 1670
      A9C = 1. - A8C                                                    OPP 1680
      QSIG3 = A7C / (1.-A8C)                                            OPP 1690
      SI3 =  DLOG(1./A8C) / (2.*ELR*R1)                                 OPP 1700
   16 CONTINUE                                                          OPP 1710
      SR(4) = SI3 * R2                                                  OPP 1720
      SR(5) = SI3 * R3                                                  OPP 1730
      SR(6) = SI3 * R4                                                  OPP 1740
      IF(IFALL .GT. 1) GOTO 110                                         OPP 1750
      I1 = 3                                                            OPP 1760
      I3 = 5                                                            OPP 1770
      I5 = 17                                                           OPP 1780
      I7 = 21                                                           OPP 1790
      IF(R2 .EQ. 0.) I1 = 1                                             OPP 1800
      IF(R5 .EQ. 0.) I3 = 9                                             OPP 1810
      IF(IFF .EQ. 1) I5 = 13                                            OPP 1820
      IF(R1 .EQ. R2 .AND. R1 .GT. 0.) I5 = 13                           OPP 1830
      I2 = I1 + 1                                                       OPP 1840
      I4 = I3 + 3                                                       OPP 1850
      I6 = I5 + 3                                                       OPP 1860
      I8 = I7 + 5 * IFF                                                 OPP 1870
      IF(ALPH .GT. 0.) I8 = I7 + 5                                      OPP 1880
      WRITE (KFAOP$,100) (A(I),I=I1,I2),(A(I),I=I3,I4)                  OPP 1890
      IF(R2 .NE. 0.) WRITE (KFAOP$,101) (A(I),I=I5,I6),(A(I),I=I7,I8)   OPP 1900
      WRITE (KFAOP$,102) R1,R2,R4,R5,SI2,SI4,SI5,F,H                    OPP 1910
  110 CONTINUE                                                          OPP 1920
C                                                                       OPP 1930
C     DIE ANDEREN A                                                     OPP 1940
C                                                                       OPP 1950
      EF = 1.                                                           OPP 1960
      IF(ALPH .GT. 0. .OR. IFF .GT. 0) EF = DEXP(SR(4)*(2./(3.*F)-1.))  OPP 1970
      IF(R5 .LE. 0.) GOTO 300                                           OPP 1980
      D(1) = SR(5)                                                      OPP 1990
      D(2) = SR(5)                                                      OPP 2000
      D(3) = SR(7)                                                      OPP 2010
      D(4) = RRO2(4)                                                    OPP 2020
      D(5) = RRO2(2)                                                    OPP 2030
      D(11) = SR(4) * 2.D0 / (3.D0*F)                                   OPP 2040
      D(12) = DEXP(-D(11))                                              OPP 2050
      CA(10) = 3.D0 * DEXP(SR(4)) / (4.D0*SR(5))                        OPP 2060
      UND = 4.D0                                                        OPP 2070
      D(6) = 1.                                                         OPP 2080
C                                                                       OPP 2090
      CALL NUMINT(UND,D,CNUMA)                                          OPP 2100
C                                                                       OPP 2110
      UND = 3.D0                                                        OPP 2120
      D(6) = 1.                                                         OPP 2130
C                                                                       OPP 2140
      CALL NUMINT(UND,D,CNUMI)                                          OPP 2150
C                                                                       OPP 2160
      A4 = CA(10) * (CNUMA-CNUMI)                                       OPP 2170
      A5 = (CA(10)*CNUMI)                                               OPP 2180
      A4 = A4 * EF                                                      OPP 2190
      A5 = A5 * EF                                                      OPP 2200
      A3 = QSIG3 * (1.-A4-A5)                                           OPP 2210
      D(1) = SR(8)                                                      OPP 2220
      D(2) = SR(6)                                                      OPP 2230
      D(4) = RRO2(3)                                                    OPP 2240
      CA(1) = SR(9)                                                     OPP 2250
      CA(2) = 1.D0 / RRO2(3)                                            OPP 2260
      UND = 1.D0                                                        OPP 2270
      D(6) = 1.                                                         OPP 2280
C                                                                       OPP 2290
      CALL NUMINT(UND,D,CNUMI)                                          OPP 2300
C                                                                       OPP 2310
      UND = 2.D0                                                        OPP 2320
      D(6) = 1.                                                         OPP 2330
C                                                                       OPP 2340
      CALL NUMINT(UND,D,CNUMA)                                          OPP 2350
C                                                                       OPP 2360
      CALL ANINTF(CA)                                                   OPP 2370
C                                                                       OPP 2380
      A8A = (CA(4)+CA(2)*CNUMI)                                         OPP 2390
      A9A = (CA(5)-CA(2)*CNUMI)                                         OPP 2400
      A7A = (CA(5)-CA(2)*(CNUMI+CNUMA))                                 OPP 2410
      A6A = QSIG3 * (1.-A7A-A8A)                                        OPP 2420
      CA(1) = SR(10)                                                    OPP 2430
      CA(2) = 0.D0                                                      OPP 2440
C                                                                       OPP 2450
      CALL ANINTF(CA)                                                   OPP 2460
C                                                                       OPP 2470
      A8B = CA(4)                                                       OPP 2480
      A7B = CA(5)                                                       OPP 2490
C                                                                       OPP 2500
C     BERECHNUNG DER ENTKOMMWAHRSCHEINLICHKEIT P                        OPP 2510
C                                                                       OPP 2520
      A7 = (1.-H) * A7A + H * A7B                                       OPP 2530
      A9 = (1.-H) * A9A + H * A7B                                       OPP 2540
      A8 = (1.-H) * A8A + H * A8B                                       OPP 2550
      A6 = (1.-H) * A6A                                                 OPP 2560
      GOTO 309                                                          OPP 2570
  300 CONTINUE                                                          OPP 2580
C                                                                       OPP 2590
      CALL STAEBE(R2,R4,SI3,W5,W8)                                      OPP 2600
C                                                                       OPP 2610
      W5 = W5 * EF                                                      OPP 2620
  309 CONTINUE                                                          OPP 2630
      IF(R2 .NE. 0.) GOTO 83                                            OPP 2640
      A1 = 0.                                                           OPP 2650
      A2 = 1.                                                           OPP 2660
      A3 = 0.                                                           OPP 2670
      A6 = 0.                                                           OPP 2680
   83 CONTINUE                                                          OPP 2690
      P1 = A1 + A2                                                      OPP 2700
      IF(R5 .GT. 0.) GOTO 301                                           OPP 2710
      P = A1 + A2 * (QSIG3 + W5*(1.-QSIG3)*(1.-EW(14))/(1.-EW(14)*W8))  OPP 2720
      GOTO 302                                                          OPP 2730
  301 CONTINUE                                                          OPP 2740
      P = A1 + A2 * (A3+A4+A5*(A6+A7)/A9)                               OPP 2750
  302 CONTINUE                                                          OPP 2760
      PNU = P                                                           OPP 2770
      IF(IFALL .EQ. 1) GOTO 30                                          OPP 2780
      IF((EW(7)-DFLOAT(IFIX(SNGL(EW(7)))/10)*10.)- 1.) 31,30,30         OPP 2790
   30 CONTINUE                                                          OPP 2800
      WRITE (KFAOP$,103) IFALL,SNU,SIGA,SI1,SI3                         OPP 2810
      IF(R2 .EQ. 0.) GOTO 84                                            OPP 2820
      IF(IFF .EQ. 0) GOTO 325                                           OPP 2830
      SI31S = SI1 * F                                                   OPP 2840
      SI32S = SI2 * (1.-F)                                              OPP 2850
      DNEINF = DN * F                                                   OPP 2860
      GOTO 326                                                          OPP 2870
  325 CONTINUE                                                          OPP 2880
      RO3 = (R1/R2)**3                                                  OPP 2890
      SI31S = SI1 * F * RO3                                             OPP 2900
      SI32S = SI2 * (F*(1.-RO3)+ ALPH*(1.-F))                           OPP 2910
      DNEINF = DN * F * RO3                                             OPP 2920
  326 CONTINUE                                                          OPP 2930
      SI3S = SI31S + SI32S                                              OPP 2940
      QSIG3S = SI32S / SI3S                                             OPP 2950
      SMIK2E = SI32S / DNEINF                                           OPP 2960
      IF(IFALL .GT. 1) GOTO 84                                          OPP 2970
      WRITE (KFAOP$,57) DNEINF,SMIK2E                                   OPP 2980
   84 CONTINUE                                                          OPP 2990
      WRITE (KFAOP$,98) P,P1                                            OPP 3000
      IF(R2 .NE. 0.) GOTO 85                                            OPP 3010
      IF(R5 .GT. 0.) GOTO 305                                           OPP 3020
      A1 = 0.                                                           OPP 3030
      A2 = W5                                                           OPP 3040
      GOTO 306                                                          OPP 3050
  305 CONTINUE                                                          OPP 3060
      A1 = A4                                                           OPP 3070
      A2 = A5                                                           OPP 3080
  306 CONTINUE                                                          OPP 3090
   85 CONTINUE                                                          OPP 3100
      IF(SIGA-10000.) 41,40,40                                          OPP 3110
   40 CDANK = 1. - P / (A1+A2)                                          OPP 3120
      IF(R5 .GT. 0.) EW(14) = 1. - (A4+A5*A7/A9) / (A4+A5)              OPP 3130
      WRITE (KFAOP$,97) CDANK                                           OPP 3140
      WRITE (KFAOP$,197) EW(14)                                         OPP 3150
   41 CONTINUE                                                          OPP 3160
   31 CONTINUE                                                          OPP 3170
      IF((EW(7)-DFLOAT(IFIX(SNGL(EW(7)))/100)*100.)-10.) 17,202,202     OPP 3180
  202 CONTINUE                                                          OPP 3190
      WRITE (KFAOP$,TESTGR)                                             OPP 3200
      IF(R5 .LE. 0.) WRITE (KFAOP$,310) A1,A2,QSIG3,W5,W8,EW(14)        OPP 3210
   17 CONTINUE                                                          OPP 3220
      RETURN                                                            OPP 3230
      END                                                               OPP 3240
      FUNCTION ANI(X)                                                   NI    10
C                                                                       NI    20
      IMPLICIT REAL*8(A-H,O-Z)                                          NI    30
C                                                                       NI    40
C                                                                       NI    50
      Y = X                                                             NI    60
      IF(DABS(Y) .LT. 0.001) GOTO 1                                     NI    70
      IF(DABS(Y) .GT. 130.) Y = -1.                                     NI    80
C                                                                       NI    90
      ANI = 2. * (1.-(1.+Y)*DEXP(-Y)) / (X**2)                          NI   100
C                                                                       NI   110
      RETURN                                                            NI   120
    1 CONTINUE                                                          NI   130
      IF(X .EQ. 0.) GOTO 2                                              NI   140
C                                                                       NI   150
      ANI = 1. - Y * 2. / 3. + (Y**2) / 4. - (Y**3) / 15.               NI   160
C                                                                       NI   170
      RETURN                                                            NI   180
    2 CONTINUE                                                          NI   190
C                                                                       NI   200
      ANI = 0.                                                          NI   210
C                                                                       NI   220
      RETURN                                                            NI   230
      END                                                               NI   240
      SUBROUTINE STAEBE(R2,R4,SIGT,W5,W8)                               TAE   10
C                                                                       TAE   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         TAE   30
C                                                                       TAE   40
      DIMENSION V(6),Z(5),W(6)                                          TAE   50
C                                                                       TAE   60
C                                                                       TAE   70
      N6 = 6                                                            TAE   80
      DO 3 K=1,5                                                        TAE   90
        W(K) = 0.                                                       TAE  100
        V(K) = 0.                                                       TAE  110
        Z(K) = 0.                                                       TAE  120
    3 CONTINUE                                                          TAE  130
      ALF = R4 * SIGT                                                   TAE  140
      BET = 1. - ((R4-R2)/R4)**2                                        TAE  150
C                                                                       TAE  160
      CALL INT2(V,W,Z,ALF,BET,N6,R2,SIGT,W5,W8)                         TAE  170
C                                                                       TAE  180
      RETURN                                                            TAE  190
      END                                                               TAE  200
      SUBROUTINE INT2(V,W,Z,ALF,BET,N6,R2,SIGT,W5,W8)                   INT   10
C                                                                       INT   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         INT   30
C                                                                       INT   40
      DIMENSION V(6),W(6),Z(5),IL(4),IK(4),FS(2)                        INT   50
C                                                                       INT   60
   10 FORMAT (///I6,' STEPS OF INTEGRATION FOR THE EXTERNAL INTEGRAL. UPINT   70
     1 TO W(3)=',E24.15//20X,'**NOT CONVERGED**'//)                     INT   80
   11 FORMAT (//I6,' INTEGRATION STEPS, L7=',I6/' QUOTA=',E24.14,', EPS1INT   90
     1=',E24.14,', INTEGRAL=',E24.14//)                                 INT  100
C                                                                       INT  110
C                                                                       INT  120
      PI = 3.141592653589793                                            INT  130
      PID2 = PI / 2.                                                    INT  140
      DO 2 L7=1,2                                                       INT  150
        M = 1                                                           INT  160
        PROV2 = 0.                                                      INT  170
        W(1) = PID2                                                     INT  180
        W(6) = 0.                                                       INT  190
        EPS1 = 2.0D-4                                                   INT  200
        EPS2 = EPS1 * 0.01                                              INT  210
        SCHRIT = (W(6)-W(1)) / 4.                                       INT  220
        DO 1 I=1,4                                                      INT  230
          IL(I) = 0                                                     INT  240
    1   CONTINUE                                                        INT  250
        FSI = 0.                                                        INT  260
        FS1 = 0.                                                        INT  270
        DX = 0.                                                         INT  280
    3   CONTINUE                                                        INT  290
C                                                                       INT  300
        CALL SIMP2(W,Z,FSI,EPS1,EPS2,SCHRIT,M,IB,IT,DX,IL,FS1,PROV2)    INT  310
C                                                                       INT  320
        GOTO(6,4),M                                                     INT  330
    4   CONTINUE                                                        INT  340
        DO 5 J=IB,5,IT                                                  INT  350
          V(J) = DCOS(W(J))**2                                          INT  360
C                                                                       INT  370
          CALL INT1(V,Z,J,L7,ALF,BET,N6,F1SIM,IK,U0)                    INT  380
C                                                                       INT  390
    5   CONTINUE                                                        INT  400
        IF(IL(4) .LT. 300) GOTO 3                                       INT  410
        WRITE (N6,10) IL(4),W(3)                                        INT  420
    6   CONTINUE                                                        INT  430
        QUOTA = 1.                                                      INT  440
        IF(FSI .EQ. 0.) GOTO 7                                          INT  450
        QUOTA = DABS(1.-FS1/FSI)                                        INT  460
    7   CONTINUE                                                        INT  470
        WRITE (N6,11) IL(4),L7,QUOTA,EPS1,FSI                           INT  480
        FS(L7) = FSI                                                    INT  490
    2 CONTINUE                                                          INT  500
      W8 = 1. - FS(1) * 2. / PI                                         INT  510
      W5 = DEXP(R2*SIGT) * FS(2) / (PI*ALF*(1.-BET))                    INT  520
      RETURN                                                            INT  530
      END                                                               INT  540
      SUBROUTINE INT1(V,Z,J,L7,ALF,BET,N6,F1SIM,IK,U0)                  NT1   10
C                                                                       NT1   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         NT1   30
C                                                                       NT1   40
      COMMON /UI/ IDUM(9)                                               NT1   50
C                                                                       NT1   60
      COMMON /U8/ DUM(21),QUOT                                          NT1   70
C                                                                       NT1   80
      DIMENSION IK(4),U(6),Y(5),V(6),Z(5)                               NT1   90
C                                                                       NT1  100
   10 FORMAT (//I6,' STEPS OF INTEGRATION AT V(J) =',E25.15/' BETWEEN U NT1  110
     1=',E24.15,' AND',E24.15,'. ***NOT CONVERGED***')                  NT1  120
   82 FORMAT (//' QUOT=',E24.14,', NEW INTEGRATION WITH EPSI1=',D24.14/)NT1  130
C                                                                       NT1  140
C                                                                       NT1  150
      L = 1                                                             NT1  160
      U0 = BET * V(J) / (1.-BET*(1.-V(J)))                              NT1  170
      IF(L7 .EQ. 1) U0 = 0.                                             NT1  180
      U(6) = U0                                                         NT1  190
      IANZ = 0                                                          NT1  200
      EPSI0 = 4.0D-6                                                    NT1  210
      EPSI1 = 1.0D-1                                                    NT1  220
      FPROV = 0.                                                        NT1  230
   80 CONTINUE                                                          NT1  240
      ANZ = 100.**IANZ                                                  NT1  250
      EPSI1 = EPSI0 / (ANZ*30.)                                         NT1  260
      IF(IANZ .GT. 0) WRITE (N6,82) QUOT,EPSI1                          NT1  270
      U(1) = 1.                                                         NT1  280
      SCHRIT = U(6) - U(1)                                              NT1  290
      FPROV = FPROV / SCHRIT                                            NT1  300
      SCHRIT = SCHRIT / (2.*ANZ)                                        NT1  310
      EPSI2 = EPSI1 * 0.01                                              NT1  320
      FSIMP = 0.                                                        NT1  330
      DX1 = 0.                                                          NT1  340
      DO 1 I=1,4                                                        NT1  350
        IK(I) = 0                                                       NT1  360
    1 CONTINUE                                                          NT1  370
    2 CONTINUE                                                          NT1  380
C                                                                       NT1  390
      CALL SIMP2(U,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIM,FPROVNT1  400
     1 )                                                                NT1  410
C                                                                       NT1  420
      GOTO(20,3),L                                                      NT1  430
    3 CONTINUE                                                          NT1  440
      DO 9 I=IA,5,IS                                                    NT1  450
        IF(U(I) .GT. 0.) GOTO 4                                         NT1  460
        Y(I) = 0.                                                       NT1  470
        GOTO 9                                                          NT1  480
    4   CONTINUE                                                        NT1  490
        D1 = U(I) / (U(I)*(1.-V(J))+V(J))                               NT1  500
        F1 = ALF * D1 / DSQRT(U(I))                                     NT1  510
        WU = 1.                                                         NT1  520
        EMBD1 = 1. - BET / D1                                           NT1  530
        IF(EMBD1 .LT. 0.) EMBD1 = 0.                                    NT1  540
        IF(L7 .GT. 1) WU = DSQRT(EMBD1)                                 NT1  550
        E1 = F1 * (1.-WU)                                               NT1  560
        E2 = F1 * (1.+WU)                                               NT1  570
        EX1 = 0.                                                        NT1  580
        EX2 = 0.                                                        NT1  590
        IF(E1 .LT. 80.) EX1 = DEXP(-E1)                                 NT1  600
        IF(E2 .LT. 80.) EX2 = DEXP(-E2)                                 NT1  610
        Y(I) = EX1 - EX2                                                NT1  620
    9 CONTINUE                                                          NT1  630
      IF(IK(4) .LT. 600) GOTO 2                                         NT1  640
      WRITE (N6,10) IK(4),V(J),U0,U(3)                                  NT1  650
   20 Z(J) = FSIMP                                                      NT1  660
      QUOT = 1.                                                         NT1  670
      IF(FSIMP .EQ. 0.) GOTO 81                                         NT1  680
      QUOT = DABS(1.-F1SIM/FSIMP)                                       NT1  690
   81 CONTINUE                                                          NT1  700
      IF(QUOT .LT. EPSI0) GOTO 90                                       NT1  710
      IANZ = IANZ + 1                                                   NT1  720
      FPROV = FSIMP                                                     NT1  730
      GOTO 80                                                           NT1  740
   90 CONTINUE                                                          NT1  750
      RETURN                                                            NT1  760
      END                                                               NT1  770
      SUBROUTINE ZUTGAM                                                 UTG   10
C                                                                       UTG   20
      DIMENSION RI(37),EO(37),ENULL(500),CORINT(500),ISAZ(700),         UTG   30
     1 INUCL(700),LOESCH(9),RIZW(37)                                    UTG   40
C                                                                       UTG   50
      COMMON /GAM/ M6,NGAM,IDENT,IDSATZ,IDNUCL,ENULL,CORINT,NUM1,LOESCH,UTG   60
     1 M9                                                               UTG   70
C                                                                       UTG   80
   85 FORMAT ('1 ALL',I6,' SETS ON DATA UNIT ARE OCCUPIED. THERE IS NO PUTG   90
     1LACE FOR THE NEW ONE')                                            UTG  100
  101 FORMAT ('1PROGRAM WRITES ON UNIT',I6//' WITH IDENTIFICATION NO.', UTG  110
     1 I6,', THIS NO. MUST AGREE WITH',I6//' THERE IS PLACE FOR',I6,' SEUTG  120
     2TS.')                                                             UTG  130
  102 FORMAT (////' INCORRECT IDENTIFICATON NO.; THE NEW SET CANNOT BE SUTG  140
     1TORED.')                                                          UTG  150
  103 FORMAT (/////' RESONANCES OVER',E12.5,' EV CANNOT BE STORED')     UTG  160
  104 FORMAT (///' THE RESONANCES ARE WRITTEN TO FOLLOWING GROUPS:'//'  UTG  170
     1  NO.  EN.GRP.UPPER LIM.  RES.INT.'//(I6,E17.5,E14.5))            UTG  180
  105 FORMAT (//' THEY WILL BE STRORED UNDER REFERENCE NO.',I6,'  NUCLIDUTG  190
     1E NO.',I6)                                                        UTG  200
  107 FORMAT (//' I, ISAZ(I), INUCL(I), ...'/6(I6,I6,I4,2X)/)           UTG  210
  108 FORMAT (////' THE UNIT CONTAINS NOW FOLLOWING SETS:')             UTG  220
C                                                                       UTG  230
C                                                                       UTG  240
      REWIND M9                                                         UTG  250
      REWIND NGAM                                                       UTG  260
      READ (NGAM) IDENTI,IZAHL,NGR                                      UTG  270
      WRITE (M6,101) NGAM,IDENTI,IDENT,IZAHL                            UTG  280
      IF(IDENTI .EQ. IDENT) GOTO 2                                      UTG  290
      WRITE (M6,102)                                                    UTG  300
      GOTO 99                                                           UTG  310
    2 CONTINUE                                                          UTG  320
      DO 3 I=1,NGR                                                      UTG  330
        ZI = FLOAT(I)                                                   UTG  340
        RI(I) = 0.                                                      UTG  350
        EO(I) = 1.0E7 / EXP((30.+ZI)/4.)                                UTG  360
    3 CONTINUE                                                          UTG  370
C                                                                       UTG  380
C     ZUSAMMENFASSEN DER RESONANZINTEGRALE ZU GAM-GRUPPEN               UTG  390
C                                                                       UTG  400
      IG = NGR                                                          UTG  410
      DO 13 I=1,NUM1                                                    UTG  420
   10   CONTINUE                                                        UTG  430
        IF(ENULL(I)-EO(IG)) 11,11,12                                    UTG  440
   11   CONTINUE                                                        UTG  450
        RI(IG) = RI(IG) + CORINT(I)                                     UTG  460
        GOTO 13                                                         UTG  470
   12   CONTINUE                                                        UTG  480
        IG = IG - 1                                                     UTG  490
        IF(IG .GT. 0) GOTO 10                                           UTG  500
        IG = IG + 1                                                     UTG  510
        WRITE (M6,103) EO(IG)                                           UTG  520
   13 CONTINUE                                                          UTG  530
      WRITE (M6,104) (IG,EO(IG),RI(IG),IG=1,NGR)                        UTG  540
      WRITE (M6,105) IDSATZ,IDNUCL                                      UTG  550
C                                                                       UTG  560
C     LOESCHEN UND VERBUCHEN IM REGISTER DES DATENSATZES                UTG  570
C                                                                       UTG  580
      LOE = 0                                                           UTG  590
      DO 53 LO=1,9                                                      UTG  600
        LOE = LOE + LOESCH(LO)                                          UTG  610
   53 CONTINUE                                                          UTG  620
      II = 0                                                            UTG  630
      DO 40 I=1,IZAHL                                                   UTG  640
        II = II + 1                                                     UTG  650
        READ (NGAM) ISAZ(II),INUCL(II),(RIZW(J),J=1,NGR)                UTG  660
        ISAZI = ISAZ(II)                                                UTG  670
        IF(LOE .LE. 0) GOTO 55                                          UTG  680
        DO 41 LO=1,9                                                    UTG  690
          IF(ISAZ(II) .EQ. LOESCH(LO)) GOTO 42                          UTG  700
   41   CONTINUE                                                        UTG  710
   55   CONTINUE                                                        UTG  720
        WRITE (M9) ISAZ(II),INUCL(II),(RIZW(J),J=1,NGR)                 UTG  730
        GOTO 43                                                         UTG  740
   42   CONTINUE                                                        UTG  750
        II = II - 1                                                     UTG  760
   43   CONTINUE                                                        UTG  770
        IF(ISAZI .LT. 0) GOTO 44                                        UTG  780
   40 CONTINUE                                                          UTG  790
      IF(II .LT. IZAHL) GOTO 44                                         UTG  800
      WRITE (M6,85) IZAHL                                               UTG  810
      GOTO 99                                                           UTG  820
C                                                                       UTG  830
C     VERBUCHEN DES NEUEN SATZES                                        UTG  840
C                                                                       UTG  850
   44 CONTINUE                                                          UTG  860
      REWIND NGAM                                                       UTG  870
      REWIND M9                                                         UTG  880
      III = II + 1                                                      UTG  890
      ISAZ(III) = ISAZ(II)                                              UTG  900
      INUCL(III) = INUCL(II)                                            UTG  910
      ISAZ(II) = IDSATZ                                                 UTG  920
      INUCL(II) =IDNUCL                                                 UTG  930
      WRITE (NGAM) IDENTI,IZAHL,NGR,(RI(J),J=1,NGR)                     UTG  940
      IF(II .LE. 1) GOTO 50                                             UTG  950
      IM = II - 1                                                       UTG  960
      DO 49 I=1,IM                                                      UTG  970
        READ (M9) ISAZ(I),INUCL(I),(RIZW(J),J=1,NGR)                    UTG  980
        WRITE (NGAM) ISAZ(I),INUCL(I),(RIZW(J),J=1,NGR)                 UTG  990
   49 CONTINUE                                                          UTG 1000
   50 CONTINUE                                                          UTG 1010
      DO 51 I=II,III                                                    UTG 1020
        WRITE (NGAM) ISAZ(I),INUCL(I),(RI(J),J=1,NGR)                   UTG 1030
   51 CONTINUE                                                          UTG 1040
      IZAHL = III                                                       UTG 1050
      WRITE (M6,108)                                                    UTG 1060
      WRITE (M6,107) (I,ISAZ(I),INUCL(I),I=1,IZAHL)                     UTG 1070
   99 CONTINUE                                                          UTG 1080
      NGAM = 0                                                          UTG 1090
      RETURN                                                            UTG 1100
      END                                                               UTG 1110
      SUBROUTINE GAMDAT(IDENTI,IZAHL,NGR,NGAM,N6)                       AMD   10
C                                                                       AMD   20
      DIMENSION RI(40),J(10)                                            AMD   30
C                                                                       AMD   40
    6 FORMAT ('1',20X,'A NEW DATA UNIT WILL BE PREPARED'///' IDENTIFICATAMD   50
     1ION IDENTI=',I6,',  DISC UNIT NGAM=',I6/)                         AMD   60
    7 FORMAT (' AVAILABLE STORAGE FOR SET NO.',I5)                      AMD   70
    8 FORMAT (/' THE DESIRED',I5,' SETS WITH',I3,' ENERGY GROUPS CAN BE AMD   80
     1STORED'////)                                                      AMD   90
    9 FORMAT (10I2)                                                     AMD  100
   12 FORMAT (' ATTENTION, CHANGE IN PROGRAM:'/' AFTER CARD Z19 CARD Z20AMD  110
     1 WITH (K(I),I=1,3), FORMAT(3I2), MUST BE READ.'/' ALL K(I) HAVE TOAMD  120
     2 BE = -1 .'/' PLEASE ADD THIS CARD.')                             AMD  130
C                                                                       AMD  140
C                                                                       AMD  150
      REWIND NGAM                                                       AMD  160
      IF(NGR .EQ. 0) NGR = 37                                           AMD  170
      WRITE (N6,6) IDENTI,NGAM                                          AMD  180
      DO 1 I=1,NGR                                                      AMD  190
        RI(I) = 0.                                                      AMD  200
    1 CONTINUE                                                          AMD  210
      IG = -9                                                           AMD  220
C                                                                       AMD  230
      READ (5,9) (J(I),I=1,3)                                           AMD  240
C                                                                       AMD  250
      DO 10 I=1,3                                                       AMD  260
        IF(J(I) .NE. -1) GOTO 11                                        AMD  270
   10 CONTINUE                                                          AMD  280
      WRITE (NGAM) IDENTI,IZAHL,NGR,(RI(I),I=1,NGR)                     AMD  290
      DO 4 K=1,IZAHL                                                    AMD  300
        WRITE (NGAM) IG,IG,(RI(I),I=1,NGR)                              AMD  310
        WRITE (N6,7) K                                                  AMD  320
    4 CONTINUE                                                          AMD  330
      GOTO 13                                                           AMD  340
   11 CONTINUE                                                          AMD  350
      WRITE (N6,12)                                                     AMD  360
      STOP                                                              AMD  370
   13 CONTINUE                                                          AMD  380
      WRITE (N6,8) IZAHL,NGR                                            AMD  390
      RETURN                                                            AMD  400
      END                                                               AMD  410
      SUBROUTINE TUZ                                                    TUZ   10
C                                                                       TUZ   20
      DIMENSION DATA(14),EN(68),UNR(68),FIELD(10)                       TUZ   30
C                                                                       TUZ   40
      REAL*4 SIRB(68)/68*0./                                            TUZ   50
C                                                                       TUZ   60
      COMMON /UI/ IDUM(9),QUOTO,QUOTU                                   TUZ   70
C                                                                       TUZ   80
      COMMON /TUZGAM/ DATA,EN,UNR,A,ANS,AUV,B,EK,FIN,Q000FL,SIGP,STA,XILTUZ   90
     1 ,IU,EO,NEU,NEO,NEOO                                              TUZ  100
C                                                                       TUZ  110
      COMMON /GAGRU/ EGO(69),SIRA(69),FF,FL,M6,NGGR,NGGR1,EOE,EERSTE,   TUZ  120
     1 ELETZT,IO,IV,RIJS,MMEES,IIII,JRJR,ITUZ,IGRUG,M5,NEUI,NALTI,      TUZ  130
     2 UAPS(501)                                                        TUZ  140
C                                                                       TUZ  150
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     TUZ  160
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   TUZ  170
     2 ESIG2,EDIQU2,KI,IEING5                                           TUZ  180
C                                                                       TUZ  190
      EQUIVALENCE(AZERO,DATA(1)),(G,DATA(2)),(SIGPZ,DATA(3)),           TUZ  200
     1 (TEMP,DATA(4)),(EC,DATA(5)),(D,DATA(6)),(GAMNO,DATA(7)),         TUZ  210
     2 (GMGM,DATA(8)),(GEOM,DATA(9)),(ABAR,DATA(10)),(C,DATA(11)),      TUZ  220
     3 (DZERO,DATA(12)),(SIGM1,DATA(13)),(SIGM2,DATA(14))               TUZ  230
C                                                                       TUZ  240
C                                                                       TUZ  250
      IF(ITUZ .GT. 0) GOTO 2                                            TUZ  260
      NEU = 0                                                           TUZ  270
      NEO = 0                                                           TUZ  280
      DO 1 I=1,14                                                       TUZ  290
        DATA(I) = 0.                                                    TUZ  300
    1 CONTINUE                                                          TUZ  310
    2 CONTINUE                                                          TUZ  320
C                                                                       TUZ  330
C     INPUT DATA                                                        TUZ  340
C                                                                       TUZ  350
      ITUZ  = ITUZ + 1                                                  TUZ  360
      GOTO(11,12),ITUZ                                                  TUZ  370
   11 CONTINUE                                                          TUZ  380
      EC = FIELD(1)                                                     TUZ  390
      GAMNO = FIELD(2)                                                  TUZ  400
      GMGM = FIELD(3)                                                   TUZ  410
      G = FIELD(4)                                                      TUZ  420
      D = FIELD(5)                                                      TUZ  430
      EO = FIELD(6)                                                     TUZ  440
      GOTO 99                                                           TUZ  450
   12 CONTINUE                                                          TUZ  460
      SIGPZ = FIELD(1)                                                  TUZ  470
      SIGM1 = FIELD(2)                                                  TUZ  480
      SIGM2 = FIELD(3)                                                  TUZ  490
      C = FIELD(4)                                                      TUZ  500
      GOTO 99                                                           TUZ  510
C                                                                       TUZ  520
      ENTRY TUTS                                                        TUZ  530
C                                                                       TUZ  540
      AZERO = EAZERO                                                    TUZ  550
      DZERO = EDZERO                                                    TUZ  560
      TEMP = ETEMP                                                      TUZ  570
      GEOM = AINT(ESOLVE/10000.)                                        TUZ  580
      IF(GEOM .GT. 0.) GOTO 15                                          TUZ  590
   15 CONTINUE                                                          TUZ  600
      IF(ABAR .EQ. 0.) ABAR = EABAR                                     TUZ  610
      IF(C .EQ. 0.) C = ECDANC                                          TUZ  620
      IF(SIGPZ .EQ. 0.) SIGPZ = ESIGPZ                                  TUZ  630
      IF(SIGM1 .EQ. 0.) SIGM1 = ESIGM1                                  TUZ  640
      IF(SIGM2 .EQ. 0.) SIGM2 = ESIGM2                                  TUZ  650
      IF(ESIG1 .GT. 0.) SIGM1 = SIGM1 * EDIQU1                          TUZ  660
      IF(ESIG2 .GT. 0.) SIGM2 = SIGM2 * EDIQU2                          TUZ  670
      KZ = 0                                                            TUZ  680
      IF(IGRUG .GT. 0) GOTO 40                                          TUZ  690
   50 CONTINUE                                                          TUZ  700
      NGGR = 68                                                         TUZ  710
      NGGR1 = NGGR + 1                                                  TUZ  720
      DO 41 N=1,NGGR1                                                   TUZ  730
        EGO(N) = 1.0E7 / EXP(FLOAT(N-1)/4.)                             TUZ  740
        SIRA(N) = 0.                                                    TUZ  750
   41 CONTINUE                                                          TUZ  760
   40 CONTINUE                                                          TUZ  770
      KZ = KZ + 1                                                       TUZ  780
      DO 30 N=1,NGGR                                                    TUZ  790
        UNR(N) = 0.                                                     TUZ  800
        EN(N) = EGO(N+1)                                                TUZ  810
        ENDELT = EGO(N) - EN(N)                                         TUZ  820
        IF(EN(N) .GE. EO .OR. NEO .NE. 0) GOTO 31                       TUZ  830
        QUOTO = (EO-EN(N)) / ENDELT                                     TUZ  840
        NEO = N                                                         TUZ  850
   31   CONTINUE                                                        TUZ  860
        IF(EN(N) .GT. EC .OR. NEU .NE. 0) GOTO 32                       TUZ  870
        QUOTU = (EGO(N)-EC) / ENDELT                                    TUZ  880
        NEU = N                                                         TUZ  890
   32   CONTINUE                                                        TUZ  900
   30 CONTINUE                                                          TUZ  910
      NEOO = NEO - 1                                                    TUZ  920
      IGEOM = 1.5 + GEOM                                                TUZ  930
      GOTO(5,6,7,8),IGEOM                                               TUZ  940
C                                                                       TUZ  950
C     HOMOGENEOUS CASE                                                  TUZ  960
C                                                                       TUZ  970
    5 SIGP = SIGPZ + SIGM1 + SIGM2                                      TUZ  980
      GOTO 10                                                           TUZ  990
C                                                                       TUZ 1000
C     CYLINDRICAL GEOMETRY                                              TUZ 1010
C                                                                       TUZ 1020
    6 RBAR = 2.0 * ABAR                                                 TUZ 1030
      GOTO 9                                                            TUZ 1040
C                                                                       TUZ 1050
C     SLAB GEOMETRY                                                     TUZ 1060
C                                                                       TUZ 1070
    7 RBAR = 4.0 * ABAR                                                 TUZ 1080
      GOTO 9                                                            TUZ 1090
C                                                                       TUZ 1100
C     SPHERICAL GEOMETRY                                                TUZ 1110
C                                                                       TUZ 1120
    8 RBAR = 1.33333333 * ABAR                                          TUZ 1130
    9 SIGP = SIGPZ + SIGM1 + SIGM2 + (1.0-C) / RBAR / DZERO             TUZ 1140
C                                                                       TUZ 1150
C     UNRESOLVED RESONANCE CALCULATION                                  TUZ 1160
C                                                                       TUZ 1170
   10 CONTINUE                                                          TUZ 1180
      WRITE (6,23) (DATA(I),I=1,14)                                     TUZ 1190
C                                                                       TUZ 1200
      CALL UNRES                                                        TUZ 1210
C                                                                       TUZ 1220
C     PRINT UNRESOLVED RESONANCE OUTPUT                                 TUZ 1230
C                                                                       TUZ 1240
      SIRA(NEO) = SIRA(NEO) * QUOTO                                     TUZ 1250
      SIRA(NEU) = SIRA(NEU) * QUOTU                                     TUZ 1260
      WRITE (6,33) (N,EN(N),EN(N-1),UNR(N),SIRA(N),N=NEO,NEU)           TUZ 1270
      WRITE (6,24) AUV                                                  TUZ 1280
      GO TO(51,52),KZ                                                   TUZ 1290
   51 CONTINUE                                                          TUZ 1300
      DO 55 K=NEO,NEU                                                   TUZ 1310
        SIRB(K) = SIRA(K)                                               TUZ 1320
   55 CONTINUE                                                          TUZ 1330
      DZERO = 1.E-10                                                    TUZ 1340
      GOTO 50                                                           TUZ 1350
   52 CONTINUE                                                          TUZ 1360
      AUV = 0.                                                          TUZ 1370
      WRITE (6,57)                                                      TUZ 1380
      DO 56 K=NEO,NEU                                                   TUZ 1390
        SIRA(K) = SIRB(K) - SIRA(K)                                     TUZ 1400
        AUV = AUV + SIRA(K)                                             TUZ 1410
        WRITE (6,58) K,SIRA(K)                                          TUZ 1420
   56 CONTINUE                                                          TUZ 1430
      WRITE (6,24) AUV                                                  TUZ 1440
   99 CONTINUE                                                          TUZ 1450
      RETURN                                                            TUZ 1460
C                                                                       TUZ 1470
   23 FORMAT (1H0/21H0 INPUT PARAMETERS  -/104H0             A ZERO     TUZ 1480
     1        G    SIG P ZERO          TEMP            EC             D TUZ 1490
     2 GAMMA N ZERO/6H      ,7(1PE14.5)/104H0        GAMMA GAMMA      GETUZ 1500
     3OMETRY         A BAR             C        N ZERO      SIG MOD1    TUZ 1510
     4  SIG MOD2/6H      ,7(1PE14.5))                                   TUZ 1520
   24 FORMAT (19H0 RESONANCE DATA  -,'  INTEGRAL'/48H0                  TUZ 1530
     1                          SUM,1PE15.5)                            TUZ 1540
   33 FORMAT ('1 GRP.     EU      -     EO           UNR            SIGATUZ 1550
     1'/60(I5,E13.6,' -', E13.6,2E15.6/))                               TUZ 1560
   57 FORMAT (///' HETEROGENEOUS INTEGRAL  MINUS  INFINITELY DILUTED INTTUZ 1570
     1EGRAL:'//'  GRP.',47X,'SIGA'/)                                    TUZ 1580
   58 FORMAT (I5,43X,E15.6)                                             TUZ 1590
      END                                                               TUZ 1600
      SUBROUTINE UNRES                                                  UNR   10
C                                                                       UNR   20
      DIMENSION DATA(14),EN(68),UNR(68)                                 UNR   30
C                                                                       UNR   40
      EXTERNAL  BARJ                                                    UNR   50
C                                                                       UNR   60
      COMMON /TUZGAM/ DATA,EN,UNR,A,ANS,AUV,B,EK,FIN,Q000FL,SIGP,STA,XILUNR   70
     1 ,IU,EO,NEU,NEO,NEOO                                              UNR   80
C                                                                       UNR   90
      COMMON /GAGRU/ EGO(69),SIRA(69),TAPS(501),FF,FL,M6,NGGR,NGGR1,EOE,UNR  100
     1 EERSTE,ELETZT,IO,IV,RIJS,MMEES,IIII,JRJR,ITUZ                    UNR  110
C                                                                       UNR  120
      EQUIVALENCE(AZERO,DATA(1)),(G,DATA(2)),(SIGPZ,DATA(3)),           UNR  130
     1 (TEMP,DATA(4)),(EC,DATA(5)),(D,DATA(6)),(GAMNO,DATA(7)),         UNR  140
     2 (GMGM,DATA(8)),(GEOM,DATA(9)),(ABAR,DATA(10)),(C,DATA(11)),      UNR  150
     3 (DZERO,DATA(12)),(SIGM1,DATA(13)),(SIGM2,DATA(14))               UNR  160
C                                                                       UNR  170
C                                                                       UNR  180
      A = SQRT(AZERO/8.6164E-5/TEMP)                                    UNR  190
      AUV = 0.                                                          UNR  200
      NUNGR = NEU - NEO + 2                                             UNR  210
      DO 2 NN=1,NUNGR                                                   UNR  220
        IU = NEU + 1 - NN                                               UNR  230
        B = A * GMGM / SQRT(EN(IU)) / 2.                                UNR  240
        XIL = B + 0.00095                                               UNR  250
        ANS = 0.                                                        UNR  260
        DO 1 JU=1,4                                                     UNR  270
          STA = XIL + FLOAT(JU-1) * (1.0-XIL) / 4.                      UNR  280
          FIN = XIL + FLOAT(JU) * (1.0-XIL) / 4.                        UNR  290
C                                                                       UNR  300
C     BARJ                                                              UNR  310
C                                                                       UNR  320
          CALL SIMPZ(STA,FIN,BARJ,Q000FL,2.5E-5)                        UNR  330
C                                                                       UNR  340
          ANS = ANS + Q000FL                                            UNR  350
    1   CONTINUE                                                        UNR  360
        EK = (ALOG(3.84184E-7*SIGP*EN(IU)*XIL/G/0.00095)+11.512925) /   UNR  370
     1   0.69314718                                                     UNR  380
        UNR(IU) = GMGM * SIGP * (ANS+0.06164414*FJ(XIL,EK)) / D /       UNR  390
     1   SQRT(3.141593*A*GAMNO)                                         UNR  400
    2 CONTINUE                                                          UNR  410
C                                                                       UNR  420
C     INTEGRATE OVER THE VALUES UNR(IU)                                 UNR  430
C                                                                       UNR  440
      DO 32 N=NEO,NEU                                                   UNR  450
        SIRA(N) = (UNR(N)/EN(N)+UNR(N-1)/EN(N-1)) * (EN(N-1)-EN(N)) / 2.UNR  460
   32 CONTINUE                                                          UNR  470
      DO 3 NN=1,NUNGR                                                   UNR  480
        IU = NEU + 1 - NN                                               UNR  490
        AUV = AUV + SIRA(IU)                                            UNR  500
    3 CONTINUE                                                          UNR  510
      RETURN                                                            UNR  520
C                                                                       UNR  530
C     DEFINITION OF CONSTANTS:                                          UNR  540
C       8.6164E-5     KAPPA                                             UNR  550
C       0.00095       DELTA XI                                          UNR  560
C       0.06164414    2.*SQRTF(0.00095)                                 UNR  570
C       0.69314718    LOGF(2.0)                                         UNR  580
C       11.512925     5.*LOGF(10.0)                                     UNR  590
C                                                                       UNR  600
      END                                                               UNR  610
      FUNCTION BARJ(AXI)                                                BAR   10
C                                                                       BAR   20
C     COMPUTES INTEGRAND OF FUNCTION JBAR                               BAR   30
C                                                                       BAR   40
      DIMENSION DATA(14),EN(68),UNR(68)                                 BAR   50
C                                                                       BAR   60
      COMMON /TUZGAM/ DATA,EN,UNR,A,ANS,AUV,B,EK,FIN,Q000FL,SIGP,STA,XILBAR   70
     1 ,IU,EO,NEU,NEO,NEOO                                              BAR   80
C                                                                       BAR   90
      EQUIVALENCE(AZERO,DATA(1)),(G,DATA(2)),(SIGPZ,DATA(3)),           BAR  100
     1 (TEMP,DATA(4)),(EC,DATA(5)),(D,DATA(6)),(GAMNO,DATA(7)),         BAR  110
     2 (GMGM,DATA(8)),(GEOM,DATA(9)),(ABAR,DATA(10)),(C,DATA(11)),      BAR  120
     3 (DZERO,DATA(12)),(SIGM1,DATA(13)),(SIGM2,DATA(14))               BAR  130
C                                                                       BAR  140
C                                                                       BAR  150
      AK = (ALOG(3.84184E-7*SIGP*EN(IU)*AXI/G/(AXI-B))+11.512925) /     BAR  160
     1 0.69314718                                                       BAR  170
C                                                                       BAR  180
      BARJ = (FJ(AXI,AK)/SQRT(AXI-B)) * EXP((B-AXI)/A/GAMNO)            BAR  190
C                                                                       BAR  200
      RETURN                                                            BAR  210
      END                                                               BAR  220
      FUNCTION FJ(AXI,AK)                                               FJ    10
C                                                                       FJ    20
C     INTERPOLATES FOR THE FUNCTION J(XI,K)                             FJ    30
C                                                                       FJ    40
      DIMENSION TXI(23),TK(44),TJ(23,44),AJ(2),AKA(2),TAB(4),P(161),    FJ    50
     1 Q(138),R(138),V(138),PA(138),QA(299)                             FJ    60
C                                                                       FJ    70
      EQUIVALENCE(TJ(1,1),P(1)),(TJ(1,8),Q(1)),(TJ(1,14),R(1)),         FJ    80
     1 (TJ(1,20),V(1)),(TJ(1,26),PA(1)),(TJ(1,32),QA(1))                FJ    90
C                                                                       FJ   100
      DATA TXI/0.0,0.005,0.010,0.015,0.02,0.03,0.04,0.05,0.10,0.15,0.20,FJ   110
     1 0.25,0.30,0.35,0.40,0.45,0.50,0.60,0.70,0.80,0.90,1.0,1.E10/     FJ   120
C                                                                       FJ   130
      DATA TK/4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,10.5,FJ   140
     1 11.0,11.5,12.0,12.5,13.0,13.5,14.0,14.5,15.0,15.5,16.0,16.5,17.0,FJ   150
     2 17.5,18.0,18.5,19.0,19.5,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,FJ   160
     3 28.0,29.0,30.0,31.0/                                             FJ   170
C                                                                       FJ   180
      DATA P/9817.5,728.97,400.72,280.31,238.6,185.6,161.3,150.1,130.7, FJ   190
     1 126.9,125.7,125.1,124.8,124.6,124.5,124.5,124.4,124.3,124.3,124.3FJ   200
     2 ,124.2,124.2,124.2,6942.0,689.89,383.03,269.10,222.9,170.4,145.5,FJ   210
     3 133.5,112.0,107.7,106.2,105.5,105.2,105.0,104.8,104.8,104.7,104.6FJ   220
     4 ,104.56,104.54,104.49,104.47,104.4,4908.7,645.41,364.49,257.40,  FJ   230
     5 208.6,157.3,132.4,119.8,96.67,91.68,89.93,89.14,88.72,88.47,88.31FJ   240
     6 ,88.20,88.12,88.02,87.96,87.92,87.90,87.88,87.80,3471.0,547.70,  FJ   250
     7 344.94,245.14,195.1,145.9,121.2,108.3,84.00,78.36,76.34,75.41,   FJ   260
     8 74.91,74.62,74.43,74.30,74.21,74.099,74.026,73.980,73.948,73.921,FJ   270
     9 73.82,2454.4,516.24,322.54,232.23,182.1,135.5,111.6,98.63,73.55, FJ   280
     X 67.33,65.01,63.93,63.35,63.01,62.78,62.63,62.52,62.38,62.30,62.25FJ   290
     Y ,62.21,62.18,62.07,1735.5,458.97,288.48,218.56,169.2,125.7,103.0,FJ   300
     Z 90.22,64.91,58.19,55.59,54.36,53.69,53.28,53.02,52.84,52.72,     FJ   310
     Z 52.568,52.569,52.407,52.360,52.324,52.19,1227.2,402.99,258.12,   FJ   320
     Z 195.96,156.3,116.4,95.08,82.73,57.72,50.63,47.77,46.38,45.62,    FJ   330
     Z 45.15,44.85,44.64,44.50,44.300,44.190,44.120,44.070,44.030,43.88/FJ   340
C                                                                       FJ   350
      DATA Q/867.75,347.70,229.49,175.51,143.1,107.3,87.61,75.88,51.66, FJ   360
     1 44.36,41.28,39.75,38.89,38.36,38.01,37.77,37.60,37.403,37.268,   FJ   370
     2 37.179,37.116,37.070,36.89,613.59,293.92,201.49,156.19,129.6,    FJ   380
     3 98.09,80.36,69.44,46.47,39.11,35.89,34.23,33.28,32.69,32.30,32.03FJ   390
     4 ,31.83,31.580,31.430,31.330,31.260,31.210,31.01,433.88,242.51,   FJ   400
     5 173.85,137.49,115.8,88.81,73.19,63.23,41.91,34.68,31.39,29.64,   FJ   410
     6 28.61,27.96,27.53,27.22,27.01,26.745,26.563,26.444,26.361,26.304,FJ   420
     7 26.06,306.80,195.42,146.96,119.01,101.8,79.40,66.00,57.13,37.81, FJ   430
     8 30.88,27.59,25.79,24.71,24.01,23.54,23.21,22.97,22.65,22.45,22.32FJ   440
     9 ,22.23,22.17,21.90,216.94,153.86,121.26,100.10,87.71,69.88,58.76,FJ   450
     X 51.07,34.03,27.54,24.35,22.55,21.44,20.71,20.21,19.85,19.59,     FJ   460
     Y 19.262,19.035,18.886,18.783,18.710,18.39,153.40,118.56,97.71,    FJ   470
     Z 83.66,73.99,60.37,51.50,45.04,30.45,24.54,21.53,19.77,18.67,17.92FJ   480
     Z ,17.41,17.03,16.75,16.38,16.14,15.98,15.87,15.79,15.44/          FJ   490
C                                                                       FJ   500
      DATA R/108.47,89.644,76.929,67.687,60.99,51.07,44.29,39.07,27.01, FJ   510
     1 21.79,19.03,17.37,16.29,15.56,15.04,14.66,14.37,13.985,13.730,   FJ   520
     2 13.562,13.439,13.352,12.96,76.699,66.712,59.282,53.497,49.09,    FJ   530
     3 42.23,37.30,33.25,23.67,19.21,16.76,15.23,14.23,13.52,13.01,12.64FJ   540
     4 ,12.35,11.94,11.68,11.51,11.38,11.29,10.87,54.234,49.015,44.822, FJ   550
     5 41.370,38.61,34.11,30.70,27.70,20.43,16.76,14.66,13.31,12.39,    FJ   560
     6 11.74,11.26,10.90,10.62,10.215,9.9546,9.7765,9.6447,9.5493,9.099,FJ   570
     7 38.350,35.653,33.356,31.373,29.73,26.91,24.68,22.57,17.31,14.42, FJ   580
     8 12.68,11.54,10.74,10.15,9.718,9.382,9.119,8.739,8.484,8.304,8.174FJ   590
     9 ,8.077,7.607,27.117,25.736,24.507,23.405,22.46,20.78,19.39,17.97,FJ   600
     X 14.36,12.19,10.83,9.888,9.217,8.718,8.337,8.039,7.802,7.4329,    FJ   610
     Y 7.1980,7.0291,6.9050,6.8113,6.345,19.175,18.472,17.826,17.230,   FJ   620
     Z 16.71,15.74,14.91,14.00,11.65,10.11,9.082,8.352,7.815,7.406,7.087FJ   630
     Z ,6.834,6.629,6.322,6.107,5.950,5.833,5.744,5.276/                FJ   640
C                                                                       FJ   650
      DATA V/13.559,13.203,12.868,12.552,12.27,11.74,11.26,10.70,9.240, FJ   660
     1 8.199,7.470,6.930,6.521,6.202,5.948,5.743,5.574,5.2855,5.1054,   FJ   670
     2 4.9715,4.8700,4.7915,4.369,9.5874,9.4080,9.2361,9.0714,8.922,    FJ   680
     3 8.634,8.370,8.033,7.173,6.507,6.015,5.637,5.342,5.106,4.914,4.756FJ   690
     4 ,4.624,4.419,4.268,4.154,4.066,3.997,3.597,6.7793,6.6890,6.6015, FJ   700
     5 6.5166,6.439,6.286,6.145,5.946,5.461,5.056,4.740,4.489,4.287,    FJ   710
     6 4.122,3.985,3.870,3.773,3.5866,3.4729,3.3854,3.3170,3.2625,2.940,FJ   720
     7 4.7937,4.7484,4.7040,4.6606,4.620,4.541,4.467,4.351,4.088,3.853, FJ   730
     8 3.659,3.501,3.371,3.261,3.169,3.090,3.022,2.911,2.826,2.759,2.706FJ   740
     9 ,2.663,2.381,3.3897,3.3669,3.3445,3.3225,3.302,3.261,3.223,3.155,FJ   750
     X 3.018,2.887,2.773,2.678,2.598,2.529,2.470,2.418,2.373,2.2769,    FJ   760
     Y 2.2194,2.1733,2.1366,2.1063,1.907,2.3968,2.3855,2.3742,2.3630,   FJ   770
     Z 2.352,2.332,2.312,2.272,2.204,2.133,2.068,2.013,1.966,1.925,1.889FJ   780
     Z ,1.857,1.829,1.781,1.743,1.712,1.687,1.666,1.508/                FJ   790
C                                                                       FJ   800
      DATA PA/1.6948,1.6891,1.6835,1.6778,1.673,1.662,1.652,1.628,1.596,FJ   810
     1 1.558,1.522,1.492,1.465,1.441,1.420,1.402,1.385,1.3452,1.3215,   FJ   820
     2 1.3021,1.2861,1.2727,1.175,1.1984,1.1956,1.1927,1.1899,1.187,    FJ   830
     3 1.182,1.177,1.162,1.148,1.129,1.109,1.092,1.078,1.065,1.053,1.042FJ   840
     4 ,1.033,1.016,1.002,0.9904,0.9805,0.9722,0.9026,0.84741,0.84598,  FJ   850
     5 0.84456,0.84314,0.8419,0.8393,0.8369,0.8276,0.8217,0.8122,0.8017,FJ   860
     6 0.7927,0.7849,0.7778,0.7713,0.7655,0.7601,0.74671,0.73864,0.73185FJ   870
     7 ,0.72603,0.72106,0.6830,0.59921,0.59850,0.59778,0.59707,0.5964,  FJ   880
     8 0.5952,0.5940,0.5881,0.5862,0.5817,0.5760,0.5712,0.5671,0.5633,  FJ   890
     9 0.5599,0.5567,0.5539,0.5488,0.5445,0.5408,0.5376,0.5348,0.5098,  FJ   900
     X 0.42371,0.42335,0.42299,0.42264,0.4223,0.4217,0.4212,0.4173,     FJ   910
     Y 0.4171,0.4150,0.4120,0.4095,0.4073,0.4054,0.4036,0.4019,0.4004,  FJ   920
     Z 0.39639,0.3940,0.3919,0.39012,0.38855,0.3760,0.29961,0.29943,    FJ   930
     Z 0.29925,0.29907,0.2989,0.2986,0.2984,0.2978,0.2963,0.2954,0.2937,FJ   940
     Z 0.2924,0.2913,0.2903,0.2894,0.2885,0.2877,0.2863,0.2851,0.2840,  FJ   950
     Z 0.2831,0.2823,0.2746/                                            FJ   960
C                                                                       FJ   970
      DATA QA/0.21185,0.21176,0.21167,0.21159,0.2115,0.2114,0.2113,     FJ   980
     1 0.2112,0.2102,0.2098,0.2089,0.2082,0.2077,0.2071,0.2067,0.2062,  FJ   990
     2 0.2058,0.20473,0.20409,0.20349,0.20296,0.20253,0.1989,0.14980,   FJ  1000
     3 0.14976,0.14971,0.14967,0.1496,0.1496,0.1495,0.1493,0.1490,0.1489FJ  1010
     4 ,0.1483,0.1480,0.1477,0.1474,0.1472,0.1470,0.1468,0.1464,0.1461, FJ  1020
     5 0.1458,0.1455,0.1453,0.1431,0.074901,0.074890,0.074879,0.0744868,FJ  1030
     6 14*0.07403,0.07395,0.07388,0.07381,0.07375,0.07375,0.037451,     FJ  1040
     7 0.037448,0.037445,0.037442,14*0.03723,0.03721,0.03719,0.03718,   FJ  1050
     8 0.03716,0.03716,2*0.018725,0.018724,0.018723,15*0.01867,2*0.01866FJ  1060
     9 ,2*0.01865,0.0093627,0.0093625,0.0093623,0.0093622,14*0.009349,  FJ  1070
     X 0.009348,0.009346,0.009345,0.009344,0.009344,3*0.0046813,        FJ  1080
     Y 0.0046812,15*0.004678,4*0.004677,3*0.0023407,0.0023406,19*0.00234FJ  1090
     Z ,4*0.0011703,19*0.00117,4*0.00058517,19*0.0005851,23*0.0002926,23FJ  1100
     Z *0.0001463,18*0.00007315,5*0.00007314/                           FJ  1110
C                                                                       FJ  1120
C                                                                       FJ  1130
C     LENGTHS OF XI AND K TABLES                                        FJ  1140
C                                                                       FJ  1150
      LXI = 23                                                          FJ  1160
      LK = 44                                                           FJ  1170
C                                                                       FJ  1180
C     POSITION ARGUMENTS OF FUNCTION J(XI,K)                            FJ  1190
C     LOCATION OF STARTING ARGUMENT XI(I)                               FJ  1200
C                                                                       FJ  1210
      DO 1 IXI=1,LXI                                                    FJ  1220
        IF(TXI(IXI)-AXI) 1,2,2                                          FJ  1230
    1 CONTINUE                                                          FJ  1240
    2 IF(1+(LXI+IXI-6)/(4-LXI)) 3,4,5                                   FJ  1250
    3 IXI = LXI - 3                                                     FJ  1260
      GOTO 5                                                            FJ  1270
    4 IXI = IXI - 1                                                     FJ  1280
C                                                                       FJ  1290
C     LOCATION OF STARTING ARGUMENT K(I)                                FJ  1300
C                                                                       FJ  1310
    5 DO 8 KAK=1,LK                                                     FJ  1320
        IF(AK-TK(KAK)) 6,6,8                                            FJ  1330
    6   IF(1-KAK) 7,9,9                                                 FJ  1340
    8 CONTINUE                                                          FJ  1350
      KAK = LK - 1                                                      FJ  1360
      GOTO 9                                                            FJ  1370
    7 KAK = KAK - 1                                                     FJ  1380
C                                                                       FJ  1390
C     INTERPOLATE FOR J(AXI,K(KAK)),J(AXI,K(KAK+1))                     FJ  1400
C     FOUR POINT DIVIDED DIFFERENCE INTERPOLATION                       FJ  1410
C                                                                       FJ  1420
    9 DO 13 I=1,2                                                       FJ  1430
        J = I + KAK - 1                                                 FJ  1440
        AKA(I) = TK(J)                                                  FJ  1450
        DO 10 K=1,4                                                     FJ  1460
          L = K + IXI - 1                                               FJ  1470
          TAB(K) = TJ(L,J)                                              FJ  1480
   10   CONTINUE                                                        FJ  1490
        AJ(I) = TAB(1)                                                  FJ  1500
        DO 12 J=1,3                                                     FJ  1510
          K = 4 - J                                                     FJ  1520
          DO 11 L=1,K                                                   FJ  1530
            M = 5 - L                                                   FJ  1540
            IA = J + IXI - 1                                            FJ  1550
            IB = M + IXI - 1                                            FJ  1560
            IC = K + IXI - L                                            FJ  1570
            TAB(M) = (TAB(M)-TAB(M-1)) * (AXI-TXI(IA)) / (TXI(IB)-      FJ  1580
     1       TXI(IC))                                                   FJ  1590
   11     CONTINUE                                                      FJ  1600
          AJ(I) = AJ(I) + TAB(J+1)                                      FJ  1610
   12   CONTINUE                                                        FJ  1620
   13 CONTINUE                                                          FJ  1630
C                                                                       FJ  1640
C     INTERPOLATE FOR J(AXI,AK)                                         FJ  1650
C     ASSUME J(AXI,K)=C1*EXPF(-C2*K) FROM K(KAK) TO K(KAK+1)            FJ  1660
C                                                                       FJ  1670
      FJ = AJ(1) * EXP((AKA(1)-AK)*ALOG(AJ(2)/AJ(1))/(AKA(1)-AKA(2)))   FJ  1680
C                                                                       FJ  1690
      RETURN                                                            FJ  1700
      END                                                               FJ  1710
      SUBROUTINE SIMPZ(A,B,FUNC,ANS,ERR)                                MPZ   10
C                                                                       MPZ   20
      COMMON /UI/ IDUM(9),DU(2),PREV                                    MPZ   30
C                                                                       MPZ   40
C                                                                       MPZ   50
      NDIV = 1                                                          MPZ   60
      SONE = (B-A) * (FUNC(A)+FUNC(B)) / 2.                             MPZ   70
   10 NDIV = 2 * NDIV                                                   MPZ   80
      STWO = 0.                                                         MPZ   90
      DEL = (B-A) / FLOAT(NDIV)                                         MPZ  100
      DO 20 I=1,NDIV,2                                                  MPZ  110
        X = A + DEL * FLOAT(I)                                          MPZ  120
        STWO = STWO + FUNC(X)                                           MPZ  130
   20 CONTINUE                                                          MPZ  140
      CUR = SONE + 4. * DEL * STWO                                      MPZ  150
      IF(ERR*ABS(CUR)-ABS(CUR-PREV)) 30,40,40                           MPZ  160
   30 PREV = CUR                                                        MPZ  170
      SONE = (SONE+CUR) / 4.                                            MPZ  180
      GOTO 10                                                           MPZ  190
   40 ANS = CUR / 3.                                                    MPZ  200
      RETURN                                                            MPZ  210
      END                                                               MPZ  220
      SUBROUTINE GRUGA                                                  GRU   10
C                                                                       GRU   20
      DIMENSION W(20),U(5),RRR(5),FX(5),DATE(2),FLAG(16),HEAD(24),      GRU   30
     1 IDEN(2),IVAL(3),MESH(500),REASON(32),TFLAG(16),UNIT(2),AZERO(500)GRU   40
     2 ,G(500),SIGPZ(500),TEMP(500),EZERO(500),GAMN(500),GMGM(500),     GRU   50
     3 R(500),S(500),SOLVE(50),ABAR(50),C(50),DZERO(50),AMOD1(50),      GRU   60
     4 SIGM1(50),AMOD2(50),SIGM2(50),ALPHA(3),AMU(505),CHI(505),        GRU   70
     5 DRI(1000),EONE(500),EPSIL(500),ETA(5),F(501),GAMMA(500),PSI(505),GRU   80
     6 RI(1000),SIGAZ(505),SIGOH(500),SIGSZ(505),SUB(501),TERM(3),X(505)GRU   90
     7 ,XI(500),INDEX(1000,4),BVAL(1001,3),FIELD(10),VAPS(501),FIRA(69),GRU  100
     8 SISQ(69)                                                         GRU  110
C                                                                       GRU  120
      COMMON /FLUSS/ FUB(501),VAZVG                                     GRU  130
C                                                                       GRU  140
      COMMON U,RRR,FX,LINE,KU,NEND,W,DATE,FLAG,HEAD,IDEN,INDEX          GRU  150
C                                                                       GRU  160
      COMMON IVAL,MESH,REASON,TFLAG,UNIT,I,LCP000,ICNN,ICON,IDEL        GRU  170
C                                                                       GRU  180
      COMMON IEND,IEXM,IGEOM,IMOD1,IMOD2,IOUT,IRSV,IRVN,IRVO            GRU  190
C                                                                       GRU  200
      COMMON IRVT,JC,K,MAIN,MCON,MIBV,MMSH,MRSV,NAIN,NINP,NNXT          GRU  210
C                                                                       GRU  220
      COMMON NOUT,NOXT,AZERO,G,SIGPZ,TEMP,EZERO,GAMN,GMGM,R,S           GRU  230
C                                                                       GRU  240
      COMMON SOLVE,ABAR,C,DZERO,AMOD1,SIGM1,AMOD2,SIGM2,ALPHA           GRU  250
C                                                                       GRU  260
      COMMON AMU,BVAL,CHI,DRI,EONE,EPSIL,ETA,F,GAMMA,PSI,RI,SIGAZ       GRU  270
C                                                                       GRU  280
      COMMON SIGOH,SIGSZ,SUB,TERM,X,XI,ALFA,ARG,BETA,BETAP,COEFT        GRU  290
C                                                                       GRU  300
      COMMON GAM,GAMM,GAMP,PC,PPSI,PZERO,RBAR,SGM1,SGM2,SIGM            GRU  310
C                                                                       GRU  320
      COMMON SIGT,SRI,TRI,XONE,ZONE                                     GRU  330
C                                                                       GRU  340
      COMMON /GAGRU/ EGO(69),SIRA(69),FF,FL,M6,NGGR,NGGR1,EOE,EERSTE,   GRU  350
     1 ELETZT,IO,IU,RIJS,MMEES,IIII,JRJR,ITUZ,IGRUG,M5,NEUI,NALTI,      GRU  360
     2 UAPS(501)                                                        GRU  370
C                                                                       GRU  380
      COMMON /DIR25/ NAE25,NEI25,LIBSAE,KENNNR,LENNNR,IFALNR,NUCLID,    GRU  390
     1 INHALT(34,2),SISP(68),LOPUTZ(9),MAXSAE,LIBIN                     GRU  400
C                                                                       GRU  410
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     GRU  420
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   GRU  430
     2 ESIG2,EDIQU2,KI,IEING5,TESTA,ENERGU,ENERGO,NNRESO,NNDATA,NNSIGA, GRU  440
     3 JI,DENHOM                                                        GRU  450
C                                                                       GRU  460
      COMMON /ENDIV/ NWING0                                             GRU  470
C                                                                       GRU  480
      COMMON /UI/ IDUM(9),DUM(3),IGRUP,NA25,NEUJ                        GRU  490
C                                                                       GRU  500
   32 FORMAT (///' NO GAM-GROUP RESONANCE-SIGM. HAVE BEEN CALCULATED')  GRU  510
   34 FORMAT (//' FIRST AND LAST CALCULATED RESONANCE AT',2E15.6,' EV'/)GRU  520
   56 FORMAT (10(10(' / ',I6,',',I3)/))                                 GRU  530
   57 FORMAT (///' UNIT',I4,' CONTAINS FOLLOWING SETS (SET-NO., NUCLIDE/GRU  540
     1.....)')                                                          GRU  550
   58 FORMAT ('1 ON UNIT',I3,' SET NO. ',I4,' FOR CASE NO.',I7,', NUCLIDGRU  560
     1E-NO.',I4,' FOR ',E12.5,' KELVIN IS STORED:'/)                    GRU  570
   59 FORMAT (/' RESONANCE INTEGRAL IN GAM-GROUPS'/'  GRP.  UP.ENERG.LIMGRU  580
     1IT   RES.SIGM.       RES.INT.'//68(I5,E14.6,2E16.6/))             GRU  590
  100 FORMAT (///' DIRECT-ACCESS-UNIT',I4,' WITH IDENTIFICATION NO',I6,'GRU  600
     1 IS USED FOR STORING IN GAM-GROUPS'/)                             GRU  610
  101 FORMAT (' INPUT SPECIFIES IDENTIFICATION NO.',I6,'. --> **STOP**')GRU  620
  102 FORMAT (///' ON DIRECT-ACCESS-UNIT',I4,' ALL',I5,' SETS ARE OCCUPIGRU  630
     1ED.'/' CLEAN SOME OLD SETS. **STOP**')                            GRU  640
  103 FORMAT ('1 ON SET NO.',I4,' FOR IFALNO ',I6,', NUCLIDE',I5,' ALREAGRU  650
     1DY FOLLOWING RES.SIGMAS ARE EXISTING (GRP., RES.SIGM...)'/' THE NEGRU  660
     2W CALCULATED ONES WILL BE ADDED'/)                                GRU  670
  104 FORMAT (4(I4,E15.6,6X))                                           GRU  680
 1000 FORMAT (/' SUM:  ',24X,E16.6/)                                    GRU  690
 1001 FORMAT (/' SUM:  ',28X,E16.6/)                                    GRU  700
 1011 FORMAT (/' VOLUME OF ABSORBER / VOLUME OF CELL =',F7.4)           GRU  710
11110 FORMAT ('1GRP.      EO      -      EU          RES.ABS.      FLUX GRU  720
     1INT.'/68(I5,E13.6,'-',E13.6,2E15.6/))                             GRU  730
C                                                                       GRU  740
C                                                                       GRU  750
      IF(LENNNR .GT. 0) GOTO 1                                          GRU  760
C                                                                       GRU  770
      CALL DIRA25                                                       GRU  780
C                                                                       GRU  790
      GOTO 99                                                           GRU  800
    1 CONTINUE                                                          GRU  810
      NGGR = 68                                                         GRU  820
      NGGR1 = NGGR + 1                                                  GRU  830
      EERSTE = 0.                                                       GRU  840
      IU = NGGR                                                         GRU  850
      DO 10 N=1,NGGR1                                                   GRU  860
        SIRA(N) = 0.                                                    GRU  870
        FIRA(N) = 0.                                                    GRU  880
        EGO(N) = 1.0E7 / EXP(FLOAT(N-1)/4.)                             GRU  890
   10 CONTINUE                                                          GRU  900
      IF(NEI25 .NE. 30) GOTO 70                                         GRU  910
      NAE25 = 1                                                         GRU  920
      READ (NEI25,REC=NAE25) KENNNR,MAXSAE,LIBIN,LIBSAE,NGGR            GRU  930
      NAE25 = NAE25 + 1                                                 GRU  940
      WRITE (M6,100) NEI25,KENNNR                                       GRU  950
      IF(KENNNR .EQ. LENNNR) GOTO 11                                    GRU  960
      WRITE (M6,101) LENNNR                                             GRU  970
      STOP                                                              GRU  980
   11 CONTINUE                                                          GRU  990
C                                                                       GRU 1000
C     EVENTUELL LOESCHEN EINIGER SAETZE                                 GRU 1010
C                                                                       GRU 1020
      LOP = 0                                                           GRU 1030
      DO 12 LO=1,9                                                      GRU 1040
        LOP = LOP + LOPUTZ(LO)                                          GRU 1050
   12 CONTINUE                                                          GRU 1060
      IF(LOP .EQ. 0) GOTO 17                                            GRU 1070
      DO 13 I=1,LIBSAE                                                  GRU 1080
        READ (NEI25,REC=NAE25) ((INHALT(L,J),L=1,LIBIN),J=1,2)          GRU 1090
        DO 14 J=1,LIBIN                                                 GRU 1100
          DO 15 LO=1,9                                                  GRU 1110
            IF(INHALT(J,1) .NE. LOPUTZ(LO)) GOTO 15                     GRU 1120
            DO 16 L=1,2                                                 GRU 1130
              INHALT(J,L) = 0                                           GRU 1140
   16       CONTINUE                                                    GRU 1150
   15     CONTINUE                                                      GRU 1160
   14   CONTINUE                                                        GRU 1170
        WRITE (NEI25,REC=NAE25) ((INHALT(L,J),L=1,LIBIN),J=1,2)         GRU 1180
        NAE25 = NAE25 + 1                                               GRU 1190
   13 CONTINUE                                                          GRU 1200
      NAE25 = 2                                                         GRU 1210
   17 CONTINUE                                                          GRU 1220
C                                                                       GRU 1230
C     WOHIN WIRD VERBUCHT?                                              GRU 1240
C                                                                       GRU 1250
      NEUI = 0                                                          GRU 1260
      NALTI = 0                                                         GRU 1270
      NIBIN = LIBIN                                                     GRU 1280
      DO 71 I=1,LIBSAE                                                  GRU 1290
        READ (NEI25,REC=NAE25) ((INHALT(L,K),L=1,NIBIN),K=1,2)          GRU 1300
        NAE25 = NAE25 + 1                                               GRU 1310
        DO 72 J=1,LIBIN                                                 GRU 1320
          IF(INHALT(J,1) .NE. IFALNR) GOTO 73                           GRU 1330
          IF(INHALT(J,2) .NE. NUCLID) GOTO 73                           GRU 1340
          NALTI = I                                                     GRU 1350
          NALTJ = J                                                     GRU 1360
          NAE25 = 1 + LIBSAE + (NALTI-1) * LIBIN + NALTJ                GRU 1370
          READ (NEI25,REC=NAE25) (SISP(N),N=1,NGGR)                     GRU 1380
          WRITE (M6,103) NAE25,IFALNR,NUCLID                            GRU 1390
          DO 105 N1=1,17                                                GRU 1400
            N2 = N1 + 17                                                GRU 1410
            N3 = N2 + 17                                                GRU 1420
            N4 = N3 + 17                                                GRU 1430
            WRITE (M6,104) N1,SISP(N1),N2,SISP(N2),N3,SISP(N3),N4,      GRU 1440
     1       SISP(N4)                                                   GRU 1450
  105     CONTINUE                                                      GRU 1460
          GOTO 70                                                       GRU 1470
   73     CONTINUE                                                      GRU 1480
          IF(NEUI .NE. 0) GOTO 72                                       GRU 1490
          IF(INHALT(J,1) .NE. 0) GOTO 72                                GRU 1500
          NEUI =  I                                                     GRU 1510
          NEUJ =  J                                                     GRU 1520
          NA25  = 1 + LIBSAE + (NEUI-1) * LIBIN + NEUJ                  GRU 1530
   72   CONTINUE                                                        GRU 1540
   71 CONTINUE                                                          GRU 1550
      NAE25 = NA25                                                      GRU 1560
      DO 2 I=1,NGGR                                                     GRU 1570
        SISP(I) = 0.                                                    GRU 1580
    2 CONTINUE                                                          GRU 1590
      IF(NEUI .NE. 0) GOTO 70                                           GRU 1600
      WRITE (M6,102) NEI25,MAXSAE                                       GRU 1610
      STOP                                                              GRU 1620
   70 CONTINUE                                                          GRU 1630
      GOTO 99                                                           GRU 1640
C                                                                       GRU 1650
      ENTRY GRUGA1                                                      GRU 1660
C                                                                       GRU 1670
C     SIMPSON-INTEGRATION                                               GRU 1680
C                                                                       GRU 1690
      IF(EERSTE .EQ. 0.) EERSTE = ELETZT                                GRU 1700
      SIJS = 0.                                                         GRU 1710
      RIJS = 0.                                                         GRU 1720
      SIFJS = 0.                                                        GRU 1730
      RIFJS = 0.                                                        GRU 1740
      EOE = EOE / 3.                                                    GRU 1750
      DO 30 NJS=1,MMEES                                                 GRU 1760
        UAPS(NJS) = 0.                                                  GRU 1770
        VAPS(NJS) = 0.                                                  GRU 1780
   30 CONTINUE                                                          GRU 1790
      DO 130 NJS=3,MMEES,2                                              GRU 1800
        FIPS = SUB(NJS) + SUB(NJS-2) + 4. * SUB(NJS-1)                  GRU 1810
        FFIPS = FUB(NJS) + FUB(NJS-2) + 4. * FUB(NJS-1)                 GRU 1820
        NJT = NJS - 1                                                   GRU 1830
        UAPS(NJT) = FIPS                                                GRU 1840
        VAPS(NJT) = FFIPS                                               GRU 1850
  130 CONTINUE                                                          GRU 1860
      MIMES = (MMEES-1) / 2                                             GRU 1870
      DO 132 I1=1,MIMES                                                 GRU 1880
        I2 = MMEES + 1 - I1                                             GRU 1890
        FIPS = UAPS(I1) + UAPS(I2)                                      GRU 1900
        SIJS = SIJS + FIPS                                              GRU 1910
  132 CONTINUE                                                          GRU 1920
      FIPS = UAPS(MIMES+1)                                              GRU 1930
      SIJS = SIJS + FIPS                                                GRU 1940
      RIJS = SIJS * EOE                                                 GRU 1950
      GOTO 99                                                           GRU 1960
C                                                                       GRU 1970
      ENTRY GRUGA2                                                      GRU 1980
C                                                                       GRU 1990
C     FLUEGEL-KORREKTUR UND VERBUCHEN AUF GAM-GRUPPEN                   GRU 2000
C                                                                       GRU 2010
      IF(IGRUG .EQ. 0) GOTO 99                                          GRU 2020
      I = IIII                                                          GRU 2030
      JR = JRJR                                                         GRU 2040
      IF(NWING0 .GT. 0) DRI(JR) = 0.                                    GRU 2050
      UAPS(1) = UAPS(1) + (DRI(JR)*FF/EOE)                              GRU 2060
      UAPS(MMEES) = UAPS(MMEES) + (DRI(JR)*FL/EOE)                      GRU 2070
      IGR1 = 1                                                          GRU 2080
      GIPS = 0.                                                         GRU 2090
      FIPS = 0.                                                         GRU 2100
      UONF = ALOG(1.0E7/EONE(I)) * 4.                                   GRU 2110
      U1 = UONF                                                         GRU 2120
      EPS4 = 4. * EPSIL(I)                                              GRU 2130
      DO 40 NME=1,MMEES                                                 GRU 2140
        UONE = UONF + (NME-1) * EPS4                                    GRU 2150
        IGRUP = IFIX(UONE) + 1                                          GRU 2160
        IF(IGR1 .EQ. IGRUP) GOTO 41                                     GRU 2170
        DU = (UONE-U1) / 4.                                             GRU 2180
        SIRA(IGR1) = SIRA(IGR1) + GIPS * EOE                            GRU 2190
        FIRA(IGR1) = FIRA(IGR1) + FIPS * EOE - DU                       GRU 2200
        GIPS = 0.                                                       GRU 2210
        FIPS = 0.                                                       GRU 2220
        IGR1 = IGRUP                                                    GRU 2230
        U1 = UONE                                                       GRU 2240
   41   CONTINUE                                                        GRU 2250
        GIPS = GIPS + UAPS(NME)                                         GRU 2260
        FIPS = FIPS + VAPS(NME)                                         GRU 2270
   40 CONTINUE                                                          GRU 2280
      DU = (UONE-U1) / 4.                                               GRU 2290
      SIRA(IGRUP) = SIRA(IGRUP) + GIPS * EOE                            GRU 2300
      FIRA(IGRUP) = FIRA(IGRUP) + FIPS * EOE - DU                       GRU 2310
      GOTO 99                                                           GRU 2320
C                                                                       GRU 2330
      ENTRY GRUGA3                                                      GRU 2340
C                                                                       GRU 2350
C     WICHTIGE GAM-GRUPPEN HERAUSSUCHEN                                 GRU 2360
C                                                                       GRU 2370
C     SIRA() =  UNKORR. RI                                              GRU 2380
C     SISQ() =  MIT FLUSSVERH. KORR. WQ                                 GRU 2390
C     SISP() =  MIT FLUSSVERH. KORR. RI                                 GRU 2400
C                                                                       GRU 2410
      DU = 0.25                                                         GRU 2420
      DO 35 I=1,NGGR                                                    GRU 2430
        IO = I                                                          GRU 2440
        IF(SIRA(I) .NE. 0.) GOTO 31                                     GRU 2450
   35 CONTINUE                                                          GRU 2460
      WRITE (M6,32)                                                     GRU 2470
      GOTO 99                                                           GRU 2480
   31 CONTINUE                                                          GRU 2490
      DO 33 I=IO,NGGR                                                   GRU 2500
        IF(SIRA(I) .NE. 0.) IU = I                                      GRU 2510
   33 CONTINUE                                                          GRU 2520
      SIRHAN = 0.                                                       GRU 2530
      DO 60 I=IO,IU                                                     GRU 2540
        FIRA(I) = DU + FIRA(I) * VAZVG                                  GRU 2550
   60 CONTINUE                                                          GRU 2560
      WRITE (M6,11110) (N,EGO(N),EGO(N+1),SIRA(N),FIRA(N),N=IO,IU)      GRU 2570
      DO 38 I=1,NGGR                                                    GRU 2580
        SIRHAN = SIRHAN + SIRA(I)                                       GRU 2590
   38 CONTINUE                                                          GRU 2600
      WRITE (M6,1000) SIRHAN                                            GRU 2610
      WRITE (M6,34) EERSTE,ELETZT                                       GRU 2620
      DO 61 I=IO,IU                                                     GRU 2630
        SISQ(I) = SIRA(I) / FIRA(I)                                     GRU 2640
   61 CONTINUE                                                          GRU 2650
      TOT = 0.                                                          GRU 2660
      DO 51 I=1,NGGR                                                    GRU 2670
        SISP(I) = SISQ(I) * DU                                          GRU 2680
        TOT = TOT + SISP(I)                                             GRU 2690
   51 CONTINUE                                                          GRU 2700
      IF(NEI25 .NE. 30) GOTO 52                                         GRU 2710
      NA25 = NAE25                                                      GRU 2720
      WRITE (NEI25,REC=NAE25) (SISP(I),I=1,NGGR),ETEMP,DENHOM           GRU 2730
      NAE25 = NAE25 + 1                                                 GRU 2740
      IF(NALTI .GT. 0) GOTO 54                                          GRU 2750
      NAE25 = NEUI + 1                                                  GRU 2760
      READ (NEI25,REC=NAE25) ((INHALT(I,J),I=1,LIBIN),J=1,2)            GRU 2770
      INHALT(NEUJ,1) = IFALNR                                           GRU 2780
      INHALT(NEUJ,2) = NUCLID                                           GRU 2790
      WRITE (NEI25,REC=NAE25) ((INHALT(I,J),I=1,LIBIN),J=1,2)           GRU 2800
   54 CONTINUE                                                          GRU 2810
      NAE25 = 2                                                         GRU 2820
      WRITE (M6,58) NEI25,NA25,IFALNR,NUCLID,ETEMP                      GRU 2830
   52 CONTINUE                                                          GRU 2840
      WRITE (M6,1011) VAZVG                                             GRU 2850
      WRITE (M6,59) (I,EGO(I),SISQ(I),SISP(I),I=1,NGGR)                 GRU 2860
      WRITE (M6,1001) TOT                                               GRU 2870
      IF(NEI25 .NE. 30) GOTO 99                                         GRU 2880
C                                                                       GRU 2890
C     WAS IST AUF DER EINHEIT DRAUF?                                    GRU 2900
C                                                                       GRU 2910
      WRITE (M6,57) NEI25                                               GRU 2920
      DO 53 K=1,LIBSAE                                                  GRU 2930
        READ (NEI25,REC=NAE25) ((INHALT(L,J),L=1,LIBIN),J=1,2)          GRU 2940
        NAE25 = NAE25 + 1                                               GRU 2950
        WRITE (M6,56) ((INHALT(I,J),J=1,2),I=1,LIBIN)                   GRU 2960
   53 CONTINUE                                                          GRU 2970
   99 RETURN                                                            GRU 2980
      END                                                               GRU 2990
      SUBROUTINE DIRA25                                                 DIR   10
C                                                                       DIR   20
      DIMENSION K(3)                                                    DIR   30
C                                                                       DIR   40
      COMMON /DIR25/ NAE25,NEI25,LIBSAE,KENNNR,LENNNR,IFALNR,NUCLID,    DIR   50
     1 INHALT(34,2),SISP(68),LOPUTZ(9)                                  DIR   60
C                                                                       DIR   70
      COMMON /GAM/ ID(4),IDNUCL                                         DIR   80
C                                                                       DIR   90
    9 FORMAT (10I2)                                                     DIR  100
   10 FORMAT (///'  A NEW ZUT->GAM-DATA SET IN DIRECT ACCESS SHALL BE CRDIR  110
     1EATED ON UNIT',I4/' IDENTIFICATION NO=',I7,', MAX. NUMBER OF SETS'DIR  120
     2 ,I5)                                                             DIR  130
   12 FORMAT (' ATTENTION, FOR GENERATING A NEW UNIT 30 A SPECIAL TRIC IDIR  140
     1S REQUIRED:'/' AFTER CARD Z19 CARD Z20 WITH (K(I),I=1,3), FORMAT(3DIR  150
     2I2), MUST BE READ.'/' ALL K(I) MUST BE = -1 .'/' PLEASE ADD THIS CDIR  160
     3ARD')                                                             DIR  170
   15 FORMAT (' SET NO.',I6,' IS WRITTEN,  NAE25=',I6)                  DIR  180
C                                                                       DIR  190
C                                                                       DIR  200
      NAE25 = 1                                                         DIR  210
      M6 = 6                                                            DIR  220
      LIBIN = 34                                                        DIR  230
      LIBSAE = 5                                                        DIR  240
      NGGR = 68                                                         DIR  250
      MAXSAE = IDNUCL                                                   DIR  260
      KENNNR = IFALNR                                                   DIR  270
      WRITE (M6,10) NEI25,KENNNR,MAXSAE                                 DIR  280
      DO 14 I=1,3                                                       DIR  290
        K(I) = -1                                                       DIR  300
   14 CONTINUE                                                          DIR  310
      WRITE (NEI25,REC=NAE25) KENNNR,MAXSAE,LIBIN,LIBSAE,NGGR           DIR  320
      NAE25 = NAE25 + 1                                                 DIR  330
      DO 1 I=1,LIBIN                                                    DIR  340
        DO 1 J=1,2                                                      DIR  350
          INHALT(I,J) = 0                                               DIR  360
    1 CONTINUE                                                          DIR  370
      DO 2 I=1,NGGR                                                     DIR  380
        SISP(I) = 0.                                                    DIR  390
    2 CONTINUE                                                          DIR  400
      DO 3 L=1,LIBSAE                                                   DIR  410
        WRITE (NEI25,REC=NAE25) ((INHALT(I,J),I=1,LIBIN),J=1,2)         DIR  420
        NAE25 = NAE25 + 1                                               DIR  430
    3 CONTINUE                                                          DIR  440
      DO 4 M=1,MAXSAE                                                   DIR  450
        WRITE (NEI25,REC=NAE25) (SISP(I),I=1,NGGR)                      DIR  460
        WRITE (M6,15) M,NAE25                                           DIR  470
        NAE25 = NAE25 + 1                                               DIR  480
    4 CONTINUE                                                          DIR  490
      RETURN                                                            DIR  500
      END                                                               DIR  510
      SUBROUTINE EINGAB                                                 EIN   10
C                                                                       EIN   20
      DIMENSION FIELD(10)                                               EIN   30
C                                                                       EIN   40
      CHARACTER*8 MODE,MODE1                                            EIN   50
C                                                                       EIN   60
C                                                                       EIN   70
      COMMON /FLUSS/ FUB(501),VAZVG                                     EIN   80
C                                                                       EIN   90
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     EIN  100
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   EIN  110
     2 ESIG2,EDIQU2,KI,IEING5,TESTA,ENERGU,ENERGO,NNRESO,NNDATA,NNSIGA, EIN  120
     3 JI                                                               EIN  130
C                                                                       EIN  140
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          EIN  150
C                                                                       EIN  160
      COMMON /DATA2/ NEI29,NXT29,FUTYP,DATA(14),IWOHIN,FTYP             EIN  170
C                                                                       EIN  180
      COMMON /GAGRU/ EGO(69),SIRA(69),FF,FL,M6,NGGR,NGGR1,EOE,EERSTE,   EIN  190
     1 ELETZT,IO,IU,RIJS,MMEES,IIII,JRJR,ITUZ,IGRUG,M5,NEUI,NALTI,      EIN  200
     2 UAPS(501),N5                                                     EIN  210
C                                                                       EIN  220
      COMMON /ENDIV/ NWING0                                             EIN  230
C                                                                       EIN  240
      COMMON /CL/ A                                                     EIN  250
C                                                                       EIN  260
   30 FORMAT (//10X,' FROM ZDATA2 TRANSFERRED ZUT-VALUES ACCORDING TO CAEIN  270
     1RD *Z11* :'/11X,60('=')/10X,' ABAR   = ',E12.5//)                 EIN  280
   31 FORMAT (//10X,' FROM ZDATA2 TRANSFERRED ZUT-VALUES ACCORDING TO CAEIN  290
     1RD *Z12* :'/11X,60('=')/10X,' EDZERO = ',E12.5/10X,' EDIQU1 = ',  EIN  300
     2 E12.5/10X,' EDIQU2 = ',E12.5//)                                  EIN  310
   32 FORMAT (//10X,' FROM ZDATA2 TRANSFERRED ZUT-VALUES ACCORDING TO CAEIN  320
     1RD *Z13* :'/11X,60('=')/10X,' R1     = ',E12.5/10X,' R2     = ',  EIN  330
     2 E12.5/10X,' R4     = ',E12.5/10X,' R5     = ',E12.5/10X,' F      EIN  340
     3= ',E12.5/10X,' H      = ',E12.5//)                               EIN  350
   33 FORMAT (//10X,' FROM ZDATA2 TRANSFERRED ZUT-VALUES ACCORDING TO CAEIN  360
     1RD *Z14* :'/11X,60('=')/10X,' SI2    = ',E12.5/10X,' SI4    = ',  EIN  370
     2 E12.5/10X,' SI5    = ',E12.5/10X,' ALPH   = ',E12.5//)           EIN  380
   40 FORMAT (6E12.5)                                                   EIN  390
   41 FORMAT (7E10.3)                                                   EIN  400
  600 FORMAT (///' ***** DENSITY OF ABSORBER EQUALS 0.   PROGRAM STOPS. EIN  410
     1*****')                                                           EIN  420
C                                                                       EIN  430
C                                                                       EIN  440
      IEINGA = IEINGA + 1                                               EIN  450
      GOTO(7,8,9),IEINGA                                                EIN  460
    7 CONTINUE                                                          EIN  470
      EAZERO = FIELD(1)                                                 EIN  480
      EGSTAT = FIELD(2)                                                 EIN  490
      ESIGPZ = FIELD(3)                                                 EIN  500
      ETEMP = FIELD(4)                                                  EIN  510
      GOTO 99                                                           EIN  520
    8 CONTINUE                                                          EIN  530
      IF(FTYP .EQ. 0.) GOTO 15                                          EIN  540
      FIELD(2) = DATA(1)                                                EIN  550
      WRITE (6,30) DATA(1)                                              EIN  560
   15 CONTINUE                                                          EIN  570
      EABAR = FIELD(2)                                                  EIN  580
      IF(FIELD(3) .EQ. 0.) FIELD(3) = ECDANC                            EIN  590
      ECDANC = FIELD(3)                                                 EIN  600
      IF(KI .EQ. 3) GOTO 99                                             EIN  610
      EDZERO = FIELD(4)                                                 EIN  620
      EAMOD1 = FIELD(5)                                                 EIN  630
      ESIGM1 = FIELD(6)                                                 EIN  640
      EAMOD2 = FIELD(7)                                                 EIN  650
      GOTO 99                                                           EIN  660
    9 CONTINUE                                                          EIN  670
      IF(FTYP .EQ. 0.) GOTO 19                                          EIN  680
      IF(DATA(2) .GT. 0.) GOTO 605                                      EIN  690
      WRITE (6,600)                                                     EIN  700
      STOP                                                              EIN  710
  605 CONTINUE                                                          EIN  720
      FIELD(1) = DATA(2)                                                EIN  730
      FIELD(4) = DATA(3)                                                EIN  740
      FIELD(7) = DATA(4)                                                EIN  750
      WRITE (6,31) (DATA(I),I=2,4)                                      EIN  760
   19 ESIGM2 = FIELD(1)                                                 EIN  770
      IF(KI .EQ. 1) GOTO 99                                             EIN  780
      EDZERO = FIELD(1)                                                 EIN  790
      EAMOD1 = FIELD(2)                                                 EIN  800
      ESIGM1 = FIELD(3)                                                 EIN  810
      EDIQU1 = FIELD(4)                                                 EIN  820
      EAMOD2 = FIELD(5)                                                 EIN  830
      ESIGM2 = FIELD(6)                                                 EIN  840
      EDIQU2 = FIELD(7)                                                 EIN  850
      ESIG1 = ESIGM1                                                    EIN  860
      ESIG2 = ESIGM2                                                    EIN  870
      FIELD(3) = ESIG1 * EDIQU1                                         EIN  880
      FIELD(5) = ESIG2 * EDIQU2                                         EIN  890
      FIELD(4) = EAMOD2                                                 EIN  900
      KI = 5                                                            EIN  910
      GOTO 99                                                           EIN  920
C                                                                       EIN  930
      ENTRY EINGA5                                                      EIN  940
C                                                                       EIN  950
      IEING5 = IEING5 + 1                                               EIN  960
      GOTO(11,12),IEING5                                                EIN  970
   11 CONTINUE                                                          EIN  980
      IF(FTYP .EQ. 0.) GOTO 20                                          EIN  990
      DO 18 I=1,6                                                       EIN 1000
        FIELD(I) = DATA(I+4)                                            EIN 1010
   18 CONTINUE                                                          EIN 1020
      WRITE (6,32) (DATA(I),I=5,10)                                     EIN 1030
   20 DO 13 I=1,6                                                       EIN 1040
        EW(I) = FIELD(I)                                                EIN 1050
   13 CONTINUE                                                          EIN 1060
      EW(7) = TESTA                                                     EIN 1070
      GOTO 99                                                           EIN 1080
   12 CONTINUE                                                          EIN 1090
      IF(FTYP .EQ. 0.) GOTO 21                                          EIN 1100
      DO 17 I=1,4                                                       EIN 1110
        FIELD(I) = DATA(I+10)                                           EIN 1120
   17 CONTINUE                                                          EIN 1130
      WRITE (6,33) (DATA(I),I=11,14)                                    EIN 1140
   21 DO 14 I=1,4                                                       EIN 1150
        EW(I+7) = FIELD(I)                                              EIN 1160
   14 CONTINUE                                                          EIN 1170
      EW(18) = EW(11)                                                   EIN 1180
      EW(11) = ESIGPZ                                                   EIN 1190
      ESI1 = ESIGM1                                                     EIN 1200
      ESI2 = ESIGM2                                                     EIN 1210
      IF(EDIQU1 .GT. 0.) ESI1 = ESI1 * EDIQU1                           EIN 1220
      IF(EDIQU2 .GT. 0.) ESI2 = ESI2 * EDIQU2                           EIN 1230
      EW(12) = (ESI1+ESI2) * EDZERO                                     EIN 1240
      EW(14) = ECDANC                                                   EIN 1250
      EW(15) = 0.                                                       EIN 1260
      EW(16) = EDZERO                                                   EIN 1270
      EW(17) = EABAR                                                    EIN 1280
      GOTO 99                                                           EIN 1290
C                                                                       EIN 1300
      ENTRY EINGA9(MODE,MODE1)                                          EIN 1310
C                                                                       EIN 1320
      NWING0 = 1                                                        EIN 1330
      NNRESO = IFIX(FIELD(1))                                           EIN 1340
      NNDATA = 0                                                        EIN 1350
      IF(MODE .EQ. MODE1) NNDATA = 29                                   EIN 1360
      FIELD(2) = 30.                                                    EIN 1370
      NNSIGA = IFIX(FIELD(2))                                           EIN 1380
C                                                                       EIN 1390
C     FORMERLY USED FOR WING CORRECTION                                 EIN 1400
C                                                                       EIN 1410
C     IF(KI .EQ. 2) NWING0 = IFIX(FIELD(2))                             EIN 1420
C                                                                       EIN 1430
      IF(NNRESO .NE. 0) GOTO 98                                         EIN 1440
C                                                                       EIN 1450
      CALL TERMINAT                                                     EIN 1460
C                                                                       EIN 1470
   98 CONTINUE                                                          EIN 1480
CFZJ035                                                       14.09.04  EIN 1490
      GOTO(50,51,52,53,54,60),NNRESO                                    EIN 1500
   50 NNRESO = 26                                                       EIN 1510
      GOTO 61                                                           EIN 1520
   51 NNRESO = 27                                                       EIN 1530
      GOTO 61                                                           EIN 1540
   52 NNRESO = 28                                                       EIN 1550
      GOTO 61                                                           EIN 1560
   53 NNRESO = 25                                                       EIN 1570
      GOTO 61                                                           EIN 1580
   54 NNRESO = 24                                                       EIN 1590
      GOTO 61                                                           EIN 1600
   60 CONTINUE                                                          EIN 1610
      NNRESO = 5                                                        EIN 1620
   61 CONTINUE                                                          EIN 1630
CFZJ035                                                       14.09.04  EIN 1640
      IF(NNRESO .EQ. 24) OPEN(UNIT=NNRESO,FILE='Libraries\resdapu0')    EIN 1650
      IF(NNRESO .EQ. 25) OPEN(UNIT=NNRESO,FILE='Libraries\resdapu2')    EIN 1660
      IF(NNRESO .EQ. 26) OPEN(UNIT=NNRESO,FILE='Libraries\resdatu5')    EIN 1670
      IF(NNRESO .EQ. 27) OPEN(UNIT=NNRESO,FILE='Libraries\resdatth')    EIN 1680
      IF(NNRESO .EQ. 28) OPEN(UNIT=NNRESO,FILE='Libraries\resdatu8')    EIN 1690
C                                                                       EIN 1700
      IF(NNRESO .GT. 0) N5 = NNRESO                                     EIN 1710
C                                                                       EIN 1720
      READ (M5,41) ETEMP,ENERGU,ENERGO,ESOLVE,TESTA,EW(13),VAZVG        EIN 1730
C                                                                       EIN 1740
      IF(NNDATA .GT. 0) READ (M5,40) FUTYP,EAMOD1,ESIGM1,EAMOD2,ESIGM2, EIN 1750
     1 ECDANC                                                           EIN 1760
C                                                                       EIN 1770
      JI = -NNSIGA                                                      EIN 1780
      FIELD(1) = FUTYP                                                  EIN 1790
   99 CONTINUE                                                          EIN 1800
      RETURN                                                            EIN 1810
      END                                                               EIN 1820
      SUBROUTINE RESODA(IZAEHL,IEND)                                    ESO   10
C                                                                       ESO   20
C     VERARBEITEN DER RESONANZDATENEINHEIT                              ESO   30
C                                                                       ESO   40
      DIMENSION FIELD(10)                                               ESO   50
C                                                                       ESO   60
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     ESO   70
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   ESO   80
     2 ESIG2,EDIQU2,KI,IEING5,TESTA,ENERGU,ENERGO,NNRESO,NNDATA,NNSIGA, ESO   90
     3 JI                                                               ESO  100
C                                                                       ESO  110
      COMMON /DATA2/ NEI29,NXT29,FUTYP,DATA(14),IWOHIN,FTYP             ESO  120
C                                                                       ESO  130
      COMMON /DOHE/ EW(20),XTAB(200),FTAB(200)                          ESO  140
C                                                                       ESO  150
      COMMON /GAGRU/ EGO(69),SIRA(69),FF,FL,M6,NGGR,NGGR1,EOE,EERSTE,   ESO  160
     1 ELETZT,IO,IU,RIJS,MMEES,IIII,JRJR,ITUZ,IGRUG,M5,NEUI,NALTI,      ESO  170
     2 UAPS(501),N5                                                     ESO  180
C                                                                       ESO  190
      COMMON /DIR25/ NAE25,NEI25,LIBSAE,KENNNR,LENNNR,IFALNR,NUCLID     ESO  200
C                                                                       ESO  210
C                                                                       ESO  220
      IWOHIN = 0                                                        ESO  230
      IF(NNRESO .LT. 0) GOTO 4                                          ESO  240
      IF(IEND .NE. 2) GOTO 1                                            ESO  250
      IF(FIELD(1) .LT. ENERGU .OR. FIELD(1) .GT. ENERGO) IWOHIN = -1    ESO  260
    1 CONTINUE                                                          ESO  270
      IF(IEND .NE. 6) GOTO 2                                            ESO  280
      FIELD(1) = AMAX1(FIELD(1),ENERGU)                                 ESO  290
      FIELD(6) = AMIN1(FIELD(6),ENERGO)                                 ESO  300
      IF(FIELD(1) .GE. FIELD(6)) IWOHIN = -1                            ESO  310
    2 CONTINUE                                                          ESO  320
      IF(IEND .NE. 1) GOTO 99                                           ESO  330
      KI = 4                                                            ESO  340
      FIELD(4) = ETEMP                                                  ESO  350
      IF(FIELD(1) .EQ. 232.) IDNUV = 6                                  ESO  360
      IF(FIELD(1) .EQ. 235.) IDNUV = 10                                 ESO  370
      IF(FIELD(1) .EQ. 238.) IDNUV = 12                                 ESO  380
CFZJ035                                                       14.09.04  ESO  390
      IF(FIELD(1) .EQ. 240.) IDNUV = 15                                 ESO  400
      IF(FIELD(1) .EQ. 242.) IDNUV = 17                                 ESO  410
      IF(NUCLID .EQ. IDNUV) GOTO 2500                                   ESO  420
      WRITE (M6,505) NUCLID,IDNUV                                       ESO  430
      STOP                                                              ESO  440
 2500 CONTINUE                                                          ESO  450
      IF(NNRESO .NE. M5) REWIND NNRESO                                  ESO  460
      NNRESO = -NNDATA                                                  ESO  470
      GOTO 99                                                           ESO  480
    4 CONTINUE                                                          ESO  490
      IZAEHL = IZAEHL + 1                                               ESO  500
      GOTO(5,6,7,8,9),IZAEHL                                            ESO  510
    5 CONTINUE                                                          ESO  520
      IEND = 1                                                          ESO  530
      JI = 10                                                           ESO  540
      KI = 3                                                            ESO  550
      FIELD(1) = ESOLVE                                                 ESO  560
      FIELD(3) = ECDANC                                                 ESO  570
      GOTO 99                                                           ESO  580
    6 CONTINUE                                                          ESO  590
      IEND = 1                                                          ESO  600
      JI = 13                                                           ESO  610
      KI = 7                                                            ESO  620
      FIELD(2) = EAMOD1                                                 ESO  630
      FIELD(3) = ESIGM1                                                 ESO  640
      FIELD(5) = EAMOD2                                                 ESO  650
      FIELD(6) = ESIGM2                                                 ESO  660
      GOTO 99                                                           ESO  670
    7 CONTINUE                                                          ESO  680
      IEND = 5                                                          ESO  690
      JI = 18                                                           ESO  700
      KI = 7                                                            ESO  710
      FIELD(7) = TESTA                                                  ESO  720
      GOTO 99                                                           ESO  730
    8 CONTINUE                                                          ESO  740
      IEND = 5                                                          ESO  750
      JI = 25                                                           ESO  760
      KI = 7                                                            ESO  770
      FIELD(6) = EW(13)                                                 ESO  780
      GOTO 99                                                           ESO  790
    9 CONTINUE                                                          ESO  800
      IEND = 8                                                          ESO  810
      JI = 0                                                            ESO  820
      KI = 0                                                            ESO  830
      NNRESO = 0                                                        ESO  840
      IWOHIN = 9                                                        ESO  850
   99 CONTINUE                                                          ESO  860
      RETURN                                                            ESO  870
C                                                                       ESO  880
  505 FORMAT (///' *** THE INPUT SPECIFIES AN ABSORBER NUCLIDE (=IDNUCL,ESO  890
     1 CARD Z4) WITH THE GAM-ID.NO.',I4/' *** THE RESONANCE PARAMETERS AESO  900
     2RE READ FOR GAM-ID.NO.',I4,' (CHECK RNRESO, CARD Z1)'/' *** RESONAESO  910
     3NCE INTEGRAL CALCULATION IS STOPPED.')                            ESO  920
      END                                                               ESO  930
      SUBROUTINE DANC3(KKKK)                                            DAN   10
C                                                                       DAN   20
C     RECHENCODE DANCOFF III                                            DAN   30
C                                                                       DAN   40
C                                                                       DAN   50
      DIMENSION C(6,100),FIELD(10),EE(17)                               DAN   60
C                                                                       DAN   70
      COMMON /EINGA/ FIELD,EAZERO,EGSTAT,ESIGPZ,ETEMP,ESOLVE,EABAR,     DAN   80
     1 ECDANC,EDZERO,EAMOD1,ESIGM1,EAMOD2,ESIGM2,IEINGA,ESIG1,EDIQU1,   DAN   90
     2 ESIG2,EDIQU2,KI,IEING5                                           DAN  100
C                                                                       DAN  110
      COMMON /DANCO/ KA(25,6,2),KKQ(15),KOQ(30),KSQ(15),KSP(3,25,6),    DAN  120
     1 MQ(25,20),MAQ(15),MSQ(3,25),NQ(25,20),NAQ(15),NIQ(25),NJQ(3),    DAN  130
     2 NPHIQ(5),NSQ(3,25),NSPQ(3),NSTQ(25),NZQ(5),NI,NJ,NSP,NST,ETAQ(20)DAN  140
     3 ,G(6,100),KMQ(20),KNQ(20),KMNQ(20),PHINT(100),SHUE(100),SMOD(100)DAN  150
     4 ,SSP(100),X(20),XE(4),XI(20),Y(20),YE(4),ZINT(100),A,AD,B,DHUE,  DAN  160
     5 DSP,G1,G2,G3,K,KG,KK,KO,KS,LA,LSP,M,MA,MS,N,NA,NPHI,NS,NSIG,NZ   DAN  170
C                                                                       DAN  180
      EQUIVALENCE(EAZERO,EE(1))                                         DAN  190
C                                                                       DAN  200
   11 FORMAT (1H1/30H CALCULATION CODE  DANCOFF III  ///)               DAN  210
   41 FORMAT (26H INFINITE SQUARE GRID (K =,I3,1H)//)                   DAN  220
   43 FORMAT (13H SQUARE GRID,,I2,2H X,I2,22H RODS PER ELEMENT (K =,I3, DAN  230
     1 1H)//)                                                           DAN  240
   45 FORMAT (29H INFINITE HEXAGONAL GRID (K =,I3,1H)//)                DAN  250
   47 FORMAT (16H HEXAGONAL GRID,,I3,16H-ROD-BUNDLE (K =,I3,1H)/)       DAN  260
   48 FORMAT (28H ROD BUNDLE, PUSHED TOGETHER    //)                    DAN  270
   50 FORMAT (1H )                                                      DAN  280
   60 FORMAT (I4,35H INTEGRATION STEPS IN ZET-DIRECTION/I4,35H INTEGRATIDAN  290
     1ON STEPS IN PHI-DIRECTION//)                                      DAN  300
   61 FORMAT (56H ROD RADIUS    ROD DISTANCE   CLADDING SIZE     GAP SIZDAN  310
     1E /E11.4,3E15.4//)                                                DAN  320
   62 FORMAT (40H NOT CALCULATED - OVERLAPPING FUEL RODS )              DAN  330
  801 FORMAT (18X,36H SIGMA          SIGMA          SIGMA)              DAN  340
  811 FORMAT (15X,' (MODERATOR)     (CLADDING)       (GAP)              DAN  350
     1   G             C'//(I11,3E15.4,F13.4,E18.5))                    DAN  360
  821 FORMAT (15X,39H (MODERATOR)     (CLADDING)       (GAP),11X,3H G1, DAN  370
     1 7X,3H G2//(I11,3E15.4,F13.4,F10.4))                              DAN  380
  831 FORMAT (15X,39H (MODERATOR)     (CLADDING)       (GAP),11X,3H G1, DAN  390
     1 7X,3H G2,7X,3H G3//(I11,3E15.4,F13.4,2F10.4))                    DAN  400
  841 FORMAT (15X,39H (MODERATOR)     (CLADDING)       (GAP),11X,3H G1, DAN  410
     1 7X,3H G2,7X,3H G3,7X,3H G4//(I11,3E15.4,F13.4,3F10.4))           DAN  420
  861 FORMAT (15X,39H (MODERATOR)     (CLADDING)       (GAP),11X,3H G1, DAN  430
     1 7X,3H G2,7X,3H G3,7X,3H G4,7X,3H G5,7X,3H G6//(I11,3E15.4,F13.4, DAN  440
     2 5F10.4))                                                         DAN  450
C                                                                       DAN  460
C                                                                       DAN  470
C     EINGABEDATEN                                                      DAN  480
C                                                                       DAN  490
      WRITE (6,11)                                                      DAN  500
      K = KKKK                                                          DAN  510
      NSIG = 1                                                          DAN  520
      A = FIELD(1)                                                      DAN  530
      B = FIELD(2)                                                      DAN  540
      DHUE = FIELD(3)                                                   DAN  550
      DSP = FIELD(4)                                                    DAN  560
      SMOD(1) = FIELD(5)                                                DAN  570
      SHUE(1) = FIELD(6)                                                DAN  580
      SSP(1) = FIELD(7)                                                 DAN  590
      KG = 1                                                            DAN  600
      KO = KOQ(K)                                                       DAN  610
      KS = KSQ(KO)                                                      DAN  620
      KK = KKQ(KO)                                                      DAN  630
      MA = MAQ(KO)                                                      DAN  640
      NA = NAQ(KO)                                                      DAN  650
      NZ = NZQ(KS)                                                      DAN  660
      NPHI = NPHIQ(KS)                                                  DAN  670
      GOTO(40,42,44,46,46),KS                                           DAN  680
   40 WRITE (6,41) K                                                    DAN  690
      GOTO 59                                                           DAN  700
   42 WRITE (6,43) KK,KK,K                                              DAN  710
      GOTO 59                                                           DAN  720
   44 WRITE (6,45) K                                                    DAN  730
      GOTO 59                                                           DAN  740
   46 WRITE (6,47) KK,K                                                 DAN  750
      IF(2.*A+DHUE .LE. 0.8660254*B) GOTO 49                            DAN  760
      KG = 2                                                            DAN  770
      KO = KO + 1                                                       DAN  780
      MA = MAQ(KO)                                                      DAN  790
      NA = NAQ(KO)                                                      DAN  800
      IF(KS .EQ. 4) GOTO 49                                             DAN  810
      WRITE (6,48)                                                      DAN  820
      GOTO 59                                                           DAN  830
   49 WRITE (6,50)                                                      DAN  840
   59 CONTINUE                                                          DAN  850
      WRITE (6,60) NZ,NPHI                                              DAN  860
      WRITE (6,61) A,B,DHUE,DSP                                         DAN  870
      AD = A + DHUE                                                     DAN  880
      IF(B/2. .GE. AD) GOTO 63                                          DAN  890
      WRITE (6,62)                                                      DAN  900
      GOTO 10                                                           DAN  910
   63 CONTINUE                                                          DAN  920
C                                                                       DAN  930
C     BERECHNUNG DER STABPOSITIONEN                                     DAN  940
C                                                                       DAN  950
      IF(KS .GE. 3) GOTO 65                                             DAN  960
      G1 = 1.                                                           DAN  970
      G2 = 1.                                                           DAN  980
      G3 = 8.                                                           DAN  990
      GOTO 70                                                           DAN 1000
   65 G1 = 0.5                                                          DAN 1010
      G2 = 0.8660254                                                    DAN 1020
      G3 = 12.                                                          DAN 1030
   70 DO 71 LI=1,6                                                      DAN 1040
        DO 71 LSIG=1,NSIG                                               DAN 1050
          G(LI,LSIG) = 1.                                               DAN 1060
   71 CONTINUE                                                          DAN 1070
      IF(KS .EQ. 2) GOTO 80                                             DAN 1080
      DO 78 LA=MA,NA                                                    DAN 1090
        NST = NSTQ(LA)                                                  DAN 1100
        NI = NIQ(LA)                                                    DAN 1110
        DO 75 LST=1,NST                                                 DAN 1120
          M = MQ(LA,LST)                                                DAN 1130
          N = NQ(LA,LST)                                                DAN 1140
          KMQ(LST) = 0                                                  DAN 1150
          KNQ(LST) = 0                                                  DAN 1160
          X(LST) = G1 * FLOAT(M) * B                                    DAN 1170
          Y(LST) = G2 * FLOAT(N) * B                                    DAN 1180
   75   CONTINUE                                                        DAN 1190
C                                                                       DAN 1200
        CALL SUBR                                                       DAN 1210
C                                                                       DAN 1220
   78 CONTINUE                                                          DAN 1230
      GOTO 800                                                          DAN 1240
   80 NSP = NSPQ(KO-1)                                                  DAN 1250
      NJ = NJQ(KO-1)                                                    DAN 1260
      NST = NSTQ(1)                                                     DAN 1270
      NI = NJ                                                           DAN 1280
      DO 100 LSP=1,NSP                                                  DAN 1290
        MS = MSQ(KO-1,LSP)                                              DAN 1300
        NS = NSQ(KO-1,LSP)                                              DAN 1310
        DO 90 LST=1,NST                                                 DAN 1320
          M = MQ(1,LST)                                                 DAN 1330
          N = NQ(1,LST)                                                 DAN 1340
          KMQ(LST) = 0                                                  DAN 1350
          IF(M .GT. MS) KMQ(LST) = 1                                    DAN 1360
          KNQ(LST) = 0                                                  DAN 1370
          IF(N .GT. NS) KNQ(LST) = 1                                    DAN 1380
          X(LST) = FLOAT(M) * B + FLOAT(KMQ(LST)) * DSP                 DAN 1390
          Y(LST) = FLOAT(N) * B + FLOAT(KNQ(LST)) * DSP                 DAN 1400
   90   CONTINUE                                                        DAN 1410
C                                                                       DAN 1420
        CALL SUBR                                                       DAN 1430
C                                                                       DAN 1440
  100 CONTINUE                                                          DAN 1450
C                                                                       DAN 1460
C     DATENAUSGABE                                                      DAN 1470
C                                                                       DAN 1480
  800 WRITE (6,801)                                                     DAN 1490
      DO 700 LI=1,NI                                                    DAN 1500
        DO 700 LSIG=1,NSIG                                              DAN 1510
          C(LI,LSIG) = 1. - G(LI,LSIG)                                  DAN 1520
  700 CONTINUE                                                          DAN 1530
      GOTO(810,820,830,840,860,860),NI                                  DAN 1540
  810 WRITE (6,811) (LSIG,SMOD(LSIG),SHUE(LSIG),SSP(LSIG),(G(LI,LSIG),LIDAN 1550
     1 =1,NI),(C(LI,LSIG),LI=1,NI),LSIG=1,NSIG)                         DAN 1560
      GOTO 10                                                           DAN 1570
  820 WRITE (6,821) (LSIG,SMOD(LSIG),SHUE(LSIG),SSP(LSIG),(G(LI,LSIG),LIDAN 1580
     1 =1,NI),LSIG=1,NSIG)                                              DAN 1590
      GOTO 10                                                           DAN 1600
  830 WRITE (6,831) (LSIG,SMOD(LSIG),SHUE(LSIG),SSP(LSIG),(G(LI,LSIG),LIDAN 1610
     1 =1,NI),LSIG=1,NSIG)                                              DAN 1620
      GOTO 10                                                           DAN 1630
  840 WRITE (6,841) (LSIG,SMOD(LSIG),SHUE(LSIG),SSP(LSIG),(G(LI,LSIG),LIDAN 1640
     1 =1,NI),LSIG=1,NSIG)                                              DAN 1650
      GOTO 10                                                           DAN 1660
  860 WRITE (6,861) (LSIG,SMOD(LSIG),SHUE(LSIG),SSP(LSIG),(G(LI,LSIG),LIDAN 1670
     1 =1,NI),LSIG=1,NSIG)                                              DAN 1680
   10 CONTINUE                                                          DAN 1690
      ECDANC = C(1,1)                                                   DAN 1700
      RETURN                                                            DAN 1710
      END                                                               DAN 1720
      BLOCK DATA                                                        LOC   10
C                                                                       LOC   20
      DIMENSION KSP1(288),KSP2(162),MQ1(288),MQ2(212),NQ1(288),NQ2(212),LOC   30
     1 KA1(288),KA2(12),KAY(300),KSPY(450),MQY(500),NQY(500)            LOC   40
C                                                                       LOC   50
      COMMON /DANCO/ KA(25,6,2),KKQ(15),KOQ(30),KSQ(15),KSP(3,25,6),    LOC   60
     1 MQ(25,20),MAQ(15),MSQ(3,25),NQ(25,20),NAQ(15),NIQ(25),NJQ(3),    LOC   70
     2 NPHIQ(5),NSQ(3,25),NSPQ(3),NSTQ(25),NZQ(5),NI,NJ,NSP,NST,ETAQ(20)LOC   80
     3 ,G(6,100),KMQ(20),KNQ(20),KMNQ(20),PHINT(100),SHUE(100),SMOD(100)LOC   90
     4 ,SSP(100),X(20),XE(4),XI(20),Y(20),YE(4),ZINT(100),A,AD,B,DHUE,  LOC  100
     5 DSP,G1,G2,G3,K,KG,KK,KO,KS,LA,LSP,M,MA,MS,N,NA,NPHI,NS,NSIG,NZ   LOC  110
C                                                                       LOC  120
      EQUIVALENCE(KAY(1),KA1(1)),(KAY(289),KA2(1)),(KA(1,1,1),KAY(1)),  LOC  130
     1 (KSPY(1),KSP1(1)),(KSPY(289),KSP2(1)),(KSP(1,1,1),KSPY(1)),      LOC  140
     2 (MQY(1),MQ1(1)),(MQY(289),MQ2(1)),(MQ(1,1),MQY(1)),              LOC  150
     3 (NQY(1),NQ1(1)),(NQY(289),NQ2(1)),(NQ(1,1),NQY(1))               LOC  160
C                                                                       LOC  170
      INTEGER KA1/ 8,12, 2, 2, 0, 2, 2, 2, 0, 4, 2, 2, 0, 0, 0, 2, 2, 0,LOC  180
     1 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12, 0, 0,LOC  190
     2 2, 2, 2, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  200
     3 0, 0, 0, 0, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  210
     4 0, 0, 0, 0, 0, 0, 0, 0,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  220
     5 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  230
     6 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  240
     7 8,12, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 0, 0, 2, 2, 0,LOC  250
     8 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12, 0, 0, 2, 0, 2, 0,LOC  260
     9 2, 2, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  270
     X 0, 0, 4, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  280
     Y 0, 0, 0, 0, 0, 0, 0, 0, 0,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  290
     Z 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  300
     Z 0, 0, 0, 0, 0, 0/                                                LOC  310
C                                                                       LOC  320
      INTEGER KA2/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/                  LOC  330
C                                                                       LOC  340
      INTEGER KKQ/ 0, 4, 5, 6, 0, 2, 2, 3, 3, 7, 7,19,19, 0, 0/         LOC  350
C                                                                       LOC  360
      INTEGER KOQ/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 3, 4, 0, 0,LOC  370
     1 0, 5, 0, 6, 8, 0, 0, 0,10, 0,12, 0/                              LOC  380
C                                                                       LOC  390
      INTEGER KSQ/ 1, 2, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 5, 0, 0/         LOC  400
C                                                                       LOC  410
      INTEGER KSP1/2, 2, 2, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 2, 0, 0, 0,LOC  420
     1 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,LOC  430
     2 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,LOC  440
     3 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 1, 1, 1, 1, 0, 0,LOC  450
     4 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0,LOC  460
     5 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,LOC  470
     6 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 2,LOC  480
     7 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 2, 2, 0,LOC  490
     8 0, 0, 0, 2, 0, 2, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  500
     9 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,LOC  510
     X 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 1, 0,LOC  520
     Y 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0,LOC  530
     Z 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  540
     Z 0, 0, 1, 0, 0, 0/                                                LOC  550
C                                                                       LOC  560
      INTEGER KSP2/0, 0, 0, 0, 2, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  570
     1 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 1, 0,LOC  580
     2 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0,LOC  590
     3 1, 0, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,LOC  600
     4 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  610
     5 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 2, 0, 0,LOC  620
     6 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0,LOC  630
     7 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/                              LOC  640
C                                                                       LOC  650
      INTEGER MQ1/ 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,LOC  660
     1 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 2, 2, 0, 0, 2, 2, 0, 1, 1, 0,LOC  670
     2 1, 2, 2, 1, 2, 0, 1, 1, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,LOC  680
     3 2, 2, 0, 2, 3, 3, 2, 0, 0, 2, 2, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0,LOC  690
     4 0, 0, 0, 0, 3, 0, 3, 5, 0, 0, 0, 0, 3, 3, 0, 0, 1, 5, 0, 0, 0, 0,LOC  700
     5 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 5, 0, 0, 0, 1, 7, 0,LOC  710
     6 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  720
     7 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  730
     8 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  740
     9 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  750
     X 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  760
     Y 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  770
     Z 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0,LOC  780
     Z 0, 0, 0, 0, 0, 0/                                                LOC  790
C                                                                       LOC  800
      INTEGER MQ2/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0,LOC  810
     1 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,LOC  820
     2 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  830
     3 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  840
     4 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  850
     5 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  860
     6 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  870
     7 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  880
     8 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC  890
     9 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/            LOC  900
C                                                                       LOC  910
      INTEGER MAQ/ 1, 1, 1, 1, 2, 3, 3, 6, 6,10,10,16,16, 0, 0/         LOC  920
C                                                                       LOC  930
      INTEGER MSQ/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1,LOC  940
     1 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 3, 2, 2, 3,LOC  950
     2 2, 2, 3, 2, 2, 3, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 4,LOC  960
     3 4, 0, 4, 4, 0, 4, 4, 0, 4, 4, 0, 4, 4/                           LOC  970
C                                                                       LOC  980
      INTEGER NQ1/ 1,-1, 0, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1,-1,-1,-1, 1, 1,LOC  990
     1-1,-1, 1,-1,-1,-1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,LOC 1000
     2 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,LOC 1010
     3 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0,LOC 1020
     4 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 3, 1, 0, 0, 0, 0,LOC 1030
     5 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 4, 1, 0,LOC 1040
     6 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1050
     7 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1060
     8 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1070
     9 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1080
     X 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1090
     Y 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1100
     Z 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0,LOC 1110
     Z 0, 0, 0, 0, 0, 0/                                                LOC 1120
C                                                                       LOC 1130
      INTEGER NQ2/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0,LOC 1140
     1 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0,LOC 1150
     2 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1160
     3 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1170
     4 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1180
     5 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1190
     6 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1200
     7 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1210
     8 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,LOC 1220
     9 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/            LOC 1230
C                                                                       LOC 1240
      INTEGER NAQ/ 1, 1, 1, 1, 2, 4, 5, 8, 9,13,15,21,24, 0, 0/         LOC 1250
C                                                                       LOC 1260
      INTEGER NIQ/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 4, 4, 4,LOC 1270
     1 4, 4, 4, 4, 4, 4, 0/                                             LOC 1280
C                                                                       LOC 1290
      INTEGER NJQ/ 3, 6, 6/                                             LOC 1300
C                                                                       LOC 1310
      INTEGER NPHIQ/  8, 8, 8, 8, 8/                                    LOC 1320
C                                                                       LOC 1330
      INTEGER NSQ/ 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 0, 4, 4, 1, 0, 0,LOC 1340
     1 2, 1, 1, 3, 2, 2, 0, 3, 3, 1, 4, 4, 2, 0, 0, 3, 1, 1, 0, 2, 2, 1,LOC 1350
     2 3, 3, 2, 4, 4, 3, 0, 0, 0, 1, 1, 0, 2, 2, 0, 3, 3, 0, 4, 4, 0, 0,LOC 1360
     3 0, 0, 1, 1, 0, 2, 2, 0, 3, 3, 0, 4, 4/                           LOC 1370
C                                                                       LOC 1380
      INTEGER NSPQ/  16,25,25/                                          LOC 1390
C                                                                       LOC 1400
      INTEGER NSTQ/  18, 6, 1, 1, 1, 2, 2, 1, 1, 3, 2, 1, 3, 4, 1, 6, 4,LOC 1410
     1 3, 3, 2, 1, 5, 4, 1, 0/                                          LOC 1420
C                                                                       LOC 1430
      INTEGER NZQ/40,40,40,40,40/                                       LOC 1440
C                                                                       LOC 1450
      END                                                               LOC 1460
      SUBROUTINE SUBR                                                   UBR   10
C                                                                       UBR   20
      COMMON /DANCO/ KA(25,6,2),KKQ(15),KOQ(30),KSQ(15),KSP(3,25,6),    UBR   30
     1 MQ(25,20),MAQ(15),MSQ(3,25),NQ(25,20),NAQ(15),NIQ(25),NJQ(3),    UBR   40
     2 NPHIQ(5),NSQ(3,25),NSPQ(3),NSTQ(25),NZQ(5),NI,NJ,NSP,NST,ETAQ(20)UBR   50
     3 ,G(6,100),KMQ(20),KNQ(20),KMNQ(20),PHINT(100),SHUE(100),SMOD(100)UBR   60
     4 ,SSP(100),X(20),XE(4),XI(20),Y(20),YE(4),ZINT(100),A,AD,B,DHUE,  UBR   70
     5 DSP,G1,G2,G3,K,KG,KK,KO,KS,LA,LSP,M,MA,MS,N,NA,NPHI,NS,NSIG,NZ   UBR   80
C                                                                       UBR   90
      REAL MODS                                                         UBR  100
C                                                                       UBR  110
C                                                                       UBR  120
C     AUFSTELLUNG DER STAB-UNTERBIBLIOTHEK                              UBR  130
C                                                                       UBR  140
      PI = 3.141593                                                     UBR  150
      DPHI = 2. * PI / (G3*FLOAT(NPHI))                                 UBR  160
      PHI = DPHI / 2.                                                   UBR  170
      DO 110 LSIG=1,NSIG                                                UBR  180
        PHINT(LSIG) = 0.                                                UBR  190
  110 CONTINUE                                                          UBR  200
      DO 600 LPHI=1,NPHI                                                UBR  210
        SIFI = SIN(PHI)                                                 UBR  220
        COFI = COS(PHI)                                                 UBR  230
        NSU = 0                                                         UBR  240
        LSU = 1                                                         UBR  250
        DO 200 LST=1,NST                                                UBR  260
          ETA = -X(LST) * SIFI + Y(LST) * COFI                          UBR  270
          IF(ABS(ETA) .GT. 2.*A) GOTO 200                               UBR  280
          XI(LSU) = X(LST) * COFI + Y(LST) * SIFI                       UBR  290
          ETAQ(LSU) = ETA                                               UBR  300
          KMNQ(LSU) = 2 * KMQ(LST) + KNQ(LST) + 1                       UBR  310
          NSU = LSU                                                     UBR  320
          LSU = LSU + 1                                                 UBR  330
  200   CONTINUE                                                        UBR  340
        IF(NSU .EQ. 0) GOTO 599                                         UBR  350
C                                                                       UBR  360
C     BERECHNUNG DER PROJIZIERTEN SEHNENLAENGEN                         UBR  370
C                                                                       UBR  380
        DZ = 2. / FLOAT(NZ)                                             UBR  390
        Z = -1. + DZ / 2.                                               UBR  400
        DO 210 LSIG=1,NSIG                                              UBR  410
          ZINT(LSIG) = 0                                                UBR  420
  210   CONTINUE                                                        UBR  430
        AA = A * A                                                      UBR  440
        DO 500 LZ=1,NZ                                                  UBR  450
          AZ = A * Z                                                    UBR  460
          DO 300 LSU=1,NSU                                              UBR  470
            BETR = ABS(ETAQ(LSU)-AZ)                                    UBR  480
            IF(BETR .LE. A) GOTO 310                                    UBR  490
  300     CONTINUE                                                      UBR  500
          GOTO 499                                                      UBR  510
  310     AZ2 = AZ * AZ                                                 UBR  520
          W1 = SQRT(AA-AZ2)                                             UBR  530
          B2 = BETR**2                                                  UBR  540
          W5 = SQRT(AA-B2)                                              UBR  550
          HUES = 0.                                                     UBR  560
          IF(DHUE .EQ. 0.) GOTO 314                                     UBR  570
          AD2 = AD * AD                                                 UBR  580
          W2 = SQRT(AD2-AZ2)                                            UBR  590
          W6 = SQRT(AD2-B2)                                             UBR  600
          HUES = W2 - W1 + W6 - W5                                      UBR  610
          NSW = LSU - 1                                                 UBR  620
          IF(NSW .EQ. 0) GOTO 314                                       UBR  630
          DO 311 LSW=1,NSW                                              UBR  640
            BETR = ABS(ETAQ(LSW)-AZ)                                    UBR  650
            IF(BETR .GE. AD) GOTO 311                                   UBR  660
            B2 = BETR**2                                                UBR  670
            W4 = SQRT(AD2-B2)                                           UBR  680
            HUES = HUES + 2. * W4                                       UBR  690
  311     CONTINUE                                                      UBR  700
  314     DO 315 LSIG=1,NSIG                                            UBR  710
            IF(SMOD(LSIG) .NE. SSP(LSIG)) GOTO 320                      UBR  720
  315     CONTINUE                                                      UBR  730
          GOTO 321                                                      UBR  740
  320     KW = 0                                                        UBR  750
          KMN = KMNQ(LSU)                                               UBR  760
          GOTO(321,322,323,330),KMN                                     UBR  770
  321     SPS = 0                                                       UBR  780
          GOTO 350                                                      UBR  790
  322     SPS = DSP / SIFI                                              UBR  800
          GOTO 350                                                      UBR  810
  323     SPS = DSP / COFI                                              UBR  820
          GOTO 350                                                      UBR  830
  330     XE(1) = (FLOAT(MS)+0.5) * B                                   UBR  840
          YE(1) = (FLOAT(NS)+0.5) * B                                   UBR  850
          XE(2) = XE(1)                                                 UBR  860
          YE(2) = YE(1) + DSP                                           UBR  870
          XE(3) = XE(1) + DSP                                           UBR  880
          YE(3) = YE(1)                                                 UBR  890
          XE(4) = XE(3)                                                 UBR  900
          YE(4) = YE(2)                                                 UBR  910
          KW = 1                                                        UBR  920
          DO 340 LW=1,4                                                 UBR  930
            KV = 0.5 + 0.5 * SIGN(1.,-XE(LW)*SIFI+YE(LW)*COFI-AZ)       UBR  940
            KW = KW + KV                                                UBR  950
  340     CONTINUE                                                      UBR  960
          GOTO(341,342,343,344,345),KW                                  UBR  970
  341     SPS = DSP / COFI + DSP / SIFI                                 UBR  980
          GOTO 350                                                      UBR  990
  342     SPS = XE(4) / COFI - YE(1) / SIFI + AZ / (SIFI*COFI)          UBR 1000
          GOTO 350                                                      UBR 1010
  343     SPS = DSP / SIFI                                              UBR 1020
          GOTO 350                                                      UBR 1030
  344     SPS = -XE(1) / COFI + YE(4) / SIFI - AZ / (SIFI*COFI)         UBR 1040
          GOTO 350                                                      UBR 1050
  345     SPS = DSP / COFI + DSP / SIFI                                 UBR 1060
  350     MODS = XI(LSU) - W1 - W5 - HUES - SPS                         UBR 1070
C                                                                       UBR 1080
C     BERECHNUNG DER INTEGRALE                                          UBR 1090
C                                                                       UBR 1100
          DO 400 LSIG=1,NSIG                                            UBR 1110
            SL = SHUE(LSIG) * HUES + SMOD(LSIG) * MODS + SSP(LSIG) * SPSUBR 1120
            FKI = EXP(-SL) * (SL*SL+6.399407*SL+5.066719) / (SQRT(1.+PI/UBR 1130
     1       8.*SL)*(SL*SL+6.766116*SL+5.066719))                       UBR 1140
            ZINT(LSIG) = ZINT(LSIG) + FKI                               UBR 1150
  400     CONTINUE                                                      UBR 1160
  499     Z = Z + DZ                                                    UBR 1170
  500   CONTINUE                                                        UBR 1180
        DO 510 LSIG=1,NSIG                                              UBR 1190
          ZINT(LSIG) = DZ / 2. * ZINT(LSIG)                             UBR 1200
          PHINT(LSIG) = PHINT(LSIG) + ZINT(LSIG)                        UBR 1210
  510   CONTINUE                                                        UBR 1220
  599   PHI = PHI + DPHI                                                UBR 1230
  600 CONTINUE                                                          UBR 1240
      DO 1609 LSIG=1,NSIG                                               UBR 1250
        PHINT(LSIG) = DPHI / (2.*PI) * PHINT(LSIG)                      UBR 1260
        IF(KS .EQ. 2) GOTO 602                                          UBR 1270
        DO 601 LI=1,NI                                                  UBR 1280
          G(LI,LSIG) = G(LI,LSIG) - FLOAT(KA(LA,LI,KG)) * PHINT(LSIG)   UBR 1290
  601   CONTINUE                                                        UBR 1300
        GOTO 1609                                                       UBR 1310
  602   DO 609 LJ=1,NJ                                                  UBR 1320
          G(LJ,LSIG) = G(LJ,LSIG) - FLOAT(KSP(KO-1,LSP,LJ)) *           UBR 1330
     1     PHINT(LSIG)                                                  UBR 1340
  609   CONTINUE                                                        UBR 1350
 1609 CONTINUE                                                          UBR 1360
      RETURN                                                            UBR 1370
      END                                                               UBR 1380