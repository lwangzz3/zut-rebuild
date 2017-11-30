      SUBROUTINE INPLIST                                                INP   10
C                                                                       INP   20
C     INPUT-LISTING BEFORE STARTING THE EXECUTABLE PROGRAM              INP   30
C                                                                       INP   40
      CHARACTER*1 A(80)                                                 INP   50
C                                                                       INP   60
  100 FORMAT (80A1)                                                     INP   70
  200 FORMAT (10X,8(9X,I1))                                             INP   80
  201 FORMAT (2X,'NR. ',4X,8('123456789 '))                             INP   90
  202 FORMAT (1X,5('-'),4X,80('-')//)                                   INP  100
  203 FORMAT (I6,4X,80A1)                                               INP  110
  204 FORMAT ('1')                                                      INP  120
C                                                                       INP  130
C                                                                       INP  140
      J = 0                                                             INP  150
      WRITE (6,204)                                                     INP  160
      WRITE (6,200) (I, I=1,8)                                          INP  170
      WRITE (6,201)                                                     INP  180
      WRITE (6,202)                                                     INP  190
    1 CONTINUE                                                          INP  200
      READ (5,100,END=2) (A(I), I=1,80)                                 INP  210
      J = J + 1                                                         INP  220
      WRITE (6,203) J,(A(I), I=1,80)                                    INP  230
      GO TO 1                                                           INP  240
    2 CONTINUE                                                          INP  250
      RETURN                                                            INP  260
      END                                                               INP  270