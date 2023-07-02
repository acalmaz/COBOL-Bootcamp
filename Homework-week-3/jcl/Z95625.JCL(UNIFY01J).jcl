//UNIFY01J  JOB 1,NOTIFY=&SYSUID
//COBRUN    EXEC IGYWCL
//COBOL.SYSIN  DD DSN=Z95625.CBL(PBHWORK3),DISP=SHR
//LKED.SYSLMOD DD DSN=Z95625.LOAD(PBHWORK3),DISP=SHR
// IF RC < 5 THEN
//RUN    EXEC PGM=PBHWORK3
//STEPLIB  DD DSN=Z95625.LOAD,DISP=SHR
//INPFILE  DD DSN=Z95625.QSAM.INP,DISP=SHR
//IDXFILE  DD DSN=Z95625.VSAM.AA,DISP=SHR
//OUTFILE  DD DSN=Z95625.QSAM.OUT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,10),RLSE),
//            DCB=(RECFM=FB,LRECL=61,BLKSIZE=0)
//SYSOUT   DD SYSOUT=*,OUTLIM=15000
//CEEDUMP  DD DUMMY
//SYSUDUMP DD DUMMY
// ELSE
// ENDIF