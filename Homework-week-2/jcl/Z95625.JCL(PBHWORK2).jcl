//PBHWORK2 JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBHWORK2),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBHWORK2),DISP=SHR
//***************************************************/
// IF RC < 05 THEN
//***************************************************/
//RUN     EXEC PGM=PBHWORK2
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//DATEREC   DD DSN=&SYSUID..QSAM.BB,DISP=SHR
//PRTLINE   DD DSN=&SYSUID..QSAM,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(10,10),RLSE),
//             DCB=(RECFM=FB,LRECL=58,BLKSIZE=0)
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF