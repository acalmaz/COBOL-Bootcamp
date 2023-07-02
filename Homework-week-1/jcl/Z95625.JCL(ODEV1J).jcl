//ODEV1J JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(ODEV1),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(ODEV1),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=ODEV1
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=&SYSUID..DATA,DISP=SHR
//*Aadaki satrda deiiklik yaptk.
//*Satrn ne yaptn dosyann en altnda belirtiyoruz.
//PRTLINE   DD DSN=&SYSUID..ODEV1.OUTPUT,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5))
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
//*DD: Data Definition (Veri Tanm) anlamna gelir ve bir dosya tanm oluturm
//*DSN=&SYSUID..ODEV1.OUTPUT: Bu bölümde DSN (Dataset Name) ile balayan bir tan
//*&SYSUID özel deikeni, JCL'yi çaltran kullancnn kimliini temsil eder.
//*ODEV1.OUTPUT ise dosya adnn bir parçasdr ve burada dosyann adn belirti
//*DISP=(NEW,CATLG,DELETE): Bu bölüm, dosya ilemi için kullanlacak yönergeleri
//*NEW: Dosyann yeni bir veri seti oluturulmasn salar.
//*CATLG: Oluturulan veri setinin bir katalogda listelenmesini salar.
//*DELETE: Dosyann ilem tamamlandktan sonra silinmesini salar.
//*SPACE=(CYL,(10,5)): Bu bölüm, dosya için ayrlan alann boyutunu belirtir.
//*CYL: Silindir birimiyle alann boyutunu belirtir.
//*(10,5): Dosya için ayrlan alann minimum ve maksimum boyutunu belirtir.
//*Buradaki deerler srasyla minimum 10 silindir ve yetmezde ek olarak 5 silin
//*17.JCL satr, PRTLINE adl bir çkt dosyasnn oluturulmasn, kataloglanm
//*Dosya, kullancnn kimliini temsil eden &SYSUID ile balayan bir ad ve ODEV
//*Dosya boyutu, minimum 10 silindir ve maksimum 5 silindir olarak belirlenmiti
// ELSE
// ENDIF
