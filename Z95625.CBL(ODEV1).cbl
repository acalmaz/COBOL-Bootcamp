       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    ODEV1
       AUTHOR.        Melih Çalmaz
      *--------------------
       ENVIRONMENT DIVISION.
      *Aşağıdaki satır, programın girdi ve çıktıları bölümünün başlangıcını beli
       INPUT-OUTPUT SECTION.
      *Aşağıdaki satır, programın dosya kontrol bölümünün başlangıcını belirtir.
       FILE-CONTROL.
      *14. satırda, "PRINT-LINE" adlı bir dosya seçilir ve "PRTLINE" adıyla atam
      *15. satırda, "ACCT-REC" adlı bir dosya seçilir ve "ACCTREC" adıyla atamas
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *SELECT yan tümcesi dahili bir dosya adı oluşturur
      *ASSIGN yan tümcesi, harici bir veri kaynağı için bir ad oluşturur,
      *z/OS tarafından kullanılan JCL DDNAME ile ilişkilidir
      *Örneğin. ACCTREC, CBL0001J JCL dosyasında &SYSUID..DATA'ya bağlanmıştır.
      *burada &SYSUID. z/OS kullanıcı kimliğiniz anlamına gelir
      *Örneğin. kullanıcı kimliğiniz Z95625 ise,
      *ACTREC için kullanılan veri seti Z95625.DATA'dır.
       DATA DIVISION.
      *-------------
       FILE SECTION.
      *Alt satır, "PRINT-LINE" adlı dosyanın formatının "F" (fixed) olduğunu bel
       FD  PRINT-LINE RECORDING MODE F.
      *"PRINT-REC" adlı bir kayıt tanımlanır. Bu kayıt, "PRINT-LINE" dosyasının 
      *32. satır, "ACCT-NO-O" adlı bir alan tanımlar. Bu alan, 8 karakter uzunlu
      *33. satır, "ACCT-LIMIT-O" adlı bir alan tanımlar. Bu alan, dolar simgeler
      *ve "PRINT-REC" kaydının bir parçasıdır.
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
      *36. Satır,"LAST-NAME-O" adlı bir alan tanımlar. Bu alan, 20 karakter uzun
           05  LAST-NAME-O    PIC X(20).
           05  FIRST-NAME-O   PIC X(15).
           05  COMMENTS-O     PIC X(50).
      *05 seviyesi 01 seviyesinden düşük olduğu için tüm değişkenler PRINT-REC'e
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
      *PIC S9(7)v99 -- yedi hane artı bir işaret hanesi değeri
      *COMP-3 -- paketlenmiş BCD (ikili kodlu ondalık) gösterimi
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
      *59.satır, "LASTREC" adlı bir alan tanımlar. Bu alan, "SPACE" değeriyle ba
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
      *------------------
       PROCEDURE DIVISION.
      *------------------
      *66.satır, "ACCT-REC" adlı dosyanın giriş olarak açılmasını sağlar.
      *67. satır, "PRINT-LINE" adlı dosyanın çıkış olarak açılmasını sağlar.
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *69. satır, bir sonraki kaydı okuyan bir işlemi başlatır.
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
      *"LASTREC" 'in 'Y' olmadığı sürece bir döngünün devam etmesini sağlar.
      * END-PERFORM, döngünün sonunu belirtir.
      * Döngü bir sonraki satırda PERFORM UNTIL ile başlıyor.
           PERFORM UNTIL LASTREC = 'Y'
               PERFORM WRITE-RECORD
               PERFORM READ-RECORD
           END-PERFORM
           .
      *Bu satır, dosyaların kapatılmasını ve programın sonlanmasını sağlar. 
      *GOBACK programın sonlandığını belirtir.
       CLOSE-STOP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
       READ-RECORD.
           READ ACCT-REC
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           MOVE COMMENTS     TO  COMMENTS-O.
           WRITE PRINT-REC.
      *READ-RECORD.: Bu bölüm, bir kaydı okumak için ACCT-REC dosyasını okur.
      *READ ACCT-REC: ACCT-REC dosyasından bir kaydı okur.
      *AT END: Dosyanın sonuna gelindiğinde gerçekleşecek olan bir kontrol nokta
      *MOVE 'Y' TO LASTREC: Dosyanın sonuna gelindiğinde, LASTREC değişkenine 'Y
      *Bu, döngünün sonlanmasını sağlayacak bir kontrol mekanizmasıdır.
      *END-READ.: Dosya okuma işlemini sonlandırır.
      *WRITE-RECORD.: Bu bölüm, bir kaydı PRINT-LINE dosyasına yazmak için kulla
      *MOVE ACCT-NO TO ACCT-NO-O: ACCT-NO alanının değeri ACCT-NO-O alanına taşı
      *MOVE ACCT-LIMIT TO ACCT-LIMIT-O: ACCT-LIMIT alanının değeri ACCT-LIMIT-O 
      *MOVE ACCT-BALANCE TO ACCT-BALANCE-O: ACCT-BALANCE alanının değeri ACCT-BA
      *MOVE LAST-NAME TO LAST-NAME-O: LAST-NAME alanının değeri LAST-NAME-O alan
      *MOVE FIRST-NAME TO FIRST-NAME-O: FIRST-NAME alanının değeri FIRST-NAME-O 
      *MOVE COMMENTS TO COMMENTS-O: COMMENTS alanının değeri COMMENTS-O alanına 
      *WRITE PRINT-REC.: PRINT-REC yapısındaki kaydı PRINT-LINE dosyasına yazmak
      *Bu bölümler, ACCT-REC dosyasından bir kaydı okuyarak ilgili alanları PRIN
      *Döngü, dosyanın sonuna gelindiğinde sonlanır.