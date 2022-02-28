* ----------------------------------------------------------
* CBDEM1 IS A SIMPLE EXAMPLE PROGRAM WHICH ADDS NEW EMPLOYEE
* ROWS TO THE PERSONNEL DATA BASE. CHECKING IS DONE TO
* INSURE THE INTEGRITY OF THE DATA BASE. EMPLOYEE NUMBERS
* ARE AUTOMATICALLY SELECTED USING THE CURRENT MAXIMUM
* EMPLOYEE NUMBER AS THE START. DUPLICATE NUMBERS ARE SKIPPED.
* THE PROGRAM QUERIES THE USER FOR DATA AS FOLLOWS:
*
*		 Enter employee name  :
*		 Enter employee job   :
*		 Enter employee salary:
*		 Enter employee dept  :
*
* TO EXIT THE PROGRAM, ENTER A CARRIAGE RETURN AT THE
* PROMPT FOR EMPLOYEE NAME. IF THE ROW IS SUCCESSFULLY 
* INSERTED, THE FOLLOWING IS PRINTED:
*
* ENAME added to DNAME department as employee # NNNNN
*
* THE MAXIMUM LENGTHS OF THE 'ENAME', 'JOB', AND 'DNAME'
* COLUMNS WILL BE DETERMINED BY THE ODESCR CALL.
*-----------------------------------------------------------

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  CBDEM1.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  LDA.
     02   LDA-V2RC       PIC S9(4) COMP.
     02   FILLER         PIC X(10).
     02   LDA-RC         PIC S9(4) COMP.
     02   FILLER         PIC X(50).
 01  HDA                 PIC X(512).

 01  CURSOR-1.
     02   C-V2RC         PIC S9(4) COMP.
     02   C-TYPE         PIC S9(4) COMP.
     02   C-ROWS         PIC S9(9) COMP.
     02   C-OFFS         PIC S9(4) COMP.
     02   C-FNC          PIC S9(4) COMP.      
     02   C-RC           PIC S9(4) COMP.
     02   FILLER         PIC X(50).
 01  CURSOR-2.
     02   C-V2RC         PIC S9(4) COMP.
     02   C-TYPE         PIC S9(4) COMP.
     02   C-ROWS         PIC S9(9) COMP.
     02   C-OFFS         PIC S9(4) COMP.
     02   C-FNC          PIC S9(4) COMP.
     02   C-RC           PIC S9(4) COMP.
     02   FILLER         PIC X(50).

 77   USER-ID            PIC X(5)  VALUE "SCOTT".
 77   USER-ID-L          PIC S9(9) VALUE 5 COMP.
 77   PSW                PIC X(5)  VALUE "tiger".
 77   PSW-L              PIC S9(9) VALUE 5 COMP.
 77   CONN               PIC S9(9) VALUE 0 COMP.
 77   CONN-L             PIC S9(9) VALUE 0 COMP.
 77   CONN-MODE          PIC S9(9) VALUE 0 COMP.
 
 77   SQL-SEL            PIC X(38) VALUE
         "SELECT DNAME FROM DEPT WHERE DEPTNO=:1".
 77   SQL-SEL-L          PIC S9(9) VALUE 38 COMP.

 77   SQL-INS            PIC X(150) VALUE
         "INSERT INTO EMP (EMPNO,ENAME,JOB,SAL,DEPTNO)
-        " VALUES (:EMPNO,:ENAME,:JOB,:SAL,:DEPTNO)".
 77   SQL-INS-L          PIC S9(9) VALUE 150 COMP.

 77   SQL-SELMAX         PIC X(33) VALUE
         "SELECT NVL(MAX(EMPNO),0) FROM EMP".
 77   SQL-SELMAX-L       PIC S9(9) VALUE 33 COMP.

 77   SQL-SELEMP         PIC X(26) VALUE
         "SELECT ENAME,JOB FROM EMP".
 77   SQL-SELEMP-L       PIC S9(9) VALUE 26 COMP.
 
 77   EMPNO              PIC S9(9) COMP.
 77   EMPNO-D            PIC ZZZZ9.
 77   ENAME              PIC X(12).
 77   JOB                PIC X(12).
 77   SAL                PIC X(10).
 77   DEPTNO             PIC X(10).
 77   FMT                PIC X(6).
 77   CBUF               PIC X(10).
 77   DNAME              PIC X(15).

 77   ENAME-L            PIC S9(9) VALUE 12 COMP.
 77   ENAME-SIZE         PIC S9(4) COMP.
 77   JOB-L              PIC S9(9) VALUE 12 COMP.
 77   JOB-SIZE           PIC S9(4) COMP.
 77   SAL-L              PIC S9(9) VALUE 10 COMP.
 77   DEPTNO-L           PIC S9(9) VALUE 10 COMP.
 77   DNAME-L            PIC S9(9) VALUE 15 COMP.
 77   DNAME-SIZE         PIC S9(4) COMP.
 77   EMPNO-N            PIC X(6) VALUE ":EMPNO".
 77   ENAME-N            PIC X(6) VALUE ":ENAME".
 77   JOB-N              PIC X(4) VALUE ":JOB".
 77   SAL-N              PIC X(4) VALUE ":SAL".
 77   DEPTNO-N           PIC X(7) VALUE ":DEPTNO".
 77   EMPNO-N-L          PIC S9(9) VALUE 6 COMP.
 77   ENAME-N-L          PIC S9(9) VALUE 6 COMP.
 77   JOB-N-L            PIC S9(9) VALUE 4 COMP.
 77   SAL-N-L            PIC S9(9) VALUE 4 COMP.
 77   DEPTNO-N-L         PIC S9(9) VALUE 7 COMP.
 
 77   INTEGER            PIC S9(9) COMP VALUE 3.
 77   ASC                PIC S9(9) COMP VALUE 1.
 77   ZERO-A             PIC S9(9) COMP VALUE 0.
 77   ZERO-B             PIC S9(4) COMP VALUE 0.
 77   ONE                PIC S9(9) COMP VALUE 1.
 77   TWO                PIC S9(9) COMP VALUE 2.
 77   FOUR               PIC S9(9) COMP VALUE 4.
 77   SIX                PIC S9(9) COMP VALUE 6.
 77   EIGHT              PIC S9(9) COMP VALUE 8.
 77   ERR-RC             PIC S9(4) COMP.
 77   ERR-FNC            PIC S9(4) COMP.
 77   ERR-RC-D           PIC ZZZ9.
 77   ERR-FNC-D          PIC ZZ9.
 77   MSGBUF             PIC X(160).
 77   MSGBUF-L           PIC S9(9) COMP VALUE 160. 

 77   ASK-EMP            PIC X(25) VALUE
                           "Enter employee name: ".
 77   ASK-JOB            PIC X(25) VALUE
                           "Enter employee job: ".
 77   ASK-SAL            PIC X(25) VALUE
                           "Enter employee salary: ".
 77   ASK-DEPTNO         PIC X(25) VALUE
                           "Enter employee dept: ".

 PROCEDURE DIVISION.
 BEGIN.
