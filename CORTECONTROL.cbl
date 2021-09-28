      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORTECONTROL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO "JUGCOPAM.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA.
           SELECT SALIDA ASSIGN TO "SALIDAJUGCOPAM.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA.
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA.
           01  REG-ENTRADA.
               05  REG-JUGCOPAM-NOMBRE PIC X(20).
               05  REG-JUGCOPAM-CLUB   PIC X(16).
               05  REG-JUGCOPAM-PAIS   PIC X(03).
               05  REG-JUGCOPAM-POS    PIC X(03).
               05  REG-JUGCOPAM-NUM    PIC 9(02).
               05  REG-JUGCOPAM-GOLES  PIC 9(02).
               05  REG-JUGCOPAM-ASIST  PIC 9(02).
               05  REG-JUGCOPAM-TROJ   PIC 9(02).
               05  REG-JUGCOPAM-TAMA   PIC 9(02).

       FD  SALIDA.
           01  REG-SALIDA PIC X(110).
       WORKING-STORAGE SECTION.

       01  FILE-STATUS.
           05 FS-ENTRADA                     PIC XX.
               88 FS-ENTRADA-OK              VALUE '00'.
               88 FS-ENTRADA-FIN             VALUE '10'.
           05 FS-SALIDA                      PIC XX.
               88 FS-SALIDA-OK               VALUE '00'.


       01  VARIABLES.
           05 WS-CONTADOR-LECTURA PIC 99.
           05 WS-CONTADOR-ESCRIBO PIC 99.
           05 CONTADOR-JUGADORES PIC 9(4).
           05 CONTADOR-GOLES PIC 9(4).

       01  WS-ESTRUCTURASALIDA.
           05 FILLER PIC X(5) VALUE 'PAIS '.
           05 WS-PAISANT PIC X(3).
           05 FILLER PIC X(23) VALUE ' CANTIDAD DE JUGADORES '.
           05 TOTAL-JUGADORES-PAIS PIC 9(4).
           05 FILLER PIC X(20) VALUE ' CANTIDAD DE GOLES '.
           05 TOTAL-GOLES-PAIS PIC 9(4).

       PROCEDURE DIVISION.
       PERFORM 1000-INICIO.
       PERFORM 2000-PROCESO UNTIL FS-ENTRADA-FIN.
       PERFORM 9000-FINAL.


       1000-INICIO.
           PERFORM 1100-ABRIR-ARCHIVOS.
           PERFORM 5000-LEER-ENTRADA.
       1000-INICIO-EXIT. EXIT.

       1100-ABRIR-ARCHIVOS.
           OPEN INPUT ENTRADA
               IF NOT FS-ENTRADA-OK
                   DISPLAY 'ERROR APERTURA ENTRADA FS: ' FS-ENTRADA
                   PERFORM 9000-FINAL
               END-IF.
           OPEN OUTPUT SALIDA
               IF NOT FS-SALIDA-OK
                   DISPLAY 'ERROR APERTURA SALIDA FS: ' FS-SALIDA
                   PERFORM 9000-FINAL
               END-IF.
       1100-ABRIR-ARCHIVOS-EXIT.EXIT.

       2000-PROCESO.
           MOVE REG-JUGCOPAM-PAIS TO WS-PAISANT
           INITIALIZE CONTADOR-JUGADORES
           INITIALIZE CONTADOR-GOLES
           PERFORM 2100-CORTE-PAIS UNTIL
               REG-JUGCOPAM-PAIS <> WS-PAISANT OR FS-ENTRADA-FIN
           PERFORM 2200-TOTALIZAR.
       2000-PROCESO-EXIT.EXIT.

       2100-CORTE-PAIS.
           ADD 1 TO CONTADOR-JUGADORES
           ADD REG-JUGCOPAM-GOLES TO CONTADOR-GOLES
           PERFORM 5000-LEER-ENTRADA
           .
       2100-CORTE-PAIS-EXIT.EXIT.

       2200-TOTALIZAR.
           MOVE CONTADOR-JUGADORES TO TOTAL-JUGADORES-PAIS
           MOVE CONTADOR-GOLES TO TOTAL-GOLES-PAIS
           WRITE REG-SALIDA FROM WS-ESTRUCTURASALIDA
           ADD 1 TO WS-CONTADOR-ESCRIBO
           .
       2200-TOTALIZAR-EXIT.EXIT.


       5000-LEER-ENTRADA.
           READ ENTRADA
           ADD 1 TO WS-CONTADOR-LECTURA
           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                   ADD 1 TO WS-CONTADOR-LECTURA
               WHEN FS-ENTRADA-FIN
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'ERROR LECTURA ENTRADA FS ' FS-ENTRADA
                   PERFORM 9000-FINAL
           END-EVALUATE
           .
           5000-LEER-ENTRADA-EXIT.EXIT.
       9000-FINAL.
           CLOSE ENTRADA
           CLOSE SALIDA
           DISPLAY '***************************************************'
           DISPLAY 'TOTAL ARCHIVOS LEIDOS: ' WS-CONTADOR-ESCRIBO
           DISPLAY 'TOTAL ARCHIVOS ESCRITOS: ' WS-CONTADOR-LECTURA
           DISPLAY '***************************************************'
           .

       STOP RUN.
       END PROGRAM CORTECONTROL.
