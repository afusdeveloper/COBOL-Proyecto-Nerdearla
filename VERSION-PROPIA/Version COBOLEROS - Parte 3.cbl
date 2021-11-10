       IDENTIFICATION DIVISION.
       PROGRAM-ID.                            EJERCICIO2.
       AUTHOR.                                AYMARA M FUSARO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

         01 WSC-CONSTANTES.
             05 WSC-SENIORITY.
                10 WSC-SENIOR                 PIC X(06) VALUE 'SENIOR'.
                10 WSC-SEMISR                 PIC X(06) VALUE 'SEMISR'.
                10 WSC-JUNIOR                 PIC X(06) VALUE 'JUNIOR'.

             05 WSC-SUELDOS.
                 10 WSC-SUELDO-SENIOR         PIC 9(06) VALUE 200000.
                 10 WSC-SUELDO-SEMISR         PIC 9(06) VALUE 160000.
                 10 WSC-SUELDO-JUNIOR         PIC 9(06) VALUE 100000.
                   
             05 WSC-TIT-1.
                 10 FILLER                    PIC X(32) VALUE SPACES.
                 10 WSC-ENCABEZADO            PIC X(16) VALUE 
                                              'REPORTE DE BECAS'.
                 10 FILLER                    PIC X(31) VALUE SPACES.
                   
             05 WSC-GUIONES.
                 10 FILLER                    PIC X(80) VALUE ALL '-'.
                   
             05 WSC-COLUMNAS.
                 10 FILLER                    PIC X     VALUE SPACE.
                 10 WSC-POSTULANTE2           PIC X(10) VALUE 
                                              'POSTULANTE'.
                 10 FILLER                    PIC X(04) VALUE SPACES.
                 10 WSC-EXP                   PIC X(03) VALUE 
                                              'EXP'.
                 10 FILLER                    PIC X(06) VALUE SPACES.
                 10 WSC-SENIORITY             PIC X(09) VALUE 
                                              'SENIORITY'.
                 10 FILLER                    PIC X(10) VALUE SPACES.
                 10 WSC-SUELDO2               PIC X(06) VALUE 
                                              'SUELDO'.
                 10 FILLER                    PIC X(10) VALUE SPACES.
                 10 WSC-ANUAL2                PIC X(05) VALUE 
                                              'ANUAL'.
                 10 FILLER                    PIC X(12) VALUE SPACES.
                 10 WSC-BONO2                 PIC X(04) VALUE 
                                              'BONO'.
                                                
             05 WSC-GUIONES2.
                 10 WSC-POSTULANTE3           PIC X(11) VALUE ALL '-'.
                 10 FILLER                    PIC X(03) VALUE SPACES.
                 10 WSC-EXP3                  PIC X(04) VALUE ALL '-'. 
                 10 FILLER                    PIC X(05) VALUE SPACES.
                 10 WSC-SENIORITY3            PIC X(10) VALUE ALL '-'. 
                 10 FILLER                    PIC X(08) VALUE SPACES.
                 10 WSC-SUELDO3               PIC X(08) VALUE ALL '-'.
                 10 FILLER                    PIC X(08) VALUE SPACES.
                 10 WSC-ANUAL3                PIC X(07) VALUE ALL '-'. 
                 10 FILLER                    PIC X(10) VALUE SPACES.
                 10 WSC-BONO3                 PIC X(06) VALUE ALL '-'. 

         01 WSV-VARIABLES.
             05 WSV-POSTULANTES.
                 10 WSV-POSTU1.
                     15 WSV-NOMBRE1           PIC X(05) VALUE 'Pedro'.
                     15 WSV-EXPERIENCIA1      PIC 9(02) VALUE 00. 
                     15 WSV-SUELDO1           PIC 9(06).
                     15 WSV-SUELDO-ANUAL1     PIC 9(07).
                     15 WSV-BONO1             PIC 9(06).
            
                 10 WSV-POSTU2.
                     15 WSV-NOMBRE2           PIC X(05) VALUE 'Sofia'.
                     15 WSV-EXPERIENCIA2      PIC 9(02) VALUE 07.
                     15 WSV-SUELDO2           PIC 9(06).
                     15 WSV-SUELDO-ANUAL2     PIC 9(07).
                     15 WSV-BONO2             PIC 9(06).
          
                 10 WSV-POSTU3.
                     15 WSV-NOMBRE3           PIC X(05) VALUE 'Lala'.
                     15 WSV-EXPERIENCIA3      PIC 9(02) VALUE 03.
                     15 WSV-SUELDO3           PIC 9(06).
                     15 WSV-SUELDO-ANUAL3     PIC 9(07).
                     15 WSV-BONO3             PIC 9(06).

             05 WSV-POSTULANTE-AUX.
                 10 WSV-NOMBRE-AUX            PIC X(05).
                 10 WSV-EXPERIENCIA-AUX       PIC 9(02).
                     88 WSS-EXP-JUNIOR                  VALUE 0 1 2.
                     88 WSS-EXP-SEMISR                  VALUE 3 4 5.
                 10 WSV-SUELDO-AUX            PIC 9(06).
                 10 WSV-SUELDO-ANUAL-AUX      PIC 9(07).
                 10 WSV-BONO-AUX              PIC 9(06).

  
       PROCEDURE DIVISION.

       00-CONTROL.
            PERFORM 10-INICIO.
            PERFORM 20-PROCESO.
            STOP RUN.
       00-CONTROL-END. EXIT.

       10-INICIO.
            DISPLAY WSC-TIT-1.
            DISPLAY WSC-GUIONES.
            DISPLAY WSC-COLUMNAS.
            DISPLAY WSC-GUIONES2.
            INITIALIZE WSV-POSTULANTE-AUX.
       10-INICIO-END. EXIT.

       20-PROCESO.
            MOVE WSV-POSTU1 TO WSV-POSTULANTE-AUX.
            PERFORM 25-EVALUAR.
            PERFORM 30-CALCULAR-SUELDO.
            
            MOVE WSV-POSTU2 TO WSV-POSTULANTE-AUX.
            PERFORM 25-EVALUAR.
            PERFORM 30-CALCULAR-SUELDO.
            
            MOVE WSV-POSTU3 TO WSV-POSTULANTE-AUX.
            PERFORM 25-EVALUAR.
            PERFORM 30-CALCULAR-SUELDO.
       20-PROCESO-END. EXIT.

       25-EVALUAR.
            EVALUATE TRUE
            WHEN WSS-EXP-JUNIOR 
                MOVE WSC-SUELDO-JUNIOR TO WSV-SUELDO-AUX
            WHEN WSS-EXP-SEMISR 
                MOVE WSC-SUELDO-SEMISR TO WSV-SUELDO-AUX
            WHEN OTHER 
                MOVE WSC-SUELDO-SENIOR TO WSV-SUELDO-AUX
            END-EVALUATE.
       25-EVALUAR-END. EXIT.

       30-CALCULAR-SUELDO.
           MULTIPLY 12 BY WSV-SUELDO-AUX 
                          GIVING WSV-SUELDO-ANUAL-AUX

            EVALUATE WSV-EXPERIENCIA-AUX
                WHEN 0
                    MOVE 0 TO WSV-BONO-AUX
                    PERFORM 35-ESCRIBIR-JUNIOR THRU 35-END
                WHEN 1
                    COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 1.5
                    PERFORM 35-ESCRIBIR-JUNIOR THRU 35-END
                WHEN 2
                WHEN 3
                    COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 2
                    PERFORM 40-ESCRIBIR-SEMISR THRU 40-ESCRIBIR-SEMISR
                WHEN 4
                WHEN 5
                WHEN 6
                    COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 2.5
                    PERFORM 45-ESCRIBIR-SENIOR THRU 45-END
                WHEN OTHER 
                    COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 3 
                    PERFORM 45-ESCRIBIR-SENIOR THRU 45-END
            END-EVALUATE.

            INITIALIZE WSV-POSTULANTE-AUX.
       30-CALCULAR-SUELDO-END. EXIT.
       
       35-ESCRIBIR-JUNIOR.
            DISPLAY ' '
                    WSV-NOMBRE-AUX          '          '
                    WSV-EXPERIENCIA-AUX     '         JUNIOR          '
                    WSV-SUELDO-AUX          '        '
                    WSV-SUELDO-ANUAL-AUX    '          '
                    WSV-BONO-AUX.    
       35-END. EXIT.
       
       40-ESCRIBIR-SEMISR.
            DISPLAY ' '
                    WSV-NOMBRE-AUX          '          '
                    WSV-EXPERIENCIA-AUX     '         SEMISR          '
                    WSV-SUELDO-AUX          '        '
                    WSV-SUELDO-ANUAL-AUX    '          '
                    WSV-BONO-AUX.    
       40-END. EXIT.
       
       45-ESCRIBIR-SENIOR.
            DISPLAY ' '
                    WSV-NOMBRE-AUX          '          '
                    WSV-EXPERIENCIA-AUX     '         SENIOR          '
                    WSV-SUELDO-AUX          '        '
                    WSV-SUELDO-ANUAL-AUX    '          '
                    WSV-BONO-AUX. 
       45-END. EXIT.