10 REM String-handling program
20 REM Inputs a name, tests for validity
30 REM and breaks up into parts.
40 PRINT "Enter your full name"
50 INPUT name$

60 REM First check for non-English characters
70 LET flag = 0
80 FOR I = 1 TO LEN(name$)
90 LET ch$ = MID$(name$, I,1)
100 IF (ch$ >= "A" AND ch$ <= "z") OR ch$ = " " THEN 140
110 LET flag = 1
120 REM This forces the loop to stop
130 LET I = LEN(name$)
140 NEXT I
150 IF flag = 0 THEN 180
160 PRINT "Non-English letter,", ch$
170 GOTO 40

180 REM Jump to subroutine
190 LET return = 210
200 GOTO 1000
210 IF name$ = "" THEN 280
220 LET return = 240
230 GOTO 2000
240 LET N = N + 1
250 DIM out$(N)
260 LET out$(N) = word$
270 GOTO 180

280 REM Print out the name
285 PRINT "Name accepted"
290 FOR I = 1 TO N
300 PRINT out$(I) + " ";
310 NEXT I 
320 PRINT ""
330 GOTO 3000

1000 REM strips the leading space
1010 IF LEFT$(name$, 1) <> " " THEN return
1020 LET name$ = MID$(name$, 2, -1)
1030 GOTO 1010

2000 REM get the leading word and put it in word$
2010 LET word$ = ""
2020 LET ch$ = LEFT$(name$, 1)
2030 IF ch$ < "A" OR ch$ > "z" THEN return
2040 LET word$ = word$ + ch$
2050 LET name$ = MID$(name$, 2, -1)
2060 GOTO 2020

3000 REM END 
  
