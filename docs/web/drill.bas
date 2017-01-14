10 REM Program to drill simple arithmetic

20 PRINT "Arithmetic drill (q to quit)"
30 LET x = RND(10) + 1
40 LET y = RND(10) + 1
50 PRINT "What is", x, "+", y, "?"
60 INPUT answer$
70 IF VALLEN(answer$) <> 0 THEN 110
80 IF answer$ = "q" THEN 250
90 PRINT "Not a number"
100 GOTO 60
110 LET a = VAL(answer$)

120 REM This checks for non-space after the number
130 LET answer$ = MID$(answer$, VALLEN(answer$) + 1, -1)
140 IF answer$ = "" THEN 200
150 IF LEFT$(answer$, 1) = " " THEN 180
160 PRINT "Not an number"
170 GOTO 60
180 LET answer$ = MID$(answer$, 2, -1)
190 IF answer$ <> "" THEN 150

200 IF a = x + y THEN 230
210 PRINT "Wrong, it's", x + y
220 GOTO 30
230 PRINT "CORRECT"
240 GOTO 30
250 REM END
 
