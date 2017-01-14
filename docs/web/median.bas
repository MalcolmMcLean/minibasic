10 REM Median program.
20 LET N = 0
30 DIM array(N+1)
40 PRINT "Enter a number, q to quit"
50 INPUT line$
60 IF line$ = "q" THEN 100
70 LET N = N + 1
80 LET array(N) = VAL(line$)
90 GOTO 30
100 PRINT N, "numbers entered"
105 IF N = 0 THEN 1000
106 IF N =  1 THEN 210
110 REM Bubble sort the numbers
120 LET flag = 0
130 LET i = 1
140 IF array(i) <= array(i+1) THEN 190
150 LET flag = 1
160 LET temp = array(i)
170 LET array(i) = array(i+1)
180 LET array(i+1) = temp
190 LET i = i + 1
195 IF i < N THEN 140
200 IF flag = 1 THEN 120
210 REM print out the middle
220 IF N MOD 2 = 0 THEN 250
230 LET mid = array( (N + 1) / 2)
240 GOTO 270
250 LET mid = array(N/2) + array(N/2+1)
260 LET mid = mid/2
270 PRINT "Median", mid
1000 REM end  
