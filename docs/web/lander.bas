10 REM Lunar lander program.

20 LET dist = 100
30 LET v = 1
40 LET fuel = 1000
50 LET mass = 1000

60 PRINT "You are a in control of a lunar lander."
70 PRINT "You are drifiting towards the surface of the moon."
80 PRINT "Each turn you must decide how much fuel to burn."
90 PRINT "To accelerate enter a positive number, to decelerate a negative"

100 PRINT "Distance", dist, "km", "velocity", v, "km/s", "Fuel", fuel
110 INPUT burn
115 IF ABS(burn) <= fuel THEN 120
116 PRINT "You don't have that much fuel"
117 GOTO 100
120 LET v = v + burn * 10 / (fuel + mass)
130 LET fuel = fuel - ABS(burn)
140 LET dist = dist - v
150 IF dist > 0 THEN 100
160 PRINT "You have hit the surface"
170 IF v < 3 THEN 210
180 PRINT "Hit surface too fast (", v,")km/s"
190 PRINT "You Crash"
200 GOTO 220
210 PRINT "Well done"
220 REM END



