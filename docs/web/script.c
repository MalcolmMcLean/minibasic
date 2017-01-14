/*
  driver file for MiniBasic.
  by Malcolm Mclean
  Leeds University
*/

#include <stdio.h>
#include <stdlib.h>

#include "basic.h"

char *loadfile(char *path);

/*
  here is a simple script to play with 
*/
char *script =
"10 REM Test Script\n"
"20 REM Tests the Interpreter\n"
"30 REM By Malcolm Mclean\n"
"35 PRINT \"HERE\" \n"
"40 PRINT INSTR(\"FRED\", \"ED\", 4)\n"
"50 PRINT VALLEN(\"12a\"), VALLEN(\"xyz\")\n"
"60 LET x = SQRT(3.0) * SQRT(3.0)\n"
"65 LET x = INT(x + 0.5)\n"
"70 PRINT MID$(\"1234567890\", x, -1)\n"
;

void usage(void)
{
  printf("MiniBasic: a BASIC interpreter\n");
  printf("usage:\n");
  printf("Basic <script>\n");
  printf("See documentation for BASIC syntax.\n");
  exit(EXIT_FAILURE);
}

/*
  call with the name of the Minibasic script file
*/
int main(int argc, char **argv)
{
  char *scr;

  if(argc == 1)
  {
	/* comment out usage call to run test script */
	usage();
    basic(script, stdin, stdout, stderr);
  }
  else
  {
	scr = loadfile(argv[1]);
	if(scr)
	{
	  basic(scr, stdin, stdout, stderr);
	  free(scr);
	}
  }

  return 0;
}

/*
  function to slurp in an ASCII file
  Params: path - path to file
  Returns: malloced string containing whole file
*/
char *loadfile(char *path)
{
  FILE *fp;
  int ch;
  long i = 0;
  long size = 0;
  char *answer;
  
  fp = fopen(path, "r");
  if(!fp)
  {
    printf("Can't open %s\n", path);
	return 0;
  }

  fseek(fp, 0, SEEK_END);
  size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  answer = malloc(size + 100);
  if(!answer)
  {
    printf("Out of memory\n");
    fclose(fp);
	return 0;
  }

  while( (ch = fgetc(fp)) != EOF)
    answer[i++] = ch;

  answer[i++] = 0;

  fclose(fp);

  return answer;
}