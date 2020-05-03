/*****************************************************************
*                           Mini BASIC                           *
*                        by Malcolm McLean                       *
*                           version 1.0                          *
*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>

/* tokens defined */
#define EOL 0 
#define VALUE 1
#define PI 2
#define E 3
#define UNARYMINUS 4
#define SIN 5
#define COS 6
#define TAN 7
#define LN 8
#define POW 9
#define DIV 10
#define MULT 11
#define OPAREN 12
#define CPAREN 13
#define PLUS 14
#define MINUS 15
#define SHRIEK 16
#define COMMA 17
#define SQRT 18
#define SPACE 19
#define ERROR 20
#define EOS 21
#define EQUALS 22
#define STRID 23
#define FLTID 24
#define DIMFLTID 25
#define DIMSTRID 26
#define QUOTE 27
#define GREATER 28
#define LESS 29

#define PRINT 100
#define LET 101
#define DIM 102
#define IF 103
#define THEN 104
#define AND 105
#define OR 106
#define GOTO 107
#define INPUT 108
#define REM 109
#define FOR 110
#define TO 111
#define NEXT 112
#define STEP 113

#define MOD 200
#define ABS 201
#define LEN 202
#define ASCII 203
#define ASIN 204
#define ACOS 205
#define ATAN 206
#define INT 207
#define RND 208
#define VAL 209

#define CHRSTRING 300
#define STRSTRING 301
#define LEFTSTRING 302
#define RIGHTSTRING 303
#define MIDSTRING 304
#define STRINGSTRING 305

/* relational operators defined */

#define ROP_EQ 1
#define ROP_NEQ 2
#define ROP_LT 3
#define ROP_LTE 4
#define ROP_GT 5
#define ROP_GTE 6

/* error codes (in BASIC script) defined */
#define ERR_CLEAR 0
#define ERR_SYNTAX 1
#define ERR_OUTOFMEMORY 2
#define ERR_IDTOOLONG 3
#define ERR_NOSUCHVARIABLE 4
#define ERR_BADSUBSCRIPT 5
#define ERR_TOOMANYDIMS 6
#define ERR_TOOMANYINITS 7
#define ERR_BADTYPE 8
#define ERR_TOOMANYFORS 9
#define ERR_NONEXT 10
#define ERR_NOFOR 11
#define ERR_DIVIDEBYZERO 12
#define ERR_NEGLOG 13
#define ERR_NEGSQRT 14
#define ERR_EOF 15
#define ERR_ILLEGALOFFSET 16
#define ERR_TYPEMISMATCH 17

typedef struct
{
  int no;
  const char *str;
}LINE;

typedef struct
{
  char id[32];
  double dval;
  int ival;
  char *sval;
} VARIABLE;

typedef struct
{
  char id[32];
  int type;
  int ndims;
  int dim[5];
  char **str;
  double *dval;
} DIMVAR;

typedef struct
{
  int type;   
  char **sval;
  double *dval;
  int *ival;
} LVALUE;

typedef struct
{
  char id[32];
  int nextline;
  double toval;
  double step;
} FORLOOP;

static FORLOOP forstack[32];
static int nfors;

static VARIABLE *variables;
static int nvariables;

static DIMVAR *dimvariables;
static int ndimvariables;

static LINE *lines;
static int nlines;

static FILE *fpin;
static FILE *fpout;
static FILE *fperr;

static const char *string;        /* string we are parsing */
static int token;                 /* current token (lookahead) */
static int errorflag;             /* set when error in input encountered */


static int setup(const char *script);
static void cleanup(void);

static void reporterror(int lineno);
static int findline(int no);

static int line(void);
static void doprint(void);
static void dolet(void);
static void dodim(void);
static int doif(void);
static int dogoto(void);
static void doinput(void);
static void dorem(void);
static int dofor(void);
static int donext(void);

static void lvalue(LVALUE *lv);

static int boolexpr(void);
static int boolfactor(void);
static int relop(void);


static double expr(void);
static double term(void);
static double factor(void);
static double variable(void);
static double dimvariable(void);


static VARIABLE *findvariable(const char *id);
static DIMVAR *finddimvar(const char *id);
static DIMVAR *dimension(const char *id, int ndims, ...);
static void *getdimvar(DIMVAR *dv, ...);
static VARIABLE *addfloat(const char *id);
static VARIABLE *addstring(const char *id);
static DIMVAR *adddimvar(const char *id);

static char *stringexpr(void);
static char *chrstring(void);
static char *strstring(void);
static char *leftstring(void);
static char *rightstring(void);
static char *midstring(void);
static char *stringstring(void);
static char *stringdimvar(void);
static char *stringvar(void);
static char *stringliteral(void);

static void match(int tok);
static void seterror(int errorcode);
static int getnextline(const char *str);
static int gettoken(const char *str);
static int tokenlen(const char *str, int token);

static int isstring(int token);
static double getvalue(const char *str, int *len);
static void getid(const char *str, char *out, int *len);

static void mystrgrablit(char *dest, const char *src);
static char *mystrend(const char *str, char quote);
static int mystrcount(const char *str, char ch);
static char *mystrdup(const char *str);
static char *mystrconcat(const char *str, const char *cat);
static double factorial(double x);

int basic(const char *script, FILE *in, FILE *out, FILE *err)
{
  int curline = 0;
  int nextline;
  int answer = 0;

  fpin = in;
  fpout = out;
  fperr = err;

  if( setup(script) == -1 )
    return 1;
  
  while(curline != -1)
  {
    string = lines[curline].str;
	token = gettoken(string);
	errorflag = 0;

	nextline = line();
	if(errorflag)
	{
      reporterror(lines[curline].no);
	  break;
	}

	if(nextline == -1)
	  break;

	if(nextline == 0)
	{
      curline++;
	  if(curline == nlines)
	    break;
    }
	else
    {
      curline = findline(nextline);
	  if(curline == -1)
	  {
	    fprintf(fperr, "line %d not found\n", nextline);
		answer = 1;
		break;
	  }
    }
  }

  cleanup();
  
  return 0;
}

/*
  Sets up all our globals, including the list of lines.
  Params: script - the script passed by the user
  Returns: 0 on success, -1 on failure
*/
static int setup(const char *script)
{
  int i;

  nlines = mystrcount(script, '\n');
  lines = malloc(nlines * sizeof(LINE));
  if(!lines)
  {
	if(fperr)
      fprintf(fperr, "Out of memory\n");
	return -1;
  }
  for(i=0;i<nlines;i++)
  {
	if(isdigit(*script))
	{
      lines[i].str = script;
	  lines[i].no = strtol(script, 0, 10);
	}
	else
	{
	  i--;
	  nlines--;
	}
	script = strchr(script, '\n');
	script++;
  }
  if(!nlines)
  {
	if(fperr)
	  fprintf(fperr, "Can't read program\n");
    free(lines);
	return -1;
  }

  for(i=1;i<nlines;i++)
	if(lines[i].no <= lines[i-1].no)
	{
	  if(fperr)
		fprintf(fperr, "program lines %d and %d not in order\n", 
		  lines[i-1].no, lines[i].no);
	  free(lines);
	  return -1;
	}

  nvariables = 0;
  variables = 0;

  dimvariables = 0;
  ndimvariables = 0;

  return 0;
}

/*
  frees all the memory we have allocated
*/
static void cleanup(void)
{
  int i;
  int ii;
  int size;

  for(i=0;i<nvariables;i++)
	if(variables[i].sval)
	  free(variables[i].sval);
  if(variables)
	  free(variables);
  variables = 0;
  nvariables = 0;

  for(i=0;i<ndimvariables;i++)
  {
    if(dimvariables[i].type == STRID)
	{
	  if(dimvariables[i].str)
	  {
		size = 1;
        for(ii=0;ii<dimvariables[i].ndims;ii++)
		  size *= dimvariables[i].dim[ii];
	    for(ii=0;ii<size;ii++)
		  if(dimvariables[i].str[ii])
		    free(dimvariables[i].str[ii]);
		free(dimvariables[i].str);
	  }
	}
	else
	  if(dimvariables[i].dval)
		free(dimvariables[i].dval);
  }

  if(dimvariables)
	free(dimvariables);
 
  dimvariables = 0;
  ndimvariables = 0;

  if(lines)
	free(lines);

  lines = 0;
  nlines = 0;
  
}

/*
  error report function.
  for reporting errors in the user's script.
  checks the global errorflag.
  writes to fperr.
  Params: lineno - the line on which the error occurred
*/
static void reporterror(int lineno)
{
  if(!fperr)
	return;

  switch(errorflag)
  {
    case ERR_CLEAR:
	  assert(0);
	  break;
	case ERR_SYNTAX:
	  fprintf(fperr, "Syntax error line %d\n", lineno);
	  break;
	case ERR_OUTOFMEMORY:
	  fprintf(fperr, "Out of memory line %d\n", lineno);
	  break;
	case ERR_IDTOOLONG:
	  fprintf(fperr, "Identifier too long line %d\n", lineno);
	  break;
	case ERR_NOSUCHVARIABLE:
	  fprintf(fperr, "No such variable line %d\n", lineno);
	  break;
	case ERR_BADSUBSCRIPT:
	  fprintf(fperr, "Bad subscript line %d\n", lineno);
	  break;
	case ERR_TOOMANYDIMS:
	  fprintf(fperr, "Too many dimensions line %d\n", lineno);
	  break;
	case ERR_TOOMANYINITS:
	  fprintf(fperr, "Too many initialisers line %d\n", lineno);
	  break;
	case ERR_BADTYPE:
	  fprintf(fperr, "Illegal type line %d\n", lineno);
	  break;
	case ERR_TOOMANYFORS:
	  fprintf(fperr, "Too many nested fors line %d\n", lineno);
	  break;
	case ERR_NONEXT:
	  fprintf(fperr, "For without matching next line %d\n", lineno);
	  break;
	case ERR_NOFOR:
	  fprintf(fperr, "Next without matching for line %d\n", lineno);
	  break;
	case ERR_DIVIDEBYZERO:
	  fprintf(fperr, "Divide by zero lne %d\n", lineno);
	  break;
	case ERR_NEGLOG:
	  fprintf(fperr, "Negative logarithm line %d\n", lineno);
	  break;
	case ERR_NEGSQRT:
	  fprintf(fperr, "Negative square root line %d\n", lineno);
	  break;
	case ERR_EOF:
	  fprintf(fperr, "End of input file %d\n", lineno);
	  break;
	case ERR_ILLEGALOFFSET:
	  fprintf(fperr, "Illegal offset line %d\n", lineno);
	  break;
	case ERR_TYPEMISMATCH:
	  fprintf(fperr, "Type mismatch line %d\n", lineno);
	  break;
	default:
	  fprintf(fperr, "ERROR line %d\n", lineno);
	  break;
  }
}

/*
  binary search for a line
  Params: no - line number to find
  Returns: index of the line, or -1 on fail.
*/
static int findline(int no)
{
  int high;
  int low;
  int mid;

  low = 0;
  high = nlines-1;
  while(high > low + 1)
  {
    mid = (high + low)/2;
	if(lines[mid].no == no)
	  return mid;
	if(lines[mid].no > no)
	  high = mid;
	else
	  low = mid;
  }

  if(lines[low].no == no)
	mid = low;
  else if(lines[high].no == no)
	mid = high;
  else
	mid = -1;

  return mid;
}

/*
  Parse a line. High level parse function
*/
static int line(void)
{
  int answer = 0;

  match(VALUE);

  switch(token)
  {
    case PRINT:
	  doprint();
	  break;
    case LET:
	  dolet();
	  break;
	case DIM:
	  dodim();
	  break;
	case IF:
	  answer = doif();
	  break;
	case GOTO:
	  answer = dogoto();
	  break;
	case INPUT:
	  doinput();
	  break;
	case REM:
	  dorem();
	  return 0;
	  break;
	case FOR:
	  answer = dofor();
	  break;
	case NEXT:
	  answer = donext();
	  break;
	default:
	  seterror(ERR_SYNTAX);
	  break;
  }

  if(token != EOS)
	match(VALUE);

  return answer;
}

/*
  the PRINT statement
*/
static void doprint(void)
{
  char *str;
  double x;

  match(PRINT);

  while(1)
  {
    if(isstring(token))
	{
      str = stringexpr();
	  if(str)
	  {
        fprintf(fpout, "%s", str);
        free(str);
	  }
	}
	else
	{
	  x = expr();
	  fprintf(fpout, "%g", x);
	}
	if(token == COMMA)
	{
	  fprintf(fpout, " ");
	  match(COMMA);
	}
	else
	  break;
  }
  fprintf(fpout, "\n");
}

/*
  the LET statement
*/
static void dolet(void)
{
  LVALUE lv;
  char *temp;

  match(LET);
  lvalue(&lv);
  match(EQUALS);
  switch(lv.type)
  {
    case FLTID:
	  *lv.dval = expr();
	  break;
    case STRID:
	  temp = *lv.sval;
	  *lv.sval = stringexpr();
	  if(temp)
		free(temp);
	  break;
  }
}

/*
  the DIM statement
*/
static void dodim(void)
{
  int ndims = 0;
  double dims[6];
  char name[32];
  int len;
  DIMVAR *dimvar;
  int i;
  int size = 1;

  match(DIM);

  switch(token)
  {
    case DIMFLTID:
	case DIMSTRID:
      getid(string, name, &len);
	  match(token);
	  dims[ndims++] = expr();
	  while(token == COMMA)
	  {
	    match(COMMA);
	    dims[ndims++] = expr();
		if(ndims > 5)
		{
		  seterror(ERR_TOOMANYDIMS);
		  return;
		}
	  } 

	  match(CPAREN);
	  
	  for(i=0;i<ndims;i++)
	  {
	    if(dims[i] < 0 || dims[i] != (int) dims[i])
		{
		  seterror(ERR_BADSUBSCRIPT);
		  return;
		}
	  }
	  switch(ndims)
	  {
	    case 1:
		  dimvar = dimension(name, 1, (int) dims[0]);
		  break;
		case 2:
		  dimvar = dimension(name, 2, (int) dims[0], (int) dims[1]);
		  break;
		case 3:
		  dimvar = dimension(name, 3, (int) dims[0], (int) dims[1], (int) dims[2]);
		  break;
		case 4:
		  dimvar = dimension(name, 4, (int) dims[0], (int) dims[1], (int) dims[2], (int) dims[3]);
		  break;
		case 5:
		  dimvar = dimension(name, 5, (int) dims[0], (int) dims[1], (int) dims[2], (int) dims[3], (int) dims[4]);
		  break;
	  }
	  break;
	default:
	    seterror(ERR_SYNTAX);
	    return;
  }
  if(dimvar == 0)
  {
	/* out of memory */
	seterror(ERR_OUTOFMEMORY);
	return;
  }


  if(token == EQUALS)
  {
    match(EQUALS);

	for(i=0;i<dimvar->ndims;i++)
	  size *= dimvar->dim[i];

	switch(dimvar->type)
	{
      case FLTID:
		i = 0;
	    dimvar->dval[i++] = expr();
		while(token == COMMA && i < size)
		{
		  match(COMMA);
		  dimvar->dval[i++] = expr();
		  if(errorflag)
			break;
		}
		break;
	  case STRID:
		i = 0;
		if(dimvar->str[i])
		  free(dimvar->str[i]);
		dimvar->str[i++] = stringexpr();

		while(token == COMMA && i < size)
		{
		  match(COMMA);
		  if(dimvar->str[i])
		    free(dimvar->str[i]);
		  dimvar->str[i++] = stringexpr();
		  if(errorflag)
			break;
		}
		break;
	}
	
	if(token == COMMA)
	  seterror(ERR_TOOMANYINITS);
  }

}

/*
  the IF statement.
  if jump taken, returns new line no, else returns 0
*/
static int doif(void)
{
  int condition;
  int jump;

  match(IF);
  condition = boolexpr();
  match(THEN);
  jump = (int) expr();
  if(condition)
	return jump;
  else
	return 0;
}

/*
  the GOTO satement
  returns new line number
*/
static int dogoto(void)
{
  match(GOTO);
  return (int) expr();
}

/*
  The FOR statement.

  Pushes the for stack.
  Returns line to jump to, or -1 to end program

*/
static int dofor(void)
{
  LVALUE lv;
  char id[32];
  char nextid[32];
  int len;
  double initval;
  double toval;
  double stepval;
  const char *savestring;
  int answer;

  match(FOR);
  getid(string, id, &len);

  lvalue(&lv);
  if(lv.type != FLTID)
  {
    seterror(ERR_BADTYPE);
	return -1;
  }
  match(EQUALS);
  initval = expr();
  match(TO);
  toval = expr();
  if(token == STEP)
  {
    match(STEP);
	stepval = expr();
  }
  else
    stepval = 1.0;

  *lv.dval = initval;

  if(nfors > 31)
  {
	seterror(ERR_TOOMANYFORS);
	return -1;
  }

  if(stepval < 0 && initval < toval || stepval > 0 && initval > toval)
  {
	savestring = string;
    while(string = strchr(string, '\n'))
	{
      errorflag = 0;
	  token = gettoken(string);
	  match(VALUE);
	  if(token == NEXT)
	  {
	    match(NEXT);
		if(token == FLTID || token == DIMFLTID)
		{
          getid(string, nextid, &len);
		  if(!strcmp(id, nextid))
		  {
			answer = getnextline(string);
			string = savestring;
			token = gettoken(string);
			return answer ? answer : -1;
		  }
		}
	  }
	}

	seterror(ERR_NONEXT);
	return -1;
  }
  else
  {
	strcpy(forstack[nfors].id, id);
	forstack[nfors].nextline = getnextline(string);
	forstack[nfors].step = stepval;
	forstack[nfors].toval = toval;
	nfors++;
    return 0;
  }

}

/*
  the NEXT statement
  updates the counting index, and returns line to jump to
*/
static int donext(void)
{
  char id[32];
  int len;
  LVALUE lv;

  match(NEXT);

  if(nfors)
  {
    getid(string, id, &len);
    lvalue(&lv);
    *lv.dval += forstack[nfors-1].step;
	if( (forstack[nfors-1].step < 0 && *lv.dval < forstack[nfors-1].toval) ||
		(forstack[nfors-1].step > 0 && *lv.dval > forstack[nfors-1].toval) )
	{
	  nfors--;
	  return 0;
	}
	else
	{
      return forstack[nfors-1].nextline;
	}
  }
  else
  {
    seterror(ERR_NOFOR);
	return -1;
  }
}


/*
  the INPUT statement
*/
static void doinput(void)
{
  LVALUE lv;
  char buff[1024];
  char *end;

  match(INPUT);
  lvalue(&lv);

  switch(lv.type)
  {
  case FLTID:
	while(fscanf(fpin, "%lf", lv.dval) != 1)
	{
	  fgetc(fpin);
	  if(feof(fpin))
	  {
	    seterror(ERR_EOF);
	    return;
	  }
	}
	break;
  case STRID:
	if(*lv.sval)
	{
	  free(*lv.sval);
	  *lv.sval = 0;
	}
	fgets(buff, sizeof(buff), fpin);
	end = strchr(buff, '\n');
	if(!end)
	{
	  errorflag = 1;
	  return;
	}
	*end = 0;
	*lv.sval = mystrdup(buff);
	if(!*lv.sval)
	{
      seterror(ERR_OUTOFMEMORY);
	  return;
	}
	break;
  default:
	  return;
  }
}

/*
  the REM statement.
  Note is unique as the rest of the line is not parsed

*/
static void dorem(void)
{
  match(REM);
  return;
}

/*
  Get an lvalue from the environment
  Params: lv - structure to fill.
  Notes: missing variables (but not out of range subscripts)
         are added to the variable list.
*/
static void lvalue(LVALUE *lv)
{
  char name[32];
  int len;
  VARIABLE *var;
  DIMVAR *dimvar;
  int index[5];
  void *valptr = 0;
  int type;
  
  lv->type = ERROR;
  lv->dval = 0;
  lv->sval = 0;
  lv->ival = 0;

  switch(token)
  {
    case FLTID:
	  getid(string, name, &len);
	  match(FLTID);
	  var = findvariable(name);
	  if(!var)
		var = addfloat(name);
	  if(!var)
	  {
	    seterror(ERR_OUTOFMEMORY);
		return;
	  }
	  lv->type = FLTID;
	  lv->dval = &var->dval;
	  lv->sval = 0;
	  break;
    case STRID:
	  getid(string, name, &len);
	  match(STRID);
	  var = findvariable(name);
	  if(!var)
		var = addstring(name);
	  if(!var)
	  {
	    seterror(ERR_OUTOFMEMORY);
		return;
	  }
	  lv->type = STRID;
	  lv->sval = &var->sval;
	  lv->dval = 0;
	  break;
	case DIMFLTID:
	case DIMSTRID:
		type = (token == DIMFLTID) ? FLTID : STRID;
	  getid(string, name, &len);
	  match(token);
	  dimvar = finddimvar(name);
	  if(dimvar)
	  {
	    switch(dimvar->ndims)
		{
		  case 1:
			index[0] = (int) expr();
			if(errorflag == 0)
              valptr = getdimvar(dimvar, index[0]);
			break;
		  case 2:
			index[0] = (int) expr();
			match(COMMA);
			index[1] = (int) expr();
			if(errorflag == 0)
			  valptr = getdimvar(dimvar, index[0], index[1]);
			break;
		  case 3:
			index[0] = (int) expr();
			match(COMMA);
			index[1] = (int) expr();
			match(COMMA);
            index[2] = (int) expr();
			if(errorflag == 0)
			  valptr = getdimvar(dimvar, index[0], index[1], index[2]);
			break;
		  case 4:
			index[0] = (int) expr();
			match(COMMA);
			index[1] = (int) expr();
			match(COMMA);
            index[2] = (int) expr();
			match(COMMA);
			index[3] = (int) expr();
			if(errorflag == 0)
			  valptr = getdimvar(dimvar, index[0], index[1], index[2], index[3]);
			break;
		  case 5:
			index[0] = (int) expr();
			match(COMMA);
			index[1] = (int) expr();
			match(COMMA);
            index[2] = (int) expr();
			match(COMMA);
			index[3] = (int) expr();
			match(COMMA);
			index[4] = (int) expr();
			if(errorflag == 0)
			  valptr = getdimvar(dimvar, index[0], index[1], index[2], index[3]);
			break;
		}
		match(CPAREN);
	  }
	  else
	  {
	    seterror(ERR_NOSUCHVARIABLE);
        return;
      }
	  if(valptr)
	  {
		lv->type = type;
	    if(type == FLTID)
	      lv->dval = valptr;
	    else if(type == STRID)
	      lv->sval = valptr;
		else
		  assert(0);
	  }
	default:
	  seterror(ERR_SYNTAX);
  }
}

/*
  parse a boolean expression
  consists of expressions or strings and relational operators,
  and parentheses
*/
static int boolexpr(void)
{
  int left;
  int right;
  
  left = boolfactor();

  while(1)
  {
    switch(token)
	{
	  case AND:
        match(AND);
		right = boolexpr();
		return (left && right) ? 1 : 0;
	  case OR:
	    match(OR);
		right = boolexpr();
		return (left || right) ? 1 : 0;
	  default:
		return left;
	}
  }
}

/*
  boolean factor, consists of expression relop expression
    or string relop string, or ( boolexpr() )
*/
static int boolfactor(void)
{
  int answer;
  double left;
  double right;
  int op;
  char *strleft;
  char *strright;
  int cmp;

  switch(token)
  {
    case OPAREN:
	  match(OPAREN);
	  answer = boolexpr();
	  match(CPAREN);
	  break;
	default:
	  if(isstring(token))
	  {
	    strleft = stringexpr();
		op = relop();
		strright = stringexpr();
		if(!strleft || !strright)
		{
		  if(strleft)
		    free(strleft);
		  if(strright)
		    free(strright);
		  return 0;
		}
		cmp = strcmp(strleft, strright);
		switch(op)
		{
		  case ROP_EQ:
			  answer = cmp == 0 ? 1 : 0;
			  break;
		  case ROP_NEQ:
			  answer = cmp == 0 ? 0 : 1;
			  break;
		  case ROP_LT:
			  answer = cmp < 0 ? 1 : 0;
			  break;
		  case ROP_LTE:
			  answer = cmp <= 0 ? 1 : 0;
			  break;
		  case ROP_GT:
			  answer = cmp > 0 ? 1 : 0;
			  break;
		  case ROP_GTE:
			  answer = cmp >= 0 ? 1 : 0;
			  break;
		  default:
			answer = 0;
		}
		free(strleft);
		free(strright);
	  }
	  else
	  {
	    left = expr();
		op = relop();
		right = expr();
		switch(op)
		{
		  case ROP_EQ:
			  answer = (left == right) ? 1 : 0;
			  break;
		  case ROP_NEQ:
			  answer = (left != right) ? 1 : 0;
			  break;
		  case ROP_LT:
			  answer = (left < right) ? 1 : 0;
			  break;
		  case ROP_LTE:
			  answer = (left <= right) ? 1 : 0;
			  break;
		  case ROP_GT:
			  answer = (left > right) ? 1 : 0;
			  break;
		  case ROP_GTE:
			  answer = (left >= right) ? 1 : 0;
			  break;
		  default:
			 errorflag = 1;
			 return 0;

		}
	  }
	  
  }

  return answer;
}

/*
  get a relational operator
  returns operator parsed or ERROR
*/
static int relop(void)
{
  switch(token)
  {
    case EQUALS:
	  match(EQUALS);
	  return ROP_EQ;
    case GREATER:
	  match(GREATER);
	  switch(token)
	  {
	    case EQUALS:
		  match(EQUALS);
		  return ROP_GTE;
	    case LESS:
		  match(LESS);
          return ROP_NEQ;
	    default:
		  return ROP_GT;
	  }
	case LESS:
      match(LESS);
	  if(token == EQUALS)
	  {
	    match(EQUALS);
		return ROP_LTE;
	  }
	  return ROP_LT;
	default:
	  seterror(ERR_SYNTAX);
	  return ERROR;
  }
}

/*
  parses an expression
*/
static double expr(void)
{
  double left;
  double right;

  left = term();

  while(1)
  {
    switch(token)
	{
	case PLUS:
	  match(PLUS);
	  right = term();
	  left += right;
	  break;
	case MINUS:
	  match(MINUS);
      right = term();
	  left -= right;
	  break;
	default:
	  return left;
	}
  }
}

/*
  parses a term 
*/
static double term(void)
{
  double left;
  double right;

  left = factor();
  
  while(1)
  {
    switch(token)
	{
	case MULT:
	  match(MULT);
	  right = factor();
	  left *= right;
	  break;
	case DIV:
	  match(DIV);
	  right = factor();
	  if(right != 0.0)
	    left /= right;
	  else
		seterror(ERR_DIVIDEBYZERO);
	  break;
	case MOD:
	  match(MOD);
	  right = factor();
	  left = fmod(left, right);
	  break;
	default:
	  return left;
	}
  }

}

/*
  parses a factor
*/
static double factor(void)
{
  double answer = 0;
  char *str;
  int len;

  switch(token)
  {
    case OPAREN:
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  break;
	case VALUE:
	  answer = getvalue(string, &len);
	  match(VALUE);
	  break;
	case MINUS:
	  match(MINUS);
	  answer = -factor();
	  break;
	case FLTID:
	  answer = variable();
	  break;
	case DIMFLTID:
	  answer = dimvariable();
	  break;
	case E:
	  answer = exp(1.0);
	  match(E);
	  break;
	case PI:
	  answer = acos(0.0) * 2.0;
	  match(PI);
	  break;
	case SIN:
	  match(SIN);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = sin(answer);
	  break;
	case COS:
	  match(COS);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = cos(answer);
	  break;
	case TAN:
	  match(TAN);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = tan(answer);
	  break;
	case LN:
	  match(LN);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  if(answer > 0)
	    answer = log(answer);
	  else
		seterror(ERR_NEGLOG);
	  break;
	case POW:
      match(POW);
	  match(OPAREN);
	  answer = expr();
	  match(COMMA);
      answer = pow(answer, expr());
	  match(CPAREN);
	  break;
	case SQRT:
	  match(SQRT);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  if(answer >= 0.0)
		answer = sqrt(answer);
	  else
		seterror(ERR_NEGSQRT);
	  break;
	case ABS:
	  match(ABS);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = fabs(answer);
	  break;
    case LEN:
	  match(LEN);
	  match(OPAREN);
	  str = stringexpr();
	  match(CPAREN);
	  if(str)
	  {
	    answer = strlen(str);
	    free(str);
	  }
	  else
		answer = 0;
	  break;
    case ASCII:
	  match(ASCII);
	  match(OPAREN);
	  str = stringexpr();
	  match(CPAREN);
	  if(str)
	  {
		answer = *str;
	    free(str);
	  }
	  else
		answer = 0;
	  break;
    case ASIN:
	  match(ASIN);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = asin(answer);
	  break;
    case ACOS:
	  match(ACOS);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = acos(answer);
	  break;
    case ATAN:
	  match(ATAN);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = atan(answer);
	  break;
    case INT:
	  match(INT);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = floor(answer);
	  break;
    case RND:
	  match(RND);
	  match(OPAREN);
	  answer = expr();
	  match(CPAREN);
	  answer = floor(answer);
	  if(answer > 1)
		answer = fmod(rand(), answer);
	  else
	  {
		if(answer < 0)
		  srand( (unsigned) -answer);
		answer = 0;
	  }
	  break;
    case VAL:
	  match(VAL);
	  match(OPAREN);
	  str = stringexpr();
	  match(CPAREN);
	  if(str)
	  {
	    answer = strtod(str, 0);
		free(str);
	  }
	  else
		answer = 0;
	  break;
	default:
	  if(isstring(token))
		seterror(ERR_TYPEMISMATCH);
	  else
	    seterror(ERR_SYNTAX);
	  break;
  }

  while(token == SHRIEK)
  {
    match(SHRIEK);
	answer = factorial(answer);
  }

  return answer;
}

/*
  get the value of a scalar variable from string
  matches FLTID
*/
static double variable(void)
{
  VARIABLE *var;
  char id[32];
  int len;

  getid(string, id, &len);
  match(FLTID);
  var = findvariable(id);
  if(var)
    return var->dval;
  else
  {
	seterror(ERR_NOSUCHVARIABLE);
	return 0.0;
  }
}

/*
  get value of a dimensioned variable from string.
  matches DIMFLTID
*/
static double dimvariable(void)
{
  DIMVAR *dimvar;
  char id[32];
  int len;
  int index[5];
  double *answer;

  getid(string, id, &len);
  match(DIMFLTID);
  dimvar = finddimvar(id);
  if(!dimvar)
  {
    seterror(ERR_NOSUCHVARIABLE);
	return 0.0;
  }

  if(dimvar)
  {
    switch(dimvar->ndims)
	{
	  case 1:
	    index[0] = (int) expr();
		answer = getdimvar(dimvar, index[0]);
		break;
      case 2:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1]);
		break;
	  case 3:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		match(COMMA);
		index[2] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1], index[2]);
		break;
	  case 4:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		match(COMMA);
		index[2] = (int) expr();
		match(COMMA);
		index[3] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1], index[2], index[3]);
		break;
	  case 5:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		match(COMMA);
		index[2] = (int) expr();
		match(COMMA);
		index[3] = (int) expr();
		match(COMMA);
		index[4] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1], index[2], index[3], index[4]);
		break;

	}

	match(CPAREN);
  }

  if(answer)
	return *answer;

  return 0.0;

}

/*
  find a scalar variable invariables list
  Params: id - id to get
  Returns: pointer to that entry, 0 on fail
*/
static VARIABLE *findvariable(const char *id)
{
  int i;

  for(i=0;i<nvariables;i++)
	if(!strcmp(variables[i].id, id))
	  return &variables[i];
  return 0;
}

/*
  get a dimensioned array by name
  Params: id (includes opening parenthesis)
  Returns: pointer to array entry or 0 on fail
*/
static DIMVAR *finddimvar(const char *id)
{
  int i;

  for(i=0;i<ndimvariables;i++)
	if(!strcmp(dimvariables[i].id, id))
	  return &dimvariables[i];
  return 0;
}

/*
  dimension an array.
  Params: id - the id of the array (include leading ()
          ndims - number of dimension (1-5)
		  ... - integers giving dimension size, 
*/
static DIMVAR *dimension(const char *id, int ndims, ...)
{
  DIMVAR *dv;
  va_list vargs;
  int size = 1;
  int oldsize = 1;
  int i;
  int dimensions[5];
  double *dtemp;
  char **stemp;

  assert(ndims <= 5);
  if(ndims > 5)
	return 0;

  dv = finddimvar(id);
  if(!dv)
	dv = adddimvar(id);
  if(!dv)
  {
    seterror(ERR_OUTOFMEMORY);
	return 0;
  }

  if(dv->ndims)
  {
    for(i=0;i<dv->ndims;i++)
	  oldsize *= dv->dim[i];
  }
  else
	oldsize = 0;

  va_start(vargs, ndims);
  for(i=0;i<ndims;i++)
  {
	dimensions[i] = va_arg(vargs, int);
    size *= dimensions[i];
  }
  va_end(vargs);

  switch(dv->type)
  {
    case FLTID:
      dtemp = realloc(dv->dval, size * sizeof(double));
      if(dtemp)
        dv->dval = dtemp;
	  else
	  {
		seterror(ERR_OUTOFMEMORY);
	    return 0;
	  }
	  break;
	case STRID:
	  if(dv->str)
	  {
	    for(i=size;i<oldsize;i++)
		  if(dv->str[i])
		  {
			free(dv->str[i]);
		    dv->str[i] = 0;
		  }
	  }
	  stemp = realloc(dv->str, size * sizeof(char *));
	  if(stemp)
	  {
		dv->str = stemp;
	    for(i=oldsize;i<size;i++)
		  dv->str[i] = 0;
	  }
	  else
	  {
		for(i=0;i<oldsize;i++)
		  if(dv->str[i])
		  {
            free(dv->str[i]);
		    dv->str[i] = 0;
		  }
	    seterror(ERR_OUTOFMEMORY);
		return 0;
	  }
	  break;
	default:
	  assert(0);
  }

  for(i=0;i<5;i++)
	dv->dim[i] = dimensions[i];
  dv->ndims = ndims;

  return dv;
}

/*
  get the address of a dimensioned array element.
  works for both string and real arrays.
  Params: dv - the array's entry in variable list
          ... - integers telling which array element to get
  Returns: the address of that element, 0 on fail
*/ 
static void *getdimvar(DIMVAR *dv, ...)
{
  va_list vargs;
  int index[5];
  int i;
  void *answer = 0;

  va_start(vargs, dv);
  for(i=0;i<dv->ndims;i++)
  {
	index[i] = va_arg(vargs, int);
    index[i]--;
  }
  va_end(vargs);

  for(i=0;i<dv->ndims;i++)
    if(index[i] >= dv->dim[i] || index[i] < 0)
	{
	  seterror(ERR_BADSUBSCRIPT);
	  return 0;
	}

  if(dv->type == FLTID)
  {
    switch(dv->ndims)
	{
      case 1:
	  	answer = &dv->dval[ index[0] ]; 
	    break;
      case 2:
	    answer = &dv->dval[ index[1] * dv->dim[0] 
			+ index[0] ];
		break;
      case 3:
	    answer = &dv->dval[ index[2] * (dv->dim[0] * dv->dim[1]) 
			+ index[1] * dv->dim[0] 
			+ index[0] ];
	    break;
      case 4:
		answer = &dv->dval[ index[3] * (dv->dim[0] + dv->dim[1] + dv->dim[2]) 
			+ index[2] * (dv->dim[0] * dv->dim[1]) 
			+ index[1] * dv->dim[0] 
			+ index[0] ];
      case 5:
		answer = &dv->dval[ index[4] * (dv->dim[0] + dv->dim[1] + dv->dim[2] + dv->dim[3])
          + index[3] * (dv->dim[0] + dv->dim[1] + dv->dim[2])
		  + index[2] * (dv->dim[0] + dv->dim[1])
		  + index[1] * dv->dim[0]
		  + index[0] ];
	  break;
	}
  }
  else if(dv->type == STRID)
  {
	switch(dv->ndims)
	{
      case 1:
	  	answer = &dv->str[ index[0] ]; 
	    break;
      case 2:
	    answer = &dv->str[ index[1] * dv->dim[0] 
			+ index[0] ];
		break;
      case 3:
	    answer = &dv->str[ index[2] * (dv->dim[0] * dv->dim[1]) 
			+ index[1] * dv->dim[0] 
			+ index[0] ];
	    break;
      case 4:
		answer = &dv->str[ index[3] * (dv->dim[0] + dv->dim[1] + dv->dim[2]) 
			+ index[2] * (dv->dim[0] * dv->dim[1]) 
			+ index[1] * dv->dim[0] 
			+ index[0] ];
      case 5:
		answer = &dv->str[ index[4] * (dv->dim[0] + dv->dim[1] + dv->dim[2] + dv->dim[3])
          + index[3] * (dv->dim[0] + dv->dim[1] + dv->dim[2])
		  + index[2] * (dv->dim[0] + dv->dim[1])
		  + index[1] * dv->dim[0]
		  + index[0] ];
	  break;
	}
  }

  return answer;
}

/*
  add a real varaible to our variable list
  Params: id - id of varaible to add.
  Returns: pointer to new entry in table
*/
static VARIABLE *addfloat(const char *id)
{
   VARIABLE *vars;

  vars = realloc(variables, (nvariables + 1) * sizeof(VARIABLE));
  if(vars)
  {
	variables = vars;
    strcpy(variables[nvariables].id, id);
	variables[nvariables].dval = 0;
	variables[nvariables].sval = 0;
	nvariables++;
	return &variables[nvariables-1];
  }
  else
	seterror(ERR_OUTOFMEMORY);

  return 0; 
}

/*
  add a string variable to table.
  Params: id - id of variable to get (including trailing $)
  Retruns: pointer to new entry in table, 0 on fail.       
*/
static VARIABLE *addstring(const char *id)
{
  VARIABLE *vars;

  vars = realloc(variables, (nvariables + 1) * sizeof(VARIABLE));
  if(vars)
  {
	variables = vars;
    strcpy(variables[nvariables].id, id);
	variables[nvariables].sval = 0;
	variables[nvariables].dval = 0;
	nvariables++;
	return &variables[nvariables-1];
  }
  else
	seterror(ERR_OUTOFMEMORY);

  return 0;
}

/*
  add a new array to our symbol table.
  Params: id - id of array (include leading ()
  Returns: pointer to new entry, 0 on fail.
*/
static DIMVAR *adddimvar(const char *id)
{
  DIMVAR *vars;

  vars = realloc(dimvariables, (ndimvariables + 1) * sizeof(DIMVAR));
  if(vars)
  {
    dimvariables = vars;
	strcpy(dimvariables[ndimvariables].id, id);
	dimvariables[ndimvariables].dval = 0;
	dimvariables[ndimvariables].str = 0;
	dimvariables[ndimvariables].ndims = 0;
	dimvariables[ndimvariables].type = strchr(id, '$') ? STRID : FLTID;
	ndimvariables++;
	return &dimvariables[ndimvariables-1];
  }
  else
	seterror(ERR_OUTOFMEMORY);
 
  return 0;
}

/*
  high level string parsing function.
  Returns: a malloced pointer, or 0 on error condition.
  caller must free!
*/
static char *stringexpr(void)
{
  char *left;
  char *right;
  char *temp;

  switch(token)
  {
    case DIMSTRID:
	  left = mystrdup(stringdimvar());
	  break;
    case STRID:
      left = mystrdup(stringvar());
	  break;
	case QUOTE:
	  left = stringliteral();
	  break;
	case CHRSTRING:
	  left = chrstring();
	  break;
	case STRSTRING:
	  left = strstring();
	  break;
	case LEFTSTRING:
	  left = leftstring();
	  break;
	case RIGHTSTRING:
	  left = rightstring();
	  break;
	case MIDSTRING:
	  left = midstring();
	  break;
    case STRINGSTRING:
	  left = stringstring();
	  break;
	default:
	  if(!isstring(token))
		seterror(ERR_TYPEMISMATCH);
	  else
	    seterror(ERR_SYNTAX);
	  return mystrdup("");
  }

  if(!left)
  {
    seterror(ERR_OUTOFMEMORY);
    return 0;
  }

  switch(token)
  {
    case PLUS:
	  match(PLUS);
	  right = stringexpr();
	  if(right)
	  {
	    temp = mystrconcat(left, right);
	    free(right);
		if(temp)
		{
		  free(left);
          left = temp;
		}
		else
		  seterror(ERR_OUTOFMEMORY);
	  }
	  else
		seterror(ERR_OUTOFMEMORY);
	  break;
	default:
	  return left;
  }

  return left;
}

/*
  parse the CHR$ token
*/
static char *chrstring(void)
{
  double x;
  char buff[6];
  char *answer;

  match(CHRSTRING);
  match(OPAREN);
  x = expr();
  match(CPAREN);

  buff[0] = (char) x;
  buff[1] = 0;
  answer = mystrdup(buff);

  if(!answer)
	seterror(ERR_OUTOFMEMORY);

  return answer;
}

/*
  parse the STR$ token
*/
static char *strstring(void)
{
  double x;
  char buff[64];
  char *answer;

  match(STRSTRING);
  match(OPAREN);
  x = expr();
  match(CPAREN);

  sprintf(buff, "%g", x);
  answer = mystrdup(buff);
  if(!answer)
	seterror(ERR_OUTOFMEMORY);
  return answer;
}

/*
  parse the LEFT$ token
*/
static char *leftstring(void)
{
  char *str;
  double x;
  char *answer;

  match(LEFTSTRING);
  match(OPAREN);
  str = stringexpr();
  if(!str)
	return 0;
  match(COMMA);
  x = expr();
  match(CPAREN);

  if(x > strlen(str))
	return str;
  if(x < 0)
  {
    seterror(ERR_ILLEGALOFFSET);
    return str;
  }
  str[(int) x] = 0;
  answer = mystrdup(str);
  free(str);
  if(!answer)
	seterror(ERR_OUTOFMEMORY);
  return answer;
}

/*
  parse the RIGHT$ token
*/
static char *rightstring(void)
{
  double x;
  char *str;
  char *answer;

  match(RIGHTSTRING);
  match(OPAREN);
  str = stringexpr();
  if(!str)
	return 0;
  match(COMMA);
  x = expr();
  match(CPAREN);

  if( x > strlen(str))
	return str;

  if(x < 0)
  {
    seterror(ERR_ILLEGALOFFSET);
	return str;
  }
  
  answer = mystrdup( &str[strlen(str) - (int) x] );
  free(str);
  if(!answer)
	seterror(ERR_OUTOFMEMORY);
  return answer;
}

/*
  parse the MID$ token
*/
static char *midstring(void)
{
  char *str;
  double x;
  double len;
  char *answer;
  char *temp;

  match(MIDSTRING);
  match(OPAREN);
  str = stringexpr();
  match(COMMA);
  x = expr();
  match(COMMA);
  len = expr();
  match(CPAREN);

  if(!str)
	return 0;

  if( x > strlen(str) || len < 1)
  {
	free(str);
	answer = mystrdup("");
	if(!answer)
	  seterror(ERR_OUTOFMEMORY);
    return answer;
  }
  
  if(x < 1.0)
  {
    seterror(ERR_ILLEGALOFFSET);
	return str;
  }

  temp = &str[(int) x-1];

  answer = malloc( (size_t) len + 1);
  if(!answer)
  {
    seterror(ERR_OUTOFMEMORY);
	return str;
  }
  strncpy(answer, temp, (size_t) len);
  answer[(int) len] = 0;
  free(str);

  return answer;
}

/*
  parse the string$ token
*/
static char *stringstring(void)
{
  double x;
  char *str;
  char *answer;
  int len;
  int N;
  int i;

  match(STRINGSTRING);
  match(OPAREN);
  x = expr();
  match(COMMA);
  str = stringexpr();
  match(CPAREN);

  if(!str)
	return 0;

  N = (int) x;

  if(N < 1)
  {
    free(str);
	answer = mystrdup("");
	if(!answer)
	  seterror(ERR_OUTOFMEMORY);
	return answer;
  }

  len = strlen(str);
  answer = malloc( N * len + 1 );
  if(!answer)
  {
    free(str);
	seterror(ERR_OUTOFMEMORY);
	return 0;
  }
  for(i=0; i < N; i++)
  {
    strcpy(answer + len * i, str);
  }
  free(str);

  return answer;
}

/*
  read a dimensioned string variable from input.
  Returns: pointer to string (not malloced) 
*/
static char *stringdimvar(void)
{
  char id[32];
  int len;
  DIMVAR *dimvar;
  char **answer;
  int index[5];

  getid(string, id, &len);
  match(DIMSTRID);
  dimvar = finddimvar(id);

  if(dimvar)
  {
    switch(dimvar->ndims)
	{
	  case 1:
	    index[0] = (int) expr();
		answer = getdimvar(dimvar, index[0]);
		break;
      case 2:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1]);
		break;
	  case 3:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		match(COMMA);
		index[2] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1], index[2]);
		break;
	  case 4:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		match(COMMA);
		index[2] = (int) expr();
		match(COMMA);
		index[3] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1], index[2], index[3]);
		break;
	  case 5:
		index[0] = (int) expr();
		match(COMMA);
		index[1] = (int) expr();
		match(COMMA);
		index[2] = (int) expr();
		match(COMMA);
		index[3] = (int) expr();
		match(COMMA);
		index[4] = (int) expr();
		answer = getdimvar(dimvar, index[0], index[1], index[2], index[3], index[4]);
		break;

	}

	match(CPAREN);
  }
  else
	seterror(ERR_NOSUCHVARIABLE);

  if(!errorflag)
	if(*answer)
      return *answer;
	 
  return "";
}

/*
  parse a string variable.
  Returns: pointer to string (not malloced) 
*/
static char *stringvar(void)
{
  char id[32];
  int len;
  VARIABLE *var;

  getid(string, id, &len);
  match(STRID);
  var = findvariable(id);
  if(var)
  {
    if(var->sval)
	  return var->sval;
	return "";
  }
  seterror(ERR_NOSUCHVARIABLE);
  return "";
}

/*
  parse a string literal
  Returns: malloced string literal
  Notes: newlines aren't allwed in literals, but blind
         concatenation across newlines is. 
*/
static char *stringliteral(void)
{
  int len = 1;
  char *answer = 0;
  char *temp;
  char *substr;
  char *end;

  while(token == QUOTE)
  {
    while(isspace(*string))
	  string++;

    end = mystrend(string, '"');
    if(end)
	{
      len = end - string;
      substr = malloc(len);
	  if(!substr)
	  {
	    seterror(ERR_OUTOFMEMORY);
	    return answer;
	  }
	  mystrgrablit(substr, string);
	  if(answer)
	  {
		temp = mystrconcat(answer, substr);
	    free(substr);
		free(answer);
		answer = temp;
		if(!answer)
		{
	      seterror(ERR_OUTOFMEMORY);
		  return answer;
		}
	  }
	  else
	    answer = substr;
	  string = end;
	}
	else
	{
	  seterror(ERR_SYNTAX);
	  return answer;
	}

	match(QUOTE);
  }

  return answer;
}

/*
  check that we have a token of the passed type 
  (if not set the errorflag)
  Move parser on to next token. Sets token and string.
*/
static void match(int tok)
{
  if(token != tok)
  {
	seterror(ERR_SYNTAX);
	return;
  }

  while(isspace(*string))
	string++;

  string += tokenlen(string, token);
  token = gettoken(string);
  if(token == ERROR)
	seterror(ERR_SYNTAX);
}

/*
  set the errorflag.
  Params: errorcode - the error.
  Notes: ignores error cascades
*/
static void seterror(int errorcode)
{
  if(errorflag == 0 || errorcode == 0)
	errorflag = errorcode;
}

/*
  get the next line number
  Params: str - pointer to parse string
  Returns: line no of next line, 0 if end
  Notes: goes to newline, then finds
         first line starting with a digit.
*/
static int getnextline(const char *str)
{
  while(*str)
  {
    while(*str && *str != '\n')
	  str++;
    if(*str == 0)
	  return 0;
    str++;
    if(isdigit(*str))
	  return atoi(str);
  }
  return 0;
}

/*
  get a token from the string
  Params: str - string to read token from
  Notes: ignores white space between tokens
*/
static int gettoken(const char *str)
{
  while(isspace(*str))
    str++;

  if(isdigit(*str))
    return VALUE;
 
  switch(*str)
  {
    case 0:
	  return EOS;
    case '\n':
	  return EOL;
	case '/': 
	  return DIV;
	case '*':
	  return MULT;
	case '(':
	  return OPAREN;
	case ')':
	  return CPAREN;
	case '+':
	  return PLUS;
	case '-':
	  return MINUS;
	case '!':
	  return SHRIEK;
	case 'e':
	  return E;
	case ',':
	  return COMMA;
	case '"':
	  return QUOTE;
	case '=':
	  return EQUALS;
	case '<':
	  return LESS;
	case '>':
	  return GREATER;
	default:
	  if(!strncmp(str, "sin", 3) && !isalnum(str[3]))
	    return SIN;
      if(!strncmp(str, "cos", 3) && !isalnum(str[3]))
	    return COS;
	  if(!strncmp(str, "tan", 3) && !isalnum(str[3]))
	    return TAN;
	  if(!strncmp(str, "ln", 2) && !isalnum(str[2]))
	    return LN;
	  if(!strncmp(str, "pow", 3) && !isalnum(str[3]))
	    return POW;
	  if(!strncmp(str, "PI", 2) && !isalnum(str[2]))
	    return PI;
	  if(!strncmp(str, "sqrt", 4) && !isalnum(str[4]))
		return SQRT;
	  if(!strncmp(str, "PRINT", 5) && !isalnum(str[5]))
		return PRINT;
	  if(!strncmp(str, "LET", 3) && !isalnum(str[3]))
		return LET;
	  if(!strncmp(str, "DIM", 3) && !isalnum(str[3]))
		return DIM;
	  if(!strncmp(str, "IF", 2) && !isalnum(str[2]))
		return IF;
	  if(!strncmp(str, "THEN", 4) && !isalnum(str[4]))
		return THEN;
	  if(!strncmp(str, "AND", 3) && !isalnum(str[3]))
		return AND;
	  if(!strncmp(str, "OR", 2) && !isalnum(str[2]))
		return OR;
	  if(!strncmp(str, "GOTO", 4) && !isalnum(str[4]))
		return GOTO;
	  if(!strncmp(str, "INPUT", 5) && !isalnum(str[5]))
		return INPUT;
	  if(!strncmp(str, "REM", 3) && !isalnum(str[3]))
		return REM;
	  if(!strncmp(str, "FOR", 3) && !isalnum(str[3]))
		return FOR;
	  if(!strncmp(str, "TO", 2) && !isalnum(str[2]))
		return TO;
	  if(!strncmp(str, "NEXT", 4) && !isalnum(str[4]))
		return NEXT;
	  if(!strncmp(str, "STEP", 4) && !isalnum(str[4]))
		return STEP;

	  if(!strncmp(str, "MOD", 3) && !isalnum(str[3]))
		return MOD;
	  if(!strncmp(str, "ABS", 3) && !isalnum(str[3]))
		return ABS;
      if(!strncmp(str, "LEN", 3) && !isalnum(str[3]))
		return LEN;
      if(!strncmp(str, "ASCII", 5) && !isalnum(str[5]))
		return ASCII;
      if(!strncmp(str, "ASIN", 4) && !isalnum(str[4]))
		return ASIN;
      if(!strncmp(str, "ACOS", 4) && !isalnum(str[4]))
		return ACOS;
	  if(!strncmp(str, "ATAN", 4) && !isalnum(str[4]))
		return ATAN;
	  if(!strncmp(str, "INT", 3) && !isalnum(str[3]))
		 return INT;
      if(!strncmp(str, "RND", 3) && !isalnum(str[3]))
		 return RND;
      if(!strncmp(str, "VAL", 3) && !isalnum(str[3]))
		 return VAL;

	  if(!strncmp(str, "CHR$", 4))
		return CHRSTRING;
      if(!strncmp(str, "STR$", 4))
		return STRSTRING;
	  if(!strncmp(str, "LEFT$", 5))
		return LEFTSTRING; 
      if(!strncmp(str, "RIGHT$", 6))
		return RIGHTSTRING;
      if(!strncmp(str, "MID$", 4))
		return MIDSTRING;
      if(!strncmp(str, "STRING$", 7))
		return STRINGSTRING;

	  if(isalpha(*str))
	  {
		while(isalnum(*str))
		  str++;
		switch(*str)
		{
		  case '$':
			return str[1] == '(' ? DIMSTRID : STRID;
		  case '(':
			return DIMFLTID;
		  default:
			return FLTID;
		}
	  }

	  return ERROR;
  } 
}

/*
  get the length of a token.
  Params: str - pointer to the string containing the token
          token - the type of the token read
  Returns: length of the token, or 0 for EOL to prevent
           it being read past.
*/
static int tokenlen(const char *str, int token)
{
  int len = 0;
  char buff[32];

  switch(token)
  {
    case EOL:
	  return 1;
    case VALUE:
	  getvalue(str, &len);
	  return len;
	case DIMSTRID:
	case DIMFLTID:
	case STRID:
	  getid(str, buff, &len);
	  return len;
	case FLTID:
	  getid(str, buff, &len);
	  return len;
    case PI:
	  return 2;
    case E:
	  return 1;
    case UNARYMINUS:
	  return 1;
    case SIN:
	  return 3;
    case COS:
	  return 3;
    case TAN:
	  return 3;
    case LN:
	  return 2;
    case POW:
	  return 3;
	case SQRT:
	  return 4;
    case DIV:
	  return 1;
    case MULT:
	  return 1; 
	case OPAREN:
	  return 1;
    case CPAREN:
	  return 1;
    case PLUS:
	  return 1;
    case MINUS:
	  return 1;
    case SHRIEK:
	  return 1;
    case COMMA:
	  return 1;
	case QUOTE:
	  return 1;
	case EQUALS:
	  return 1;
	case LESS:
	  return 1;
	case GREATER:
	  return 1;
    case SPACE:
	  return 1;
    case ERROR:
	  return 0;
	case PRINT:
	  return 5;
    case LET:
	  return 3;
	case DIM:
	  return 3;
	case IF:
	  return 2;
	case THEN:
	  return 4;
	case AND:
	  return 3;
	case OR:
	  return 2;
	case GOTO:
	  return 4;
	case INPUT:
	  return 5;
	case REM:
	  return 3;
	case FOR:
	  return 3;
	case TO:
	  return 2;
	case NEXT:
	  return 4;
	case STEP:
	  return 4;
    case MOD:
	  return 3;
    case ABS:
	  return 3;
    case LEN:
	  return 3;
    case ASCII:
	  return 5;
    case ASIN:
	  return 4;
    case ACOS:
	  return 4;
    case ATAN:
	  return 4;
    case INT:
	  return 3;
    case RND:
      return 3;
    case VAL:
	  return 3;
    case CHRSTRING:
	  return 4;
	case STRSTRING:
	  return 4;
	case LEFTSTRING:
	  return 5;
	case RIGHTSTRING:
	  return 6;
	case MIDSTRING:
	  return 4;
	case STRINGSTRING:
	  return 7;
	default:
	  assert(0);
	  return 0;
  }
}

/*
  test if a token represents a string expression
  Params: token - token to test
  Returns: 1 if a string, else 0
*/
static int isstring(int token)
{
  if(token == STRID || token == QUOTE || token == DIMSTRID 
	  || token == CHRSTRING || token == STRSTRING 
	  || token == LEFTSTRING || token == RIGHTSTRING 
	  || token == MIDSTRING || token == STRINGSTRING)
	return 1;
  return 0;
}

/*
  get a numerical value from the parse string
  Params: str - the string to search
          len - return pinter for no chars read
  Retuns: the value of the string.
*/
static double getvalue(const char *str, int *len)
{
  double answer;
  char *end;

  answer = strtod(str, &end);
  assert(end != str);
  *len = end - str;
  return answer;
}

/*
  getid - get an id from the parse string:
  Params: str - string to search
          out - id output [32 chars max ]
		  len - return pointer for id length
  Notes: triggers an error if id > 31 chars
         the id includes the $ and ( qualifiers.
*/
static void getid(const char *str, char *out, int *len)
{
  int nread = 0;
  while(isspace(*str))
	str++;
  assert(isalpha(*str));
  while(isalnum(*str))
  {
	if(nread < 31)
	  out[nread++] = *str++;
	else
	{
      seterror(ERR_IDTOOLONG);
	  break;
	}
  }
  if(*str == '$')
  {
	if(nread < 31)
	  out[nread++] = *str++;
	else
	 seterror(ERR_IDTOOLONG);
  }
  if(*str == '(')
  {
	if(nread < 31)
	  out[nread++] = *str++;
	else
	  seterror(ERR_IDTOOLONG);
  }
  out[nread] = 0;
  *len = nread;
}


/*
  grab a literal from the parse string.
  Params: dest - destination string
          src - source string
  Notes: strings are in quotes, double quotes the escape
*/
static void mystrgrablit(char *dest, const char *src)
{
  assert(*src == '"');
  src++;
  
  while(*src)
  {
	if(*src == '"')
	{
	  if(src[1] == '"')
	  {
		*dest++ = *src;
	    src++;
	    src++;
	  }
	  else
		break;
	}
	else
     *dest++ = *src++;
  }

  *dest++ = 0;
}

/*
  find where a source string literal ends
  Params: src - string to check (must point to quote)
          quote - character to use for quotation
  Returns: pointer to quote which ends string
  Notes: quotes escape quotes
*/
static char *mystrend(const char *str, char quote)
{
  assert(*str == quote);
  str++;

  while(*str)
  {
    while(*str != quote)
	{
	  if(*str == '\n' || *str == 0)
		return 0;
	  str++;
	}
    if(str[1] == quote)
	  str += 2;
	else
	  break;
  }

  return (char *) (*str? str : 0);
}

/*
  Count the instances of ch in str
  Params: str - string to check
          ch - character to count
  Returns: no time chs occurs in str. 
*/
static int mystrcount(const char *str, char ch)
{
  int answer = 0;

  while(*str)
  {
    if(*str++ == ch)
	  answer++;
  }

  return answer;
}

/*
  duplicate a string:
  Params: str - string to duplicate
  Returns: malloced duplicate.
*/
static char *mystrdup(const char *str)
{
  char *answer;

  answer = malloc(strlen(str) + 1);
  if(answer)
    strcpy(answer, str);

  return answer;
}

/*
  concatenate two strings
  Params: str - firsts string
          cat - second string
  Returns: malloced string.
*/
static char *mystrconcat(const char *str, const char *cat)
{
  int len;
  char *answer;

  len = strlen(str) + strlen(cat);
  answer = malloc(len + 1);
  if(answer)
  {
    strcpy(answer, str);
    strcat(answer, cat);
  }
  return answer;
}

/*
  compute x!  
*/
static double factorial(double x)
{
  double answer = 1.0;
  double t;

  for(t=1;t<=x;t+=1.0)
	answer *= t;
  return answer;
}

