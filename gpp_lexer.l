%{
	#include <stdio.h>
%}
%%

"and"	{printf("KW_AND\n");}
"or"	{printf("KW_OR\n");}
"not"	{printf("KW_NOT\n");}
"equal"	{printf("KW_EQUAL\n");}
"list"	{printf("KW_LIST\n");}
"exit"	{printf("KW_EXIT\n");}
"append" {printf("KW_APPEND\n");}
"deffun"	{printf("KW_DEFFUN\n");}
"concat"	{printf("KW_CONCAT\n");}
"for"	{printf("KW_FOR\n");}
"set"	{printf("KW_SET\n");}
"less"	{printf("KW_LESS\n");}
"disp"	{printf("KW_DISP\n");}
"load"	{printf("KW_LOAD\n");}
"true"	{printf("KW_TRUE\n");}
"false"	{printf("KW_FALSE\n");}
"nil"	{printf("KW_NIL\n");}
"+"	{printf("OP_PLUS\n");}
"-"	{printf("OP_MINUS\n");}
"*"	{printf("OP_MULT\n");}
"/"	{printf("OP_DIV\n");}
"("	{printf("OP_OP\n");}
")"	{printf("OP_CP\n");}
","	{printf("OP_COMMA\n");}
"”"	{printf("OP_CC\n");}
"**" {printf("OP_DBLMLT\n");}
"“"	{printf("OP_OC\n");}

[\t\n]	{;}

[a-zA-Z][0-9a-zA-Z]* {printf("IDENTIFIER\n");}

[1-9][0-9]*	{printf("VALUE\n");}

";;"[^}\n]*	{printf("COMMENT\n");}

%%
int yywrap(void){
	return 1;
}

int main(argc, argv)
	int argc;
	char **argv;
	{
		++argv,--argc;
		if(argc > 0)
			yyin=fopen(argv[0],"r");
		else
			yyin=stdin;

		yylex();
	}
