%{
/****************************************************************************
mylexer.l
ParserWizard generated Lex file.

Date: 2017Äê11ÔÂ30ÈÕ
****************************************************************************/

#include "myparser.h"
#include <string>
#include <cstring>
#include <map>
#include <vector>
char* AllType[] = {"int","char","short","long","bool"};
int value;
float f_value;
char c_value;
char* identifier;
int index = -1;
int lineno = 1;
int IdNumber = 0;
string str;



//map<char*,ID> idMap; //map to store all the identifiers
//map<int,char*> reverseMap;//reverse map to get the identifier quickly
vector<char*> idVector;
void insertID(char* _identifier)
{
	char* temp = new char[strlen(_identifier)];
	strcpy(temp,_identifier);
	idVector.push_back(temp);
}
/*
void insertReverse(int number,char*  _identifier)
{
	reverseMap.insert(make_pair(number,_identifier));
}
*/
%}

/////////////////////////////////////////////////////////////////////////////
// declarations section
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*


// lexical analyser name
%name mylexer

// class definition
{
	// place any extra class members here
}

// constructor
{
	// place any extra initialisation code here
}

// destructor
{
	// place any extra cleanup code here
}

// place any declarations here

%%

/////////////////////////////////////////////////////////////////////////////
// rules section
/*
%{
	// extract yylval for use later on in actions
	YYSTYPE YYFAR& yylval = *(YYSTYPE YYFAR*)yyparserptr->yylvalptr;
%}
*/
// place your Lex rules here
"/*"([^\*\/]|"*"[^\/])*"*/"	 { /*do nothing for comment*/ }

"auto"			{return(AUTO);}
"bool"			{return(BOOL);}
"break"			{return(BREAK);}
"case"			{return(CASE);}
"char"			{return(CHAR);}
"const"			{return(CONST);}
"continue"		{return(CONTINUE);}
"default"		{return(DEFAULT);}
"do"			{return(DO);}
"double"		{/*return(DOUBLE);*/}
"else"			{return(ELSE);}
"enum"			{return(ENUM);}
"extern"		{return(EXTERN);}
"float"			{return(FLOAT);}
"for"			{return(FOR);}
"goto"			{/*return(GOTO);*/}
"if"			{return(IF);}
"int"			{return(INT);}
"long"			{return(LONG);}
//"main"			{return(MAIN);}
"print"			{return (PRINT);}
"register"		{return(REGISTER);}
"return"		{return(RETURN);}
"short"			{return(SHORT);}
"signed"		{return(SIGNED);}
"sizeof"		{return(SIZEOF);}
"static"		{return(STATIC);}
"struct"		{return(STRUCT);}
"switch"		{return(SWITCH);}
"typedef"		{return(TYPEDEF);}
"union"			{return(UNION);}
"unsigned"		{return(UNSIGNED);}
"void"			{return(VOID);}
"volatile"		{return(VOLATILE);}
"while"			{return(WHILE);}

{L}({L}|{D})*		{identifier = new char[strlen(yytext)];strcpy(identifier,yytext);insertID(identifier);index++;return(IDENTIFIER);}

0[xX]{H}+{IS}?		{value = atoi(yytext); return(CONSTANT);}
0{D}+{IS}?		{value = atoi(yytext); return(CONSTANT);}
{D}+{IS}?		{value = atoi(yytext); return(CONSTANT);}
'[a-zA-Z]'	{char * temp = new char[strlen(yytext)];strcpy(temp,yytext);c_value = temp[1];return(CCONSTANT);}

{D}+{E}{FS}?		{f_value = atof(yytext);return(FCONSTANT);}
{D}*"."{D}+({E})?{FS}?	{f_value = atof(yytext);return(FCONSTANT);}
{D}+"."{D}*({E})?{FS}?	{f_value = atof(yytext);return(FCONSTANT);}

L?\"(\\.|[^\\"])*\"	{str = yytext; return(STRING_LITERAL);}

"..."			{return(ELLIPSIS);}
">>="			{return(RIGHT_ASSIGN);}
"<<="			{return(LEFT_ASSIGN);}
"+="			{return(ADD_ASSIGN);}
"-="			{return(SUB_ASSIGN);}
"*="			{return(MUL_ASSIGN);}
"/="			{return(DIV_ASSIGN);}
"%="			{return(MOD_ASSIGN);}
"&="			{return(AND_ASSIGN);}
"^="			{return(XOR_ASSIGN);}
"|="			{return(OR_ASSIGN);}
">>"			{return(RIGHT_OP);}
"<<"			{return(LEFT_OP);}
"++"			{return(INC_OP);}
"--"			{return(DEC_OP);}
"->"			{return(PTR_OP);}
"&&"			{return(AND_OP);}
"||"			{return(OR_OP);}
"<="			{return(LE_OP);}
">="			{return(GE_OP);}
"=="			{return(EQ_OP);}
"!="			{return(NE_OP);}
";"			{return(';');}
("{"|"<%")		{return('{');}
("}"|"%>")		{return('}');}
","			{return(',');}
":"			{return(':');}
"="			{return('=');}
"("			{return('(');}
")"			{return(')');}
("[")		{return('[');}
("]")		{return(']');}
"."			{return('.');}
"&"			{return('&');}
"!"			{return('!');}
"~"			{return('~');}
"-"			{return('-');}
"+"			{return('+');}
"*"			{return('*');}
"/"			{return('/');}
"%"			{return('%');}
"<"			{return('<');}
">"			{return('>');}
"^"			{return('^');}
"|"			{return('|');}
"?"			{return('?');}
[\n]		{lineno++;}
[ \t]+	{/*do nothing*/}
.			{ /* ignore bad characters */ }

%%

/////////////////////////////////////////////////////////////////////////////
// programs section

