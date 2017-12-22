%{
/****************************************************************************
myparser.y
ParserWizard generated YACC file.

Date: 2017年11月30日
****************************************************************************/

#include "mylexer.h"
#include <fstream>
#include <map>
#include <string>
#include <cstring>
#include <vector>
using namespace std;

typedef enum { StmtK, ExpK} NodeKind;
typedef enum { LabeledK, IfK,SwitchK, DowhileK, WhileK, CompK, JumpK, ForK, InputK, PrintK, DeclarationK, FunctionDefinitionK, ExpressionListK, InitDeclaratorK, AssignmentListK,ConstantExpressionK,DeclarationSpecifiersK,InitDeclaratorListK,SuSpecifierK,StructDeclarationListK,StructDeclarationK,SqListK,StructDeclaratorListK,StructDeclaratorK,EnumSpecifierK,EnumListK,DeclaratorK,DirectDeclaratorK,TypeQualifierListK,ParamTypeListK,ParamListK,ParamDeclarationK,TypeNameK,DirectAbstractDeclaratorK,AbstractDeclaratorK,InitializerK,InitializerListK,DeclarationListK,StatementListK,TranslationUnitK,IdentifierListK,EmptyK,ExternalDeclarationK} StmtKind;
typedef enum { OpK, IdK, TypeSpecifierK , PointK, ConstantK, StringK, PostfixK, CastK, AssignmentK, StorageSpecifierK, StructUnionSpecifierK, EnumK, TypeQualifierK, LabelSpecifierK, JumpSpecifierK,MainK} ExpKind;
typedef enum { Enum,Int, Char, Bool,Void,Typedef,Extern,Auto,Static,Register,Short,Long,Signed,Unsigned,Struct,Union,Const,Volatile,Continue,Break,Return,Case,Default,Float} ExpType;

#define MAXCHILDREN 4
int NodeNum;
class Table
{
	public:
		map<string,string> Identifiers;
		Table* upTable;
	public:
		Table(){upTable = NULL;}
};
Table* presentTable;
Table* fatherTable;
Table* global;

typedef struct treeNode
{
	struct treeNode* child[MAXCHILDREN];
	int lineno;
	NodeKind nodekind;
	union {StmtKind stmt; ExpKind exp;} kind;
	union {int op; int val; char* name; string str;} attr;
	char* type;
	Table* table;
	int nodenum;
	
}TreeNode;
/*
class ID
{
	public:
		char* id;
		char* type;
	public:
		bool operator <(const ID& temp)const
		{
			return (this->id < temp.id);
		}
		bool operator =(const ID& temp)const
		{
			return !strcmp(this->id,temp.id) & !strcmp(this->type,temp.type);
		}
};
*/
TreeNode * NewStmtNode(StmtKind kind, TreeNode* child1 = NULL, TreeNode* child2 = NULL, TreeNode*child3 = NULL, TreeNode*child4 = NULL);
TreeNode * NewExpNode(ExpKind kind, TreeNode* child1 = NULL, TreeNode* child2 = NULL, TreeNode*child3 = NULL, TreeNode*child4 = NULL);
void SetType(TreeNode* node, char* type);
void PrintNode(TreeNode* node); 
void PrintOP(int);
void add(char*& a,char* b)
{
	int length = strlen(a)+strlen(b);
	char* c = new char[length];
	strcpy(c,a);
	strcat(c,b);
	a = new char[length];
	strcpy(a,c);
};

void getType(TreeNode* root,string& type)
{
	for (int i=0;i<MAXCHILDREN;++i){
		if (root->child[i]){
			getType((root->child[i]),type);
		} else {
			if ((root->kind).exp!=IdK && root->type != NULL){
				type += root->type;
				type += ",";
				return;
			} else if ((root->kind).exp == IdK){
				type += "@";
			} else {
				return ;
			}
		}
	}
};
void getIDs(TreeNode* root, vector<string>& tempTable)
{
	for (int i=0;i<MAXCHILDREN;++i){
		if (root->child[i]){
			getIDs(root->child[i],tempTable);
		} else {
			if ((root->kind).exp==IdK){
				if (tempTable.size()!=0){
					for (int i=0;i<tempTable.size();i++){
						if (tempTable[i]==(root->attr.name)){
							//cout<<"DUPLICATE IDENTIFIER!";
							return ;
						}
					}
					tempTable.push_back(root->attr.name);
					return ;
				} else {
					tempTable.push_back(root->attr.name);
					return ;
				}
			} else {
				return ;
			}
		}
	}
};
void findDeclaration(TreeNode* root, vector<TreeNode*>& declarations)
{
	
	for (int i=0;i<MAXCHILDREN;++i){
		if (root->child[i] && ((root->child[i])->kind.exp == DeclarationK || (root->child[i])->kind.exp == ParamDeclarationK)){
			declarations.push_back(root->child[i]);
		} else if (root->child[i] && (root->child[i])->kind.exp != DeclarationK && (root->child[i])->kind.exp != ParamDeclarationK){
			findDeclaration(root->child[i],declarations);
		} else {
			return;
		}
	}
};
void findParamsDeclaration(TreeNode* root, vector<TreeNode*>& declarations)
{
	for (int i=0;i<MAXCHILDREN;++i){
		if (root->child[i] && (root->child[i])->kind.stmt == ParamDeclarationK){
			declarations.push_back(root->child[i]);
		} else if(root->child[i] && (root->child[i])->kind.stmt != ParamDeclarationK){
			findParamsDeclaration(root->child[i],declarations);
		} else {
			return ;
		}
	}
};
void findFunction(TreeNode* root, vector<TreeNode*>& functions)
{
	if (root && root->kind.stmt == FunctionDefinitionK){
		functions.push_back(root);
	}
	for (int i=0;i<MAXCHILDREN;++i){
	/*
		if (root->child[i] && (root->child[i])->kind.stmt == FunctionDefinitionK){
			functions.push_back(root->child[i]);
		} else if (root->child[i] && (root->child[i]->kind.stmt != FunctionDefinitionK)){
			findFunction(root->child[i],functions);
		} else {
			return ;
		}
	*/
		if (root->child[i]){
			findFunction(root->child[i],functions);
		} else {
			return ;
		}
	}
};
void moveDown()
{
	fatherTable = presentTable;
	presentTable = new Table();
	presentTable->upTable = fatherTable;
	
};
void setTable(TreeNode* root)
{
	for (int i=0;i<MAXCHILDREN;++i){
		if (root->child[i]){
			setTable(root->child[i]);
		} else{
			if (root->kind.exp == IdK){
				root->table = presentTable;
				return;
			} else {
				return;
			}
		} 
	}
};
void addIDs(Table* present, string type, vector<string>& tempTable)
{
	for (int i=0;i<tempTable.size();++i){
	/*
		char* ttype = new char[strlen(type)];
		char* telement = new char[strlen(tempTable[i])];
		strcpy(ttype,type);
		strcpy(telement,tempTable[i]);*/
		(present->Identifiers).insert(make_pair(tempTable[i],type));
	}
};
void addFunctionsID(Table* present, string type, vector<string>& tempTable)
{
	(present->Identifiers).insert(make_pair(tempTable[0],type));
};
string cutString(string& str)
{
	string result = "";
	for (int i=0;i<str.size();++i){
		if (str[i] != '@'){
			result += str[i];
		} else {
			break;
		}
	}
	return result;
}
void createTable(TreeNode* root)
{//此函数在两种情况下会被调用，一种是在复合语句，一种是函数声明
	
	vector<TreeNode*> functions;
	findFunction(root,functions);
	if (functions.size() == 0){
		//在检测到复合语句时调用此函数
		vector<TreeNode*> declarations;
		findDeclaration(root,declarations);
		for (int i=0;i<declarations.size();++i){
			string type = "";
			string tempType = "";
			vector<string> tempTable;
			getType(declarations[i],tempType);
			type = cutString(tempType);
			getIDs(declarations[i],tempTable);
			for (int j=0;j<tempTable.size();++j){
				addIDs(presentTable,type,tempTable);
			}
		}
		setTable(root);
	} else {
		for (int k=0;k<functions.size();++k){
			vector<string> functionName;//获得function declaration子节点中所有的identifier，保留第一个就是function的名字
			vector<TreeNode*> paramsDeclaration;//获取所有的函数参数定义节点
			string tempType = "";
			string returnType = "";//保存函数的返回值类型
			string paramsType = "";//保存函数的参数类型
			string functionType = "";//函数的类型由返回值和参数类型共同决定
			getType(functions[k],tempType);//获取当前函数的返回值
			returnType = cutString(tempType);
			functionType += returnType + "/";
			findParamsDeclaration(functions[k],paramsDeclaration);//获取当前函数声明的参数声明语句
			getType(paramsDeclaration[0],paramsType);//每个函数声明只可能有一个参数声明，所以直接获取第一个元素即可
			paramsType = cutString(paramsType);
			functionType = functionType + paramsType;
			getIDs(functions[k],functionName);
			addFunctionsID(presentTable,functionType,functionName);
			vector<TreeNode*> declarations;
			findDeclaration(functions[k],declarations);
			for (int i=0;i<declarations.size();++i){
				vector<string> tempTable;
				string type = "";
				getType(declarations[i],type);
				type = cutString(type);
				getIDs(declarations[i],tempTable);
				for (int j=0;j<tempTable.size();++j){
					addIDs(presentTable,type,tempTable);
				}
			}
			setTable(functions[k]);
		}
	}
};

void moveUp()
{
	presentTable = fatherTable;
	if ((fatherTable->upTable) != NULL)
	{
		fatherTable = fatherTable->upTable;
	}
};


bool traverseIDs(TreeNode* id)
{
	/*if (id->attr.name!=NULL && !strcmp(id->attr.name,"main") && id->nodenum==1){
		return true;
	}*/
	if (id->table == NULL){
		cout<<"INIT WRONG!"<<endl;
	} else {
		Table* table = id->table;
		while (table != NULL){
			map<string,string> identifiers = table->Identifiers;
			auto it = identifiers.find((id->attr).name);
			if (it != identifiers.end()){
				return true;
			} else {
				table = table->upTable;
			}
		}
		return false;
	}
};




TreeNode* parsetree;
extern int lineno;
extern int value;
extern vector<char*> idVector;
extern int index;
extern string str;
%}

/////////////////////////////////////////////////////////////////////////////
// declarations section

// parser name
%name myparser

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

// attribute type
%include {
#ifndef YYSTYPE
#define YYSTYPE TreeNode*
#endif
}

// place any declarations here
%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF BOOL
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN 
%token PRINT

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR CONTINUE BREAK RETURN FLOAT

%nonassoc IFX
%nonassoc ELSE
%start program


%%

/////////////////////////////////////////////////////////////////////////////
// rules section

// place your YACC rules here (there must be at least one)
primary_expression
	: IDENTIFIER		{$$ = NewExpNode(IdK);$$->attr.name = idVector[index];}
	| CONSTANT			{$$ = NewExpNode(ConstantK);$$->attr.val = value;}
	| STRING_LITERAL	{$$ = NewExpNode(StringK);$$->attr.str = str;}
	| '(' expression ')'{$$ = $2;}
	;

postfix_expression
	: primary_expression		{$$ = $1;}
	| postfix_expression '[' expression ']' {$$ = NewExpNode(PostfixK,$1,$3);$$->attr.op = '[';}
	| postfix_expression '(' ')'	{$$ = NewExpNode(PostfixK,$1);$$->attr.op = '(';}
	| postfix_expression '(' argument_expression_list ')'	{$$ = NewExpNode(PostfixK,$1,$3);$$->attr.op = '(';}	
	| postfix_expression '.' IDENTIFIER		{$3 = NewExpNode(IdK);$3->attr.name = idVector[index];$$ = NewExpNode(PostfixK,$1,$3);$$->attr.op='.';}
	| postfix_expression PTR_OP IDENTIFIER	{$3 = NewExpNode(IdK);$3->attr.name = idVector[index];$$ = NewExpNode(PostfixK,$1,$3);$$->attr.op=PTR_OP;}
	| postfix_expression INC_OP		{$$ = NewExpNode(PostfixK,$1);$$->attr.op = INC_OP;}
	| postfix_expression DEC_OP		{$$ = NewExpNode(PostfixK,$1);$$->attr.op = DEC_OP;}
	;

argument_expression_list
	: assignment_expression		{$$ = $1;}
	| argument_expression_list ',' assignment_expression	{NewStmtNode(AssignmentListK,$1,$3);}
	;

unary_expression
	: postfix_expression		{$$ = NewExpNode(OpK,$1);$$->attr.op = ' ';}
	| INC_OP unary_expression	{$$ = NewExpNode(OpK,$2);$$->attr.op = INC_OP;}
	| DEC_OP unary_expression	{$$ = NewExpNode(OpK,$2);$$->attr.op = DEC_OP;}
	| '&' cast_expression		{$$ = NewExpNode(OpK,$2);$$->attr.op = '&';}
	| '*' cast_expression		{$$ = NewExpNode(OpK,$2);$$->attr.op = '*';}
	| '+' cast_expression		{$$ = NewExpNode(OpK,$2);$$->attr.op = '+';}
	| '-' cast_expression		{$$ = NewExpNode(OpK,$2);$$->attr.op = '-';}
	| '~' cast_expression		{$$ = NewExpNode(OpK,$2);$$->attr.op = '~';}
	| '!' cast_expression		{$$ = NewExpNode(OpK,$2);$$->attr.op = '!';}
	| SIZEOF unary_expression	{$$ = NewExpNode(OpK,$2);$$->attr.op = SIZEOF;}
	| SIZEOF '(' type_name ')'	{$$ = NewExpNode(OpK,$3);$$->attr.op = SIZEOF;}
	;
cast_expression
	: unary_expression		{$$ = $1;}
	| '(' type_name ')' cast_expression		{$$ = NewExpNode(CastK,$2,$4);}
	;

multiplicative_expression
	: cast_expression		{$$ = $1;}
	| multiplicative_expression '*' cast_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '*';}
	| multiplicative_expression '/' cast_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '/';}
	| multiplicative_expression '%' cast_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '%';}
	;

additive_expression
	: multiplicative_expression		{$$ =$1;}
	| additive_expression '+' multiplicative_expression	{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '+';}
	| additive_expression '-' multiplicative_expression	{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '-';}
	;

shift_expression
	: additive_expression		{$$ = $1;}
	| shift_expression LEFT_OP additive_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = LEFT_OP;}
	| shift_expression RIGHT_OP additive_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = RIGHT_OP;}
	;

relational_expression
	: shift_expression		{$$ = $1;}
	| relational_expression '<' shift_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '<';}
	| relational_expression '>' shift_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '>';}
	| relational_expression LE_OP shift_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = LE_OP;}
	| relational_expression GE_OP shift_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = GE_OP;}
	;

equality_expression
	: relational_expression		{$$ = $1;}
	| equality_expression EQ_OP relational_expression	{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = EQ_OP;}
	| equality_expression NE_OP relational_expression	{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = NE_OP;}
	;

and_expression
	: equality_expression		{$$ = $1;}
	| and_expression '&' equality_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '&';}
	;

exclusive_or_expression
	: and_expression			{$$ = $1;}
	| exclusive_or_expression '^' and_expression	{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '^';}
	;

inclusive_or_expression
	: exclusive_or_expression	{$$ = $1;}
	| inclusive_or_expression '|' exclusive_or_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = '|';}
	;

logical_and_expression
	: inclusive_or_expression	{$$ = $1;}
	| logical_and_expression AND_OP inclusive_or_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = AND_OP;}
	;

logical_or_expression
	: logical_and_expression	{$$ = $1;}
	| logical_or_expression OR_OP logical_and_expression		{$$ = NewExpNode(OpK,$1,$3);$$->attr.op = OR_OP;}
	;

conditional_expression
	: logical_or_expression		{$$ = $1;}
	| logical_or_expression '?' expression ':' conditional_expression	{$$ = NewExpNode(OpK,$1,$3,$5);$$->attr.op = '?';}
	;

assignment_expression
	: conditional_expression	{$$ = $1;}
	| unary_expression '=' assignment_expression	{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = '=';}
	| unary_expression MUL_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = MUL_ASSIGN;}
	| unary_expression DIV_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = DIV_ASSIGN;}
	| unary_expression MOD_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = MOD_ASSIGN;}
	| unary_expression ADD_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = ADD_ASSIGN;}
	| unary_expression SUB_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = SUB_ASSIGN;}
	| unary_expression LEFT_ASSIGN assignment_expression	{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = LEFT_ASSIGN;}
	| unary_expression RIGHT_ASSIGN assignment_expression	{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = RIGHT_ASSIGN;}
	| unary_expression AND_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = AND_ASSIGN;}
	| unary_expression XOR_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = XOR_ASSIGN;}
	| unary_expression OR_ASSIGN assignment_expression		{$$ = NewExpNode(AssignmentK,$1,$3);$$->attr.op = OR_ASSIGN;}
	;
expression
	: assignment_expression		{$$ = $1;}
	| expression ',' assignment_expression		{$$ = NewStmtNode(ExpressionListK,$1,$3);}
	;

constant_expression
	: conditional_expression	{$$ = $1;}
	;

declaration
	: declaration_specifiers ';'		{$$ = $1;}
	| declaration_specifiers init_declarator_list ';'	{$$ = NewStmtNode(DeclarationK,$1,$2);}
	;

declaration_specifiers
	: storage_class_specifier		{$$ = $1;}
	| storage_class_specifier declaration_specifiers	{$$ = NewStmtNode(DeclarationSpecifiersK,$1,$2);}
	| type_specifier		{$$ = $1;}
	| type_specifier declaration_specifiers		{$$ = NewStmtNode(DeclarationSpecifiersK,$1,$2);}
	| type_qualifier		{$$ = $1;}
	| type_qualifier declaration_specifiers		{$$ = NewStmtNode(DeclarationSpecifiersK,$1,$2);}
	;

init_declarator_list
	: init_declarator		{$$ = $1;}
	| init_declarator_list ',' init_declarator		{$$ = NewStmtNode(InitDeclaratorListK,$1,$3);}
	;

init_declarator
	: declarator		{$$ = $1;}
	| declarator '=' initializer		{$$ = NewStmtNode(InitDeclaratorK,$1,$3);}
	;

storage_class_specifier
	: TYPEDEF		{$$ = NewExpNode(StorageSpecifierK);SetType($$,"Typedef");}
	| EXTERN		{$$ = NewExpNode(StorageSpecifierK);SetType($$,"Extern");}
	| STATIC		{$$ = NewExpNode(StorageSpecifierK);SetType($$,"Static");}
	| AUTO			{$$ = NewExpNode(StorageSpecifierK);SetType($$,"Auto");}
	| REGISTER		{$$ = NewExpNode(StorageSpecifierK);SetType($$,"Register");}
	;

type_specifier
	: VOID			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Void");}
	| BOOL			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Bool");}
	| CHAR			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Char");}
	| SHORT			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Short");}
	| INT			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Int");}
	| FLOAT			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Float");}
	| LONG			{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Long");}
	| SIGNED		{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Signed");}
	| UNSIGNED		{$$ = NewExpNode(TypeSpecifierK);SetType($$,"Unsigned");}
	| struct_or_union_specifier		{$$ = $1;}
	| enum_specifier			{$$ = $1;}
	;

struct_or_union_specifier
	: struct_or_union IDENTIFIER '{' struct_declaration_list '}'	{$2 = NewExpNode(IdK);$2->attr.name = idVector[index];$$ = NewStmtNode(SuSpecifierK,$1,$2,$4);}
	| struct_or_union '{' struct_declaration_list '}'	{$$ = NewStmtNode(SuSpecifierK,$1,$3);}
	| struct_or_union IDENTIFIER		{$2 = NewExpNode(IdK);$2->attr.name = idVector[index];$$ = NewStmtNode(SuSpecifierK,$1,$2);}
	;

struct_or_union
	: STRUCT		{$$ = NewExpNode(StructUnionSpecifierK);SetType($$,"Struct");}
	| UNION			{$$ = NewExpNode(StructUnionSpecifierK);SetType($$,"Union");}
	;

struct_declaration_list
	: struct_declaration		{$$ = $1;}
	| struct_declaration_list struct_declaration		{$$ = NewStmtNode(StructDeclarationListK,$1,$2);}
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'		{$$ = NewStmtNode(StructDeclarationK,$1,$2);}
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list		{$$ = NewStmtNode(SqListK,$1,$2);}
	| type_specifier	{$$ = $1;}
	| type_qualifier specifier_qualifier_list		{$$ = NewStmtNode(SqListK,$1,$2);}
	| type_qualifier	{$$ = $1;}
	;

struct_declarator_list
	: struct_declarator		{$$ = $1;}
	| struct_declarator_list ',' struct_declarator		{$$ = NewStmtNode(StructDeclaratorListK,$1,$3);}
	;

struct_declarator
	: declarator	{$$ = $1;}
	| ':' constant_expression	{$$ = NewStmtNode(StructDeclaratorK,$2);}
	| declarator ':' constant_expression	{$$ = NewStmtNode(StructDeclaratorK,$1,$3);}
	;

enum_specifier
	: ENUM '{' enumerator_list '}'	{$1 = NewExpNode(EnumK);SetType($1,"Enum");$$ = NewStmtNode(EnumSpecifierK,$1,$3);}
	| ENUM IDENTIFIER '{' enumerator_list '}'	{$1 = NewExpNode(EnumK);SetType($1,"Enum");$2 = NewExpNode(IdK);$2->attr.name = idVector[index];$$ = NewStmtNode(EnumSpecifierK,$1,$2,$4);}
	| ENUM IDENTIFIER		{$1 = NewExpNode(EnumK);SetType($1,"Enum");$2 = NewExpNode(IdK);$2->attr.name = idVector[index];$$ = NewStmtNode(EnumSpecifierK,$1,$2);}
	;

enumerator_list
	: enumerator	{$$ = $1;}
	| enumerator_list ',' enumerator		{$$ = NewStmtNode(EnumListK,$1,$3);}
	;

enumerator
	: IDENTIFIER	{$$ = NewExpNode(IdK);$$->attr.name = idVector[index];}
	| IDENTIFIER '=' constant_expression	{$1 = NewExpNode(IdK);$1->attr.name = idVector[index];$$ = NewExpNode(AssignmentK,$1,$3);}
	;

type_qualifier
	: CONST		{$$ = NewExpNode(TypeQualifierK);SetType($$,"Const");}
	| VOLATILE	{$$ = NewExpNode(TypeQualifierK);SetType($$,"Volatile");}
	;

declarator
	: pointer direct_declarator		{$$ = NewStmtNode(DeclaratorK,$1,$2);}
	| direct_declarator		{$$ = $1;}
	;

direct_declarator
	: IDENTIFIER	{$1 = NewExpNode(IdK);$1->attr.name = idVector[index];$$ = NewStmtNode(DirectDeclaratorK,$1);}
	| '(' declarator ')'	{$$ = $2;}
	| direct_declarator '[' constant_expression ']'	{$$ = NewStmtNode(DirectDeclaratorK,$1,$3);}
	| direct_declarator '[' ']'	{$$ = $1;}
	| direct_declarator '(' parameter_type_list ')'	{$$ = NewStmtNode(DirectDeclaratorK,$1,$3);}
	| direct_declarator '(' identifier_list ')'	{$$ = NewStmtNode(DirectDeclaratorK,$1,$3);}
	| direct_declarator '(' ')'		{$$ = $1;}
	;

pointer
	: '*'	{$$ = NewExpNode(PointK);$$->attr.op = '*';}
	| '*' type_qualifier_list	{$$ = NewExpNode(PointK,$2);$$->attr.op = '*';}
	| '*' pointer		{$$ = NewExpNode(PointK,$2);$$->attr.op = '*';}
	| '*' type_qualifier_list pointer	{$$ = NewExpNode(PointK,$2,$3);$$->attr.op = '*';}
	;

type_qualifier_list
	: type_qualifier	{$$ = $1;}
	| type_qualifier_list type_qualifier	{$$ = NewStmtNode(TypeQualifierListK,$1,$2);}
	;


parameter_type_list
	: parameter_list	{$$ = $1;}
	| parameter_list ',' ELLIPSIS		{$$ = NewStmtNode(ParamTypeListK,$1,$3);}
	;

parameter_list
	: parameter_declaration		{$$ = $1;}
	| parameter_list ',' parameter_declaration		{$$ = NewStmtNode(ParamListK,$1,$3);}
	;

parameter_declaration
	: declaration_specifiers declarator		{$$ = NewStmtNode(ParamDeclarationK,$1,$2);}
	| declaration_specifiers abstract_declarator		{$$ = NewStmtNode(ParamDeclarationK,$1,$2);}
	| declaration_specifiers	{$$ = $1;}
	;

identifier_list
	: IDENTIFIER	{$$ = NewExpNode(IdK);$$->attr.name = idVector[index];}
	| identifier_list ',' IDENTIFIER		{$3 = NewExpNode(IdK);$3->attr.name = idVector[index];$$ = NewStmtNode(IdentifierListK,$1,$3);}
	;

type_name
	: specifier_qualifier_list		{$$ = $1;}
	| specifier_qualifier_list abstract_declarator		{$$ = NewStmtNode(TypeNameK,$1,$2);}
	;

abstract_declarator
	: pointer		{$$ = $1;}
	| direct_abstract_declarator		{$$ = NewStmtNode(AbstractDeclaratorK,$1);}
	| pointer direct_abstract_declarator		{$$ = NewStmtNode(AbstractDeclaratorK,$1,$2);}
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'	{$$ = $2;}
	| '[' ']'		{$$ = NewStmtNode(DirectAbstractDeclaratorK);}
	| '[' constant_expression ']'		{$$ = $2;}
	| direct_abstract_declarator '[' ']'	{$$ = $1;}
	| direct_abstract_declarator '[' constant_expression ']'	{$$ = NewStmtNode(DirectAbstractDeclaratorK,$1,$3);}
	| '(' ')'	{$$ = NewStmtNode(DirectAbstractDeclaratorK);}
	| '(' parameter_type_list ')'	{$$ = $2;}
	| direct_abstract_declarator '(' ')'	{$$ = $1;}
	| direct_abstract_declarator '(' parameter_type_list ')'	{$$ = NewStmtNode(DirectAbstractDeclaratorK,$1,$3);}
	;

initializer
	: assignment_expression		{$$ = $1;}	
	| '{' initializer_list '}'	{$$ = $2;}
	| '{' initializer_list ',' '}'	{$$ = $2;}
	;

initializer_list
	: initializer		{$$ = $1;}
	| initializer_list ',' initializer		{$$ = NewStmtNode(InitializerListK,$1,$3);}
	;

statement
	: labeled_statement		{$$ = $1;}
	| compound_statement	{$$ = $1;}
	| expression_statement	{$$ = $1;}
	| iteration_statement	{$$ = $1;}
	| jump_statement		{$$ = $1;}
	| selection_statement	{$$ = $1;}
	| print_statement		{$$ = $1;}
	;


print_statement
	:PRINT '(' primary_expression ')' ';'	{$$ = NewStmtNode(PrintK,$3);}
	;

labeled_statement
	: IDENTIFIER ':' statement	{$1 = NewExpNode(IdK);$1->attr.name = idVector[index];$$ = NewStmtNode(LabeledK,$1,$3);}
	| CASE constant_expression ':' statement	{$1 = NewExpNode(LabelSpecifierK);SetType($1,"Case");$$ = NewStmtNode(LabeledK,$1,$2,$4);}	
	| DEFAULT ':' statement		{$1 = NewExpNode(LabelSpecifierK);SetType($1,"Default");$$ = NewStmtNode(LabeledK,$1,$2,$4);}
	;

compound_statement
	: '{' '}'	{$$ = NewStmtNode(CompK);}
	| '{' {moveDown();} statement_list  '}' {$$ = NewStmtNode(CompK,$3);createTable($$);moveUp();}	
	| '{' declaration_list {moveDown();} '}' {$$ = NewStmtNode(CompK,$2);createTable($$);moveUp();}	
	| '{' declaration_list {moveDown();} statement_list '}' {$$ = NewStmtNode(CompK,$2,$4);createTable($$);moveUp();}	
	;

declaration_list
	: declaration		{$$ = $1;}
	| declaration_list declaration		{$$ = NewStmtNode(DeclarationListK,$1,$2);}
	;

statement_list
	: statement		{$$ = $1;}
	| statement_list statement			{$$ = NewStmtNode(StatementListK,$1,$2);}
	;

expression_statement
	: ';' {$$ = NewStmtNode(EmptyK);}
	| expression ';'	{$$ = $1;}
	;

	
selection_statement
	: IF '(' expression ')' statement %prec IFX		{$$ = NewStmtNode(IfK,$3,$5);}
	| IF '(' expression ')' statement ELSE statement	{$$ = NewStmtNode(IfK,$3,$5,$7);}
	| SWITCH '(' expression ')' statement		{$$ = NewStmtNode(SwitchK,$3,$5);}
	;


iteration_statement
	: WHILE '(' expression ')' statement	{$$ = NewStmtNode(WhileK,$3,$5);}
	| DO statement WHILE '(' expression ')' ';'		{$$ = NewStmtNode(DowhileK,$2,$5);}
	| FOR '(' expression_statement expression_statement ')' statement		{$$ = NewStmtNode(ForK,$3,$4,$6);}
	| FOR '(' expression_statement expression_statement expression ')' statement		{$$ = NewStmtNode(ForK,$3,$4,$5,$7);}
	;

jump_statement
	: CONTINUE ';'		{$1 = NewExpNode(JumpSpecifierK);SetType($1,"Continue");$$ = NewStmtNode(JumpK,$1);}
	| BREAK ';'			{$1 = NewExpNode(JumpSpecifierK);SetType($1,"Break");$$ = NewStmtNode(JumpK,$1);}
	| RETURN ';'		{$1 = NewExpNode(JumpSpecifierK);SetType($1,"Return");$$ = NewStmtNode(JumpK,$1);}
	| RETURN expression ';'		{$1 = NewExpNode(JumpSpecifierK);SetType($1,"Return");$$ = NewStmtNode(JumpK,$1,$2);}
	;


external_declaration
	: function_definition		{$$ = $1;}
	| declaration				{$$ = $1;}
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement		{$$ = NewStmtNode(FunctionDefinitionK,$1,$2,$3,$4);moveDown();createTable($$);moveUp();}
	| declaration_specifiers declarator compound_statement	{$$ = NewStmtNode(FunctionDefinitionK,$1,$2,$3);moveDown();createTable($$);moveUp();}
	| declarator declaration_list compound_statement	{$$ = NewStmtNode(FunctionDefinitionK,$1,$2,$3);moveDown();createTable($$);moveUp();}
	| declarator compound_statement		{$$ = NewStmtNode(FunctionDefinitionK,$1,$2);moveDown();createTable($$);moveUp();}
	;

translation_unit
	: external_declaration		{$$ = $1;}
	| translation_unit external_declaration		{$$ = NewStmtNode(TranslationUnitK,$1,$2);}
	;
	
program
	: translation_unit	{$$ = $1;parsetree = $$;cout<<"SUCCESS!"<<endl;}
	;

%%

/////////////////////////////////////////////////////////////////////////////
// programs section


TreeNode* NewStmtNode(StmtKind kind,TreeNode* child1,TreeNode* child2,TreeNode*child3,TreeNode*child4)
{
    TreeNode * newnode = (TreeNode *) malloc(sizeof(TreeNode));
    if (newnode==NULL)
      cout<<"Out of memory error at line %d\n"<<endl;
    else
    {
      newnode->nodekind=StmtK;
      newnode->kind.stmt=kind;
      newnode->child[0]=child1;
      newnode->child[1]=child2;
      newnode->child[2]=child3;
      newnode->child[3]=child4;
      newnode->nodenum=NodeNum;
      NodeNum++;
      newnode->type = NULL;
      newnode->lineno = lineno;
      newnode->table = NULL;
    }
    return newnode;
}


TreeNode * NewExpNode(ExpKind kind,TreeNode* child1,TreeNode* child2,TreeNode*child3,TreeNode*child4)
{
    TreeNode * newnode = (TreeNode *) malloc(sizeof(TreeNode));
    if (newnode==NULL)
        cout<<"Out of memory error at line %d\n"<<endl;
    else 
    {
      newnode->nodekind=ExpK;
      newnode->kind.exp = kind;
      newnode->child[0]=child1;
      newnode->child[1]=child2;
      newnode->child[2]=child3;
      newnode->child[3]=child4;
      newnode->lineno = lineno;
      newnode->type = NULL;
      newnode->nodenum=NodeNum;
      newnode->table = NULL;
      NodeNum++;
    }
    return newnode;
}

void PrintNode(TreeNode* node)
{
  cout.width(2);
  cout<<node->nodenum<<":";
  switch(node->nodekind)
  {
    case StmtK:
    {
        cout.width(20);
        switch(node->kind.stmt)
        {
            case LabeledK:
            {
				cout.width(20);
				cout<<"Labeled Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case DeclarationK:
            {
                cout.width(20);
                cout<<"Declaration Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case IfK:
            {
                cout.width(20);
                cout<<"If Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case CompK:
            {
                cout.width(20);
                cout<< "Compound Statement";
                cout.width(20);
                cout<<" ";
                break;
            }
            case WhileK:
            {
                cout.width(20);
                cout<<"While Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case DowhileK:
            {
                cout.width(20);
                cout<<"Do-While Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case ForK:
            {
				cout.width(20);
				cout<<"For Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case SwitchK:
            {
				cout.width(20);
				cout<<"Switch Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case PrintK:
            {
				cout.width(20);
				cout<<"Print Statement";
				cout.width(20);
				cout<<" ";
				break;
            }
            case DeclarationListK:
            {
                cout.width(20);
                cout<<"Declaration List";
               	cout.width(20);
               	cout<<" ";
                break;
            }
            case FunctionDefinitionK:
            {
				cout.width(20);
				cout<<"Function Definition";
				cout.width(20);
				cout<<" ";
				break;
            }
            case ExpressionListK:
            {
            	cout.width(20);
            	cout<<"Expression List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case InitDeclaratorK:
            {
            	cout.width(20);
            	cout<<"Init Declarator";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case AssignmentListK:
            {
            	cout.width(20);
            	cout<<"Assignment List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case DeclarationSpecifiersK:
            {
            	cout.width(20);
            	cout<<"Declaration Specifiers";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case InitDeclaratorListK:
            {
            	cout.width(20);
            	cout<<"Init Declarator List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case SuSpecifierK:
            {
            	cout.width(20);
            	cout<<"Struct or Union Specifier";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case StructDeclarationListK:
            {
            	cout.width(20);
            	cout<<"Struct Declaration List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case StructDeclarationK:
            {
            	cout.width(20);
            	cout<<"Struct Declaration";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case SqListK:
            {
            	cout.width(20);
            	cout<<"Specifier Qualifier List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case StructDeclaratorListK:
            {
            	cout.width(20);
            	cout<<"Struct Declarator List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case StructDeclaratorK:
            {
            	cout.width(20);
            	cout<<"Struct Declarator";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case EnumSpecifierK:
            {
            	cout.width(20);
            	cout<<"Enum Specifier";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case EnumListK:
            {
            	cout.width(20);
            	cout<<"Enum List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case DeclaratorK:
            {
            	cout.width(20);
            	cout<<"Declarator";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case DirectDeclaratorK:
            {
            	cout.width(20);
            	cout<<"Direct Declarator";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case TypeQualifierListK:
            {
            	cout.width(20);
            	cout<<"Type Qualifier List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case ParamTypeListK:
            {
            	cout.width(20);
            	cout<<"Parameter Type List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case ParamListK:
            {
            	cout.width(20);
            	cout<<"Parameter List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case ParamDeclarationK:
            {
            	cout.width(20);
            	cout<<"Parameter Declaration";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case TypeNameK:
            {
            	cout.width(20);
            	cout<<"Type Name";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case DirectAbstractDeclaratorK:
            {
            	cout.width(20);
            	cout<<"Direct Abstract Declarator";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case AbstractDeclaratorK:
            {
            	cout.width(20);
            	cout<<"Abstract Declarator";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case InitializerK:
            {
            	cout.width(20);
            	cout<<"Initializer";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case InitializerListK:
            {
            	cout.width(20);
            	cout<<"Initializer List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case StatementListK:
            {
            	cout.width(20);
            	cout<<"Statement List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case TranslationUnitK:
            {
            	cout.width(20);
            	cout<<"Translation Unit";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case IdentifierListK:
            {
            	cout.width(20);
            	cout<<"Identifier List";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case EmptyK:
            {
				cout.width(20);
            	cout<<"Empty Expression";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case JumpK:
            {
            	cout.width(20);
            	cout<<"Jump Statement";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case ConstantExpressionK:
            {
				cout.width(20);
            	cout<<"Constant Statement";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case ExternalDeclarationK:
            {
					cout.width(20);
            	cout<<"External Declaration";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            default :
                cout<<"WRONG statement!";
                break;

        }//end switch(node->kind.exp)
        break;
    }
    case ExpK:
    {
        char* types[] = {"enum","int", "char", "bool","void", "typedef", "extern", "auto", "static", "register", "short", "long", "signed", "unsigned", "struct", "union", "const", "volatile", "continue", "break", "return", "case", "default","float"};
        switch(node->kind.exp)
        {
            case OpK:
            {
                cout.width(20);
				cout<<"Expression";
				cout.width(10);
				cout<<"op:";
				cout.width(10);
				PrintOP(node->attr.op);
				break;
            }
            case ConstantK:
            {
				cout.width(20);
				cout<<"Constant";
				cout.width(10);
				cout<<"value:";
				cout.width(10);
				cout<<node->attr.val;
				break;
            }
            case IdK:
            {
            
				cout.width(20);
				cout<<"IDENTIFIER";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->attr.name;
				
				break;
            }
            case PostfixK:
            {
				cout.width(20);
				cout<<"Postfix Expression";
				cout.width(10);
				cout<<"op:";
				cout.width(10);
				PrintOP(node->attr.op);
				break;
            }
            case CastK:
            {
            	cout.width(20);
            	cout<<"Cast Expression";
            	cout.width(20);
            	cout<<" ";
            	break;
            }        
            case AssignmentK:
            {
            	cout.width(20);
            	cout<<"Assignment Expression";
            	cout.width(20);
            	cout<<" ";
            	break;
            }
            case PointK:
            {
                cout.width(20);
				cout<<"Pointer";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->attr.op;
				break;
            }
            case StringK:
			{
				cout.width(20);
				cout<<"String";
				cout.width(10);
				cout<<"value:";
				cout.width(10);
				cout<<(node->attr.str);
				break;
			}
			case TypeSpecifierK:
			{
				cout.width(20);
				cout<<"Type Specifier";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case StorageSpecifierK:
			{
				cout.width(20);
				cout<<"Storage Specifier";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case StructUnionSpecifierK:
			{
				cout.width(20);
				cout<<"Struct Union Specifier";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case EnumK:
			{
				cout.width(20);
				cout<<"Enum";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case TypeQualifierK:
			{
				cout.width(20);
				cout<<"Type Qualifier";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case LabelSpecifierK:
			{
				cout.width(20);
				cout<<"Label Specifier";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case JumpSpecifierK:
			{
				cout.width(20);
				cout<<"Jump Specifier";
				cout.width(10);
				cout<<"symbol:";
				cout.width(10);
				cout<<node->type;
				break;
			}
			case MainK:
			{
				cout.width(20);
				cout<<"Main";
				cout.width(10);
				cout<<"symbol:";
				cout<<"main";
				break;
			}
            default :
                cout.width(20);
                cout<<"WRONG EXPRESSION!";
                cout.width(20);
                cout<<" ";
                break;
        }//end switch(node->kind.exp)
        break;
    }
    default :
          cout<<"WRONG!";
          break;
  }//end switch(node->nodekind)

  cout<<"    ";
  cout << "Children: ";
  int i=0;
  while(node->child[i] &&i<MAXCHILDREN)
  {
      cout<<node->child[i++]->nodenum<<" ";
  }

  cout << endl;
}

void traverse(TreeNode* root)
{
	if (root == NULL){
		return;
	}
	for (int i=0;i<MAXCHILDREN;i++){
		if (root->child[i] != NULL){
			traverse(root->child[i]);
		} else{
			if (root->nodekind == ExpK && root->kind.exp == IdK){
				if (traverseIDs(root)){
					PrintNode(root);
					return;
				} else {
					cout<<"WRONG IDENTIFIER!"<<endl;
					return;
				}
			} else {
				PrintNode(root);
				return;
			}
		}
	}
	PrintNode(root);
	return ;
}

void PrintOP(int op)
{
  switch(op)
  {
     case ' ': cout<<" ";break;
     case '.': cout<<"."; break;
     case PTR_OP: cout<<"->"; break;
     case INC_OP: cout<<"++"; break;
     case DEC_OP: cout<<"--"; break;
     case '&': cout<<"&"; break;
     case '*': cout<<"*"; break;
     case '+': cout<<"+"; break;
     case '-': cout<<"-"; break;
     case '~': cout<<"~"; break;
     case '!': cout<<"!"; break;
     case SIZEOF: cout<<"SIZEOF"; break;
     case '/': cout<<"/"; break;
     case '%': cout<<"%"; break;
     case LEFT_OP: cout<<"<<"; break;
     case RIGHT_OP: cout<<">>"; break;
     case '<': cout<<"<"; break;
     case '>': cout<<">"; break;
     case LE_OP: cout<<"<="; break;
     case GE_OP: cout<<">="; break;
     case EQ_OP: cout<<"=="; break;
     case NE_OP:cout<<"!=" ; break;
     case '^':cout<<"^" ; break;
     case '|':cout<<"|"  ; break;
     case AND_OP:cout<<"&&"  ; break;
     case OR_OP:cout<<"||" ; break;
     case '?':cout<<"?"  ; break;
     case '=':cout<<"="  ; break;
     case MUL_ASSIGN: cout<<"*="; break;
     case DIV_ASSIGN: cout<<"/="; break;
     case MOD_ASSIGN: cout<<"%="; break;
     case ADD_ASSIGN: cout<<"+="; break;
     case SUB_ASSIGN: cout<<"-="; break;
     case RIGHT_ASSIGN: cout<<">>="; break;
     case LEFT_ASSIGN: cout<<"<<="; break;
     case AND_ASSIGN: cout<<"&="; break;
     case OR_ASSIGN: cout<<"|="; break;
     case XOR_ASSIGN: cout<<"^="; break;
     case '(':cout<<"(";break;
     case '[':cout<<"[";break;
     default : cout<<"other";
  }
}
void SetType(TreeNode* node,char* type)
{
    node->type=type;
    int i=0;
    while(node->child[i])
    {
        node->child[i]->type=type;
        i++;
    }
}
int main()
{
	int n = 1;
	freopen("temp.txt","r",stdin);
	mylexer lexer;
	myparser parser;
	presentTable = new Table();
	/*
	char* main = "main";
	char* Int = "int";
	Table* temp = new Table();
	*/
	//presentTable->Identifiers.insert(make_pair(main,Int));
	fatherTable = presentTable;
	//global = presentTable;
	if (parser.yycreate(&lexer)) {
		if (lexer.yycreate(&parser)) {
			n = parser.yyparse();
		}
	}
	//parsetree->table = presentTable;
	traverse(parsetree);
	return n;
}

