#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define KB(size) (1024 * size)
#define MB(size) (1024 * KB(size))
#define GB(size) (1024 * MB(size))
#define TB(size) (1024 * GB(size))

#define CreateTempAlloc(size) tempAllocPointer = malloc(size); nextTempPointer = tempAllocPointer; tempAllocSize = size
#define TempAlloc(size) nextTempPointer; assert(nextTempPointer - tempAllocPointer + size <= tempAllocSize); nextTempPointer += size
#define FreeTempAlloc() nextTempPointer = tempAllocPointer
#define PrintTotalTempAlloc() printf("TempAlloc current size: %d", (int)(nextTempPointer - tempAllocPointer))
#define DestroyTempAlloc() free(tempAllocPointer); tempAllocSize = 0;
static char* tempAllocPointer = NULL;
static char* nextTempPointer = NULL;
static size_t tempAllocSize = 0;

typedef enum TokenType
{
	Token_Unknown,
	Token_OpenBrace,
	Token_CloseBrace,
	Token_OpenParenthesis,
	Token_CloseParenthesis,
	Token_Semicolon,
	Token_Dash,
	Token_Tilde,
	Token_ExclamationPoint,
	Token_Plus,
	Token_ForwardSlash,
	Token_Asterisk,

	Token_KeywordInt,
	Token_KeywordReturn,

	Token_Identifier,
	Token_IntegerLiteral,

	Token_EndOfStream,
} TokenType;

typedef struct Token {
	TokenType type;
	int length;
	char* text;
} Token;

typedef struct Tokenizer {
	char* at;
} Tokenizer;


typedef enum ASTStatementType
{
	AST_Statement_Return,
	AST_Statement_Assign,
} ASTStatementType;

typedef enum ASTExpressionType
{
	AST_Expression_Integer,
	AST_Expression_UnaryOperation,
	AST_Expression_BinaryOperation,
} ASTExpressionType;

typedef enum ASTUnaryOperationType
{
	AST_UnaryOp_Negate,
	AST_UnaryOp_Not,
	AST_UnaryOp_BitWise_Complement,
} ASTUnaryOperationType;

typedef enum ASTBinaryOperationType
{
	AST_BinaryOp_Add,
	AST_BinaryOp_Minus,
	AST_BinaryOp_Multiply,
	AST_BinaryOp_Divide,
} ASTBinaryOperationType;

typedef struct ASTExpression
{
	ASTExpressionType type;
	union {
		int integer;
		struct { ASTUnaryOperationType op; struct ASTExpression* operand; } unaryOp;
		struct { ASTBinaryOperationType op; struct ASTExpression* left; struct ASTExpression* right; } binaryOp;
	} op; // TODO: Fill out with the data that each 'type' corresponds to
} ASTExpression;

typedef struct ASTStatment
{
	ASTStatementType type;
	ASTExpression data; // TODO: Make list of expressions
} ASTStatment;

typedef struct ASTFunction
{
	char name[64];
	int nameLength;
	ASTStatment statment; // TODO: Make list of statments
} ASTFunction;

typedef struct ASTProgram
{
	ASTFunction function; // TODO: Make list of functions
} ASTProgram;

static ASTStatment ParseStatment(Token token, Tokenizer* tokenizer);
static ASTExpression ParseExpression(Tokenizer* tokenizer);
static ASTFunction ParseFunction(Token token, Tokenizer* tokenizer);
static ASTExpression ParseTerm(Tokenizer* tokenizer);
static ASTExpression ParseFactor(Tokenizer* tokenizer);

static ASTExpression MakeUnaryOperationExpression(ASTUnaryOperationType type, ASTExpression operand);
static ASTExpression MakeBinaryOperationExpression(ASTBinaryOperationType type, ASTExpression left, ASTExpression right);

static char* ReadFile(char* fileName)
{
	char* result = NULL;
	FILE* file = NULL;
	fopen_s(&file, fileName, "r");
	if (file)
	{
		fseek(file, 0, SEEK_END);
		size_t fileSize = ftell(file);
		fseek(file, 0, SEEK_SET);

		result = malloc(fileSize + 1);
		if (result)
		{
			size_t i;
			char c = getc(file);
			for (i = 0; i < fileSize && !(c == '\0' || c == EOF); i++, c = getc(file))
			{
				result[i] = c;
			}
			result[i] = '\0';
		}

		fclose(file);
	}

	return result;
}

static int IsWhitespace(char c)
{
	return (c == ' ' || c == '\r' || c == '\n' || c == '\t');
}

static int StringCompare(char* s1, int l1, char* s2, int l2)
{
	if (l1 == l2)
	{
		for (int i = 0; i < l1; i++)
		{
			if (s1[i] != s2[i]) return 0;
		}
		return 1;
	}

	return 0;
}

static int StringCompareConst(char* s1, char* s2, int l2)
{
	int l1 = 0;
	while (s1[l1])
	{
		l1++;
	}

	return StringCompare(s1, l1, s2, l2);
}

static int IsNumber(char c)
{
	return (c >= '0' && c <= '9');
}

static int IsAlpha(char c)
{
	return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
}

static void EatAllWhitespace(Tokenizer* tokenizer)
{
	while (IsWhitespace(tokenizer->at[0]))
	{
		tokenizer->at++;
	}

}

static Token GetToken(Tokenizer* tokenizer)
{
	EatAllWhitespace(tokenizer);
	Token token;
	token.length = 1;
	token.text = tokenizer->at++;
	switch (token.text[0])
	{
	case EOF:
	case '\0': { token.type = Token_EndOfStream; } break;
	case '{': { token.type = Token_OpenBrace; } break;
	case '}': { token.type = Token_CloseBrace; } break;
	case '(': { token.type = Token_OpenParenthesis; } break;
	case ')': { token.type = Token_CloseParenthesis; } break;
	case ';': { token.type = Token_Semicolon; } break;
	case '-': { token.type = Token_Dash; } break;
	case '~': { token.type = Token_Tilde; } break;
	case '!': { token.type = Token_ExclamationPoint; } break;
	case '/': { token.type = Token_ForwardSlash; } break;
	case '+': { token.type = Token_Plus; } break;
	case '*': { token.type = Token_Asterisk; } break;
	default:
	{
		if (IsAlpha(token.text[0]))
		{
			while (IsAlpha(tokenizer->at[0]) ||
				IsNumber(tokenizer->at[0]) ||
				tokenizer->at[0] == '_')
			{
				tokenizer->at++;
			}
			token.length = (int)(tokenizer->at - token.text);
			if (StringCompareConst("int", token.text, token.length))
			{
				token.type = Token_KeywordInt;
			}
			else if (StringCompareConst("return", token.text, token.length))
			{
				token.type = Token_KeywordReturn;
			}
			else
			{
				token.type = Token_Identifier;
			}
		}
		else if (IsNumber(token.text[0]))
		{
			// Parse number
			while (IsNumber(tokenizer->at[0]))
			{
				tokenizer->at++;
			}
			token.length = (int)(tokenizer->at - token.text);
			token.type = Token_IntegerLiteral;
		}
		else
		{
			token.type = Token_Unknown;
		}
	}
	break;
	}

	return token;
}

static Token PeekToken(Tokenizer* tokenizer)
{
	char* at = tokenizer->at;
	Token token = GetToken(tokenizer);
	tokenizer->at = at;
	return token;
}

static int RequireToken(Tokenizer* tokenizer, TokenType type)
{
	Token token = GetToken(tokenizer);
	return token.type == type;
}

static ASTFunction ParseFunction(Token token, Tokenizer* tokenizer)
{
	ASTFunction function;
	Token identifyer = GetToken(tokenizer);
	if (identifyer.type != Token_Identifier)
	{
		fprintf(stderr, "ERROR: Missing function name.\n");
	}

	function.nameLength = identifyer.length;
	for (int i = 0; i < identifyer.length; i++)
	{
		function.name[i] = identifyer.text[i];
	}

	if (RequireToken(tokenizer, Token_OpenParenthesis))
	{
		token = GetToken(tokenizer);
		if (token.type == Token_CloseParenthesis)
		{
			// No arguments
			//ParseFunctionBody(tokenizer);
			if (RequireToken(tokenizer, Token_OpenBrace))
			{
				token = GetToken(tokenizer);
				switch (token.type)
				{
				case Token_KeywordReturn:
				{
					ASTStatment statment = ParseStatment(token, tokenizer);
					function.statment = statment;
				} break;
				}
			}
		}
	}
	else
	{
		fprintf(stderr, "ERROR: Missing parenthesis.\n");
	}

	return function;
}

static ASTExpression ParseFactor(Tokenizer* tokenizer)
{
	ASTExpression expression;
	Token token = GetToken(tokenizer);
	switch (token.type)
	{
		case Token_Unknown: { assert(0); } break;
		case Token_IntegerLiteral:
		{
			expression.type = AST_Expression_Integer;
			int integer = 0;
			for (int i = 0; i < token.length; i++)
			{
				integer *= 10;
				integer += token.text[i] - '0';
			}
			expression.op.integer = integer;
		} break;
		case Token_Dash:
		{
			expression = MakeUnaryOperationExpression(AST_UnaryOp_Negate, ParseExpression(tokenizer));
		}break;
		case Token_Tilde:
		{
			expression = MakeUnaryOperationExpression(AST_UnaryOp_BitWise_Complement, ParseExpression(tokenizer));
		}break;
		case Token_ExclamationPoint:
		{
			expression = MakeUnaryOperationExpression(AST_UnaryOp_Not, ParseExpression(tokenizer));
		}break;
		case Token_OpenParenthesis:
		{
			expression = ParseExpression(tokenizer);
			if (!RequireToken(tokenizer, Token_CloseParenthesis))
			{
				fprintf(stderr, "missing closeing parenthasise");
			}
		} break;
	}

	return expression;
}

static ASTExpression ParseTerm(Tokenizer* tokenizer)
{
	ASTExpression expression = ParseFactor(tokenizer);

	Token token = PeekToken(tokenizer);
	while (token.type == Token_Asterisk || token.type == Token_ForwardSlash)
	{
		GetToken(tokenizer); // Pop the operator
		expression = MakeBinaryOperationExpression(token.type == Token_Asterisk ? AST_BinaryOp_Multiply: AST_BinaryOp_Divide, expression, ParseFactor(tokenizer));
		token = PeekToken(tokenizer);
	}

	return expression;
}

static ASTExpression ParseExpression(Tokenizer* tokenizer)
{
	ASTExpression expression = ParseTerm(tokenizer);
	Token token = PeekToken(tokenizer);
	while (token.type == Token_Plus || token.type == Token_Dash)
	{
		GetToken(tokenizer); // Pop the operator
		expression = MakeBinaryOperationExpression(token.type == Token_Plus ? AST_BinaryOp_Add : AST_BinaryOp_Minus, expression, ParseTerm(tokenizer));
		token = PeekToken(tokenizer);
	}

	return expression;
}

static ASTStatment ParseStatment(Token token, Tokenizer* tokenizer)
{
	ASTStatment statment;
	statment.type = AST_Statement_Return;
	if (token.type != Token_KeywordReturn)
	{
		fprintf(stderr, "ERROR: Statment wasn't return.\n");
	}

	ASTExpression exp = ParseExpression(tokenizer);
	statment.data = exp;
	if (!RequireToken(tokenizer, Token_Semicolon))
	{
		fprintf(stderr, "ERROR: Statment didn't use an int.\n");
	}

	return statment;
}

static ASTExpression MakeUnaryOperationExpression(ASTUnaryOperationType type, ASTExpression operand)
{
	ASTExpression expression;
	expression.type = AST_Expression_UnaryOperation;
	expression.op.unaryOp.op = type;
	expression.op.unaryOp.operand = (ASTExpression*)TempAlloc(sizeof(ASTExpression));
	*expression.op.unaryOp.operand = operand;
	return expression;
}

static ASTExpression MakeBinaryOperationExpression(ASTBinaryOperationType type, ASTExpression left, ASTExpression right)
{
	ASTExpression expression;
	expression.type = AST_Expression_BinaryOperation;
	expression.op.binaryOp.op = type;
	expression.op.binaryOp.left = (ASTExpression*)TempAlloc(sizeof(ASTExpression));
	expression.op.binaryOp.right = (ASTExpression*)TempAlloc(sizeof(ASTExpression));
	*expression.op.binaryOp.left = left;
	*expression.op.binaryOp.right = right;
	return expression;
}

static void PrintExpression(ASTExpression expression)
{
	switch (expression.type)
	{
		case AST_Expression_Integer: {
			printf("%d", expression.op.integer);
		} break;
		case AST_Expression_UnaryOperation:
		{
			switch (expression.op.unaryOp.op)
			{
				case AST_UnaryOp_Not: {	printf("(UnaryOp_Not, "); } break;
				case AST_UnaryOp_Negate: { printf("(UnaryOp_Negate, "); } break;
				case AST_UnaryOp_BitWise_Complement: {printf("(UnaryOp_Negate, "); } break;
			}
			PrintExpression(*expression.op.unaryOp.operand);
			printf(")");
		}
		case AST_Expression_BinaryOperation:
		{
			switch (expression.op.binaryOp.op)
			{
				case AST_BinaryOp_Add: { printf("(BinaryOp_Add, "); } break;
				case AST_BinaryOp_Minus: { printf("(BinaryOp_Minus, "); } break;
				case AST_BinaryOp_Divide: { printf("(BinaryOp_Divide, "); } break;
				case AST_BinaryOp_Multiply: { printf("(BinaryOp_Multiply, "); } break;
			}
			PrintExpression(*expression.op.binaryOp.left);
			printf(", ");
			PrintExpression(*expression.op.binaryOp.right);
			printf(")");
		}
	}
}

static void PrintASTStatment(ASTStatment statment)
{
	printf("    ");
	switch (statment.type)
	{
		case AST_Statement_Return: {
			printf("return ");
			PrintExpression(statment.data);
		} break;
	}
	printf(";\n");
}

static void PrintAstFunction(ASTFunction function)
{
	printf("Function: %.*s \n  {\n", function.nameLength, function.name);
	PrintASTStatment(function.statment);
	printf("  }\n");
}

static void PrintAST(ASTProgram program)
{
	printf("ast - functions: %d\n\n", 1);
	PrintAstFunction(program.function);
	printf("\n\nast - end\n\n");
}

static void ExpressionCodeGen(ASTExpression expression)
{
	switch (expression.type)
	{
		case AST_Expression_Integer: {
			printf("	mov rax,%d\n", expression.op.integer);
		} break;
		case AST_Expression_UnaryOperation: {
			ExpressionCodeGen(*expression.op.unaryOp.operand);
			switch (expression.op.unaryOp.op)
			{
				case AST_UnaryOp_Not: {
					printf("	cmp rax,0\n");
					printf("	mov rax,0\n");
					printf("	sete al\n");
				} break;
				case AST_UnaryOp_Negate: {
					printf("	neg rax\n");
				} break;
				case AST_UnaryOp_BitWise_Complement: {
					printf("	not rax\n");
				} break;
			}
		} break;
		case AST_Expression_BinaryOperation:
		{
			ExpressionCodeGen(*expression.op.binaryOp.left);
			printf("	push rax\n");
			ExpressionCodeGen(*expression.op.binaryOp.right);
			printf("	mov rdi,rax\n");
			printf("	pop rax\n");
			switch (expression.op.binaryOp.op)
			{
				case AST_BinaryOp_Add: { printf("	add rax,rdi\n"); } break;
				case AST_BinaryOp_Minus: { printf("	sub rax,rdi"); } break;
				case AST_BinaryOp_Multiply: { printf("	imul rax,rdi\n"); } break;
				case AST_BinaryOp_Divide: { printf("	xor rdx,rdx\n	idiv rdi\n"); } break;
			}

		} break;
	}

}

static void StatmentCodeGen(ASTStatment statment)
{
	switch (statment.type)
	{
		case AST_Statement_Return:
		{
			ExpressionCodeGen(statment.data);
			printf("	ret\n");
		} break;
	}
}

static void FunctionCodeGen(ASTFunction function)
{
	printf("%.*s proc\n", function.nameLength, function.name);
	StatmentCodeGen(function.statment);
	printf("%.*s endp\n", function.nameLength, function.name);
}

static void ProgramCodeGen(ASTProgram program)
{
	printf(".code\n");
	FunctionCodeGen(program.function);
	printf("\nPUBLIC main");
	printf("\n\nend");
}
int main()
{
	CreateTempAlloc(MB(16));

	char* fileContents = ReadFile("S:\\Projects\\EC\\EC\\return_2.c");
	Tokenizer tokenizer;
	tokenizer.at = fileContents;
	int parsing = 1;
	ASTProgram program;
	while (parsing)
	{
		Token token = GetToken(&tokenizer);
		switch (token.type)
		{
		case Token_EndOfStream: {
			parsing = 0;
		} break;

		case Token_KeywordInt:
		{
			ASTFunction function = ParseFunction(token, &tokenizer);
			program.function = function;
		} break;
		}
	}

	if (fileContents) free(fileContents);

	//PrintAST(program);
	// Ouptut assembly to stdout, and I will just pipe it to a file for now.
	ProgramCodeGen(program);

	//PrintTotalTempAlloc();
	DestroyTempAlloc();
	return 0;
}
