#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "utils.h"


typedef uint16_t NodeType;
enum NodeType{
	NT_Global = 0,
	NT_Newline,
	NT_Error,
	NT_OpenPar,
	NT_ClosePar,
	NT_Identifier,
	NT_Number,
	NT_Minus,
	NT_Add,
	NT_Subtract,
	NT_Multiply,
	NT_Divide,
	NT_Power,
	NT_Factorial,
};


typedef struct Node{
	NodeType type;
	uint16_t size;
	uint32_t pos;
	union{
		const char *name;
		const char *error;
		int64_t integer;
		double real;
	};
} Node;

static bool is_character(char c){
	return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

static bool is_alnum(char c){
	return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9');
}

static Node get_token(const char *line, const char **iter){
	const char *it = *iter;
	Node res = {0};

	while (*it==' ' || *it=='\t') it += 1;
	res.pos = it - line;

	switch (*it){
	case '\0':
	case '\n':
		it += 1;
		res.type = NT_Newline;
		break;
	case '0'...'9':
		res.type = NT_Number;
		res.real = strtod(it, (char **)&it);
		break;
	case '(':
		it += 1;
		res.type = NT_OpenPar;
		break;
	case ')':
		it += 1;
		res.type = NT_ClosePar;
		break;
	case '+':
		it += 1;
		res.type = NT_Add;
		break;
	case '-':
		it += 1;
		res.type = NT_Subtract;
		break;
	case '*':
		it += 1;
		res.type = NT_Multiply;
		break;
	case '/':
		it += 1;
		res.type = NT_Divide;
		break;
	case '^':
		it += 1;
		res.type = NT_Power;
		break;
	case '!':
		it += 1;
		res.type = NT_Factorial;
		break;
	default:
		if (is_character(*it)){
			res.type = NT_Identifier;
			res.name = it;
			do{ it += 1; } while (is_alnum(*it));
			res.size = it - res.name;
			if (res.size > 64){
				res.type = NT_Error;
				res.error = "identifier name too long";
				goto Return;
			}
			break;
		}
		res.type = NT_Error;
		res.error = "unrecognized token";
	}
Return:
	*iter = it;
	return res;
}


typedef uint16_t DataType;
enum DataType{
	DT_Void = 0,
	DT_Error,
	DT_Real
};

typedef struct Value{
	DataType type;
	uint16_t size;
	union{
		double real;
		int64_t integer;
		const char *string;
		const char *error;
	};
} Value;


#define SYMBOL_CAPACITY 64
typedef struct SymbolTable{
	size_t symbol_count;
	const char *names[SYMBOL_CAPACITY];
	uint8_t name_sizes[SYMBOL_CAPACITY];
	Value values[SYMBOL_CAPACITY];
} SymbolTable;

static Value get_identifier(const SymbolTable *symbols, const char *name, size_t name_size){
	for (size_t i=0; i!=symbols->symbol_count; i+=1){
		if (
			symbols->name_sizes[i] == name_size &&
			memcmp(symbols->names[i], name, name_size) == 0
		) return symbols->values[i];
	}
	return (Value){.type = DT_Error, .error = "identifier not found"};
}

static void set_identifier(SymbolTable *symbols, const char *name, size_t name_size, Value value){
	for (size_t i=0; i!=symbols->symbol_count; i+=1){
		if (
			symbols->name_sizes[i] == name_size &&
			memcmp(symbols->names[i], name, name_size) == 0
		){
			symbols->values[i] = value;
			return;
		}
	}
	size_t count = symbols->symbol_count;
	symbols->names[count] = name;
	symbols->name_sizes[count] = name_size;
	symbols->values[count] = value;
	symbols->symbol_count += 1;
}

typedef struct Precedence{
	uint8_t left;
	uint8_t right;
} Precedence;

static Precedence get_prec(NodeType oper_type){
	switch (oper_type){
	case NT_Newline:   return (Precedence){1, 0};
	case NT_Global:    return (Precedence){0, 0};
	case NT_ClosePar:  return (Precedence){1, 0};
	case NT_OpenPar:   return (Precedence){99, 0};
	case NT_Minus:     return (Precedence){59, 59};
	case NT_Add:       return (Precedence){50, 50};
	case NT_Subtract:  return (Precedence){50, 50};
	case NT_Multiply:  return (Precedence){55, 55};
	case NT_Divide:    return (Precedence){55, 55};
	case NT_Power:     return (Precedence){61, 60};
	case NT_Factorial: return (Precedence){62, 62};
	default:           return (Precedence){255, 255};
	}
}


static Value evaluate_line(const SymbolTable *symbols, const char *line){
	const char *it = line;
	Node opers[64];
	opers[0] = (Node){.type = NT_Global};
	size_t opers_size = 1;

	Value stack[64];
	size_t stack_size = 0;

#define ERROR_VALUE(msg, pos) (Value){.type=DT_Error, .size=pos, .error=msg}

	ExpectValue:{
		Node curr = get_token(line, &it);
		if (curr.type == NT_Error) return ERROR_VALUE(curr.error, curr.pos);
		
		switch (curr.type){
		case NT_OpenPar:
			opers[opers_size] = curr;
			opers_size += 1;
			goto ExpectValue;
		case NT_Add:
			goto ExpectValue;
		case NT_Subtract:
			curr.type = NT_Minus;
			opers[opers_size] = curr;
			opers_size += 1;
			goto ExpectValue;
		case NT_Identifier:
			stack[stack_size] = get_identifier(symbols, curr.name, curr.size);
			if (stack[stack_size].type == DT_Error){
				stack[stack_size].size = curr.pos;
				return stack[stack_size];
			}
			stack_size += 1;
			goto ExpectOperator;
		case NT_Number:
			stack[stack_size].type = DT_Real;
			stack[stack_size].real = curr.real;
			stack_size += 1;
			goto ExpectOperator;
		default:
			return ERROR_VALUE("expected value", curr.pos);
		}
	}
	
	ExpectOperator:{
		Node curr = get_token(line, &it);
		if (curr.type == NT_Error) return ERROR_VALUE(curr.error, curr.pos);
		
		for (;;){
			if (get_prec(opers[opers_size-1].type).right < get_prec(curr.type).left) break;
			opers_size -= 1;
			switch (opers[opers_size].type){
			case NT_Minus:
				if (stack[stack_size-1].type != DT_Real)
					return ERROR_VALUE("wrong data type", opers[opers_size].pos);
				stack[stack_size-1].real = -stack[stack_size-1].real;
				break;
			case NT_Add:
				if (stack[stack_size-1].type != DT_Real || stack[stack_size-2].type != DT_Real)
					return ERROR_VALUE("wrong data type", opers[opers_size].pos);
				stack[stack_size-2].real = stack[stack_size-2].real + stack[stack_size-1].real;
				stack_size -= 1; break;
			case NT_Subtract:
				if (stack[stack_size-1].type != DT_Real || stack[stack_size-2].type != DT_Real)
					return ERROR_VALUE("wrong data type", opers[opers_size].pos);
				stack[stack_size-2].real = stack[stack_size-2].real - stack[stack_size-1].real;
				stack_size -= 1; break;
			case NT_Multiply:
				if (stack[stack_size-1].type != DT_Real || stack[stack_size-2].type != DT_Real)
					return ERROR_VALUE("wrong data type", opers[opers_size].pos);
				stack[stack_size-2].real = stack[stack_size-2].real * stack[stack_size-1].real;
				stack_size -= 1; break;
			case NT_Divide:
				if (stack[stack_size-1].type != DT_Real || stack[stack_size-2].type != DT_Real)
					return ERROR_VALUE("wrong data type", opers[opers_size].pos);
				if (stack[stack_size-1].real == 0.0)
					return ERROR_VALUE("divide by zero", opers[opers_size].pos);
				stack[stack_size-2].real = stack[stack_size-2].real / stack[stack_size-1].real;
				stack_size -= 1; break;
			case NT_Power:
				if (stack[stack_size-1].type != DT_Real || stack[stack_size-2].type != DT_Real)
					return ERROR_VALUE("wrong data type", opers[opers_size].pos);
				if (stack[stack_size-2].real < 0.0)
					return ERROR_VALUE("negative power base", opers[opers_size].pos);
				stack[stack_size-2].real = pow(stack[stack_size-2].real, stack[stack_size-1].real);
				stack_size -= 1; break;
			default: fprintf(stderr, "ERROR: broken parser\n"); exit(1);
			}
		}

		switch (curr.type){
		case NT_Add:
		case NT_Subtract:
		case NT_Multiply:
		case NT_Divide:
		case NT_Power:
			opers[opers_size] = curr;
			opers_size += 1;
			goto ExpectValue;
		case NT_Factorial:
			if (stack[stack_size-1].type != DT_Real)
				return ERROR_VALUE("wrong data type", curr.pos);
			if (stack[stack_size-1].real < 0.0)
				return ERROR_VALUE("factorial of negative number", curr.pos);
			stack[stack_size-1].real = tgamma(1.0 + stack[stack_size-1].real);
			goto ExpectOperator;
		case NT_Newline:
			if (opers_size != 1)
				return ERROR_VALUE("parenthesis not closed", curr.pos);
			return stack[0];
		case NT_ClosePar:
			if (opers[opers_size-1].type != NT_OpenPar)
				return ERROR_VALUE("mismatched parenthesis", curr.pos);
			opers_size -= 1;
			goto ExpectOperator;
		default:
			return ERROR_VALUE("expected operator", curr.pos);
		}
	}
#undef ERROR_VALUE
}



int main(){
	char buffer[256];
	SymbolTable symbols = {0};
	set_identifier(&symbols, "e", 1, (Value){.type=DT_Real, .real=M_E});
	set_identifier(&symbols, "pi", 2, (Value){.type=DT_Real, .real=M_PI});

	for (;;){
		char *line = fgets(buffer, sizeof(buffer), stdin);
		if (line == NULL) break;
		Value res = evaluate_line(&symbols, line);
		switch (res.type){
		case DT_Error:
			for (size_t i=0; i!=res.size; i+=1) putchar(' ');
			printf("^\nERROR: %s\n", res.error);
			break;
		case DT_Real:
			printf("= %lf\n", res.real);
			break;
		}
	}

	return 0;
}
