#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
using namespace llvm;
using namespace cl;
//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
opt<std::string> InputFilename(Positional, desc("Specify input filename"), value_desc("filename"));
opt<std::string> OutputFilename("o", desc("Specify output filename"), value_desc("filename"));
static FILE* ifile_ptr;
static FILE* ofile_ptr;
enum Token {
    tok_eof = -1,

    // commands
    tok_void = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
    tok_uniform = -6,
    tok_in = -7,
    tok_out = -8,
    tok_location = -9,
    tok_binding = -10,
    tok_type = -11,
    tok_version = -12,
    tok_layout = -13,
    tok_if = -14,
    tok_else = -15,
    tok_wle = -16,
    tok_do = -17,
    tok_fr = -18,
    tok_bk = -19,
    tok_con = -20,
    tok_rt = -21,
    tok_plus_plus = -22,
    tok_sub_sub = -23,
    tok_left_left = -24,
    tok_right_right = -25,
    tok_ge = -26,
    tok_le = -27,
    tok_eq = -28,
    tok_ne = -29,
    tok_and = -30,
    tok_or = -31,
    tok_plus_eq = -32,
    tok_sub_eq = -33,
    tok_mul_eq = -34,
    tok_div_eq = -35,
    tok_mod_eq = -36,
    tok_left_eq = -37,
    tok_right_eq = -38,
    tok_or_or_eq = -39,
    tok_and_and_eq = -40,
    tok_or_eq = -41,
    tok_and_eq = -42,
    tok_const = -43,
    tok_int = -44,
    tok_vec = -45,
    tok_dot = -46,
    tok_bool = -47,
    tok_mat = -48,
    tok_float = -49,
    tok_double = -50

};

static std::string IdentifierStr; // Filled in if tok_identifier
static float NumVal;             // Filled in if tok_number
static bool is_true = true;
static bool is_valid = true;
/// gettok - Return the next token from standard input.
static int gettok() {
    static int LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = fgetc(ifile_ptr);

    if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while ((isalnum((LastChar = fgetc(ifile_ptr)))) || (LastChar == '_') || (IdentifierStr == "vec") || (IdentifierStr == "mat"))
            IdentifierStr += LastChar;
        if (IdentifierStr == "void") {
            is_valid = false;
            return tok_void;
        }
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "layout")
            return tok_layout;
        if (IdentifierStr == "uniform")
            return tok_uniform;
        if (IdentifierStr == "in")
            return tok_in;
        if (IdentifierStr == "out")
            return tok_out;
        if (IdentifierStr == "location")
            return tok_location;
        if (IdentifierStr == "binding")
            return tok_binding;
        if (IdentifierStr == "TYPE")
            return tok_type;
        if (IdentifierStr == "if")
            return tok_if;
        if (IdentifierStr == "else")
            return tok_else;
        if (IdentifierStr == "while")
            return tok_wle;
        if (IdentifierStr == "do")
            return tok_do;
        if (IdentifierStr == "for")
            return tok_fr;
        if (IdentifierStr == "con")
            return tok_con;
        if (IdentifierStr == "break")
            return tok_bk;
        if (IdentifierStr == "return")
            return tok_rt;
        if (IdentifierStr == "vec4")
            return tok_vec;
        if (IdentifierStr == "vec3")
            return tok_vec;
        if (IdentifierStr == "vec2")
            return tok_vec;
        if (IdentifierStr == "int")
            return tok_int;
        if (IdentifierStr == "float")
            return tok_float;
        if (IdentifierStr == "double")
            return tok_double;
        if (IdentifierStr == "bool")
            return tok_bool;
        if (IdentifierStr == "mat2")
            return tok_mat;
        if (IdentifierStr == "mat3")
            return tok_mat;
        if (IdentifierStr == "mat4")
            return tok_mat;
        return tok_identifier;
    }
    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = fgetc(ifile_ptr);
        } while (isdigit(LastChar) || LastChar == '.' || LastChar == 'f');
        if (NumStr.back() == 'f') { NumStr.pop_back(); }
        if (NumStr == ".") {
            //fprintf(stderr, "get a dot");
            return tok_dot;
        }
        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (LastChar == '#') {
        // Comment until end of line.
        IdentifierStr = LastChar;
        while (isalnum((LastChar = fgetc(ifile_ptr))))
            IdentifierStr += LastChar;

        if (IdentifierStr == "#version")
            return tok_version;
    }
    if (LastChar == '+') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "++") {
            //fprintf(stderr, "Error: %s\n", "++");
            return tok_plus_plus;

        }
        else if (IdentifierStr == "+=")
            return tok_plus_eq;
        else if (IdentifierStr == "+") {
            // fprintf(stderr, "Error: %s\n", "+");
            return '+';
        }
        else {
            return '+';
        }
    }

    if (LastChar == '-') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "--")
            return tok_sub_sub;
        else if (IdentifierStr == "-=")
            return tok_sub_eq;
        else if (IdentifierStr == "-")
            return '-';
        else {
            return '-';
        }
    }
    if (LastChar == '*') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "*=")
            return tok_mul_eq;
        else if (IdentifierStr == "*")
            return '*';
        else {
            return '*';
        }
    }
    if (LastChar == '/') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "/=")
            return tok_div_eq;
        else if (IdentifierStr == "/")
            return '/';
        else {
            return '/';
        }
    }
    if (LastChar == '%') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "%=")
            return tok_mod_eq;
        else if (IdentifierStr == "%")
            return '%';
        else {
            return '%';
        }
    }
    if (LastChar == '<') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 2)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "<<")
            return tok_left_left;
        else if (IdentifierStr == "<=")
            return tok_le;
        else if (IdentifierStr == "<<=")
            return tok_left_eq;
        else if (IdentifierStr == "<")
            return '<';
        else {
            return '<';
        }
    }
    if (LastChar == '>') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 2)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == ">>")
            return tok_right_right;
        else if (IdentifierStr == ">=")
            return tok_ge;
        else if (IdentifierStr == ">>=")
            return tok_right_eq;
        else if (IdentifierStr == ">")
            return '>';
        else {
            return '>';
        }
    }
    if (LastChar == '&') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 2)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "&&")
            return tok_and;
        else if (IdentifierStr == "&=")
            return tok_and_eq;
        else if (IdentifierStr == "&&=")
            return tok_and_and_eq;
        else if (IdentifierStr == "&")
            return '&';
        else {
            return '&';
        }
    }
    if (LastChar == '|') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 2)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "||")
            return tok_or;
        else if (IdentifierStr == "|=")
            return tok_or_eq;
        else if (IdentifierStr == "||=")
            return tok_or_or_eq;
        else if (IdentifierStr == "|")
            return '|';
        else {
            return '|';
        }
    }
    if (LastChar == '=') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "==")
            return tok_eq;
        else if (IdentifierStr == "=")
            return '=';
        else {
            return '=';
        }
    }
    if (LastChar == '!') {
        // Comment until end of line.
        int count = 0;
        IdentifierStr = LastChar;
        while (!isalnum(LastChar = fgetc(ifile_ptr)) && (count < 1)) {
            IdentifierStr += LastChar;
            count = count + 1;
        }
        if (IdentifierStr == "!=")
            return tok_ne;
        else if (IdentifierStr == "!") {
            //  fprintf(stderr, "Error2: %s\n", "!");
            return '!';
        }
        else {
            //  fprintf(stderr, "Error: %s\n", "!");
            return '!';
        }
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF)
        return tok_eof;

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = fgetc(ifile_ptr);
    return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

    /// ExprAST - Base class for all expression nodes.
    class ExprAST {
    public:
        virtual ~ExprAST() = default;
        virtual Value* Codegen() = 0;
    };

    /// NumberExprAST - Expression class for numeric literals like "1.0".
    class NumberExprAST : public ExprAST {
        float Val;

    public:
        NumberExprAST(float Val) : Val(Val) {}
        Value* Codegen() override;
    };
    class xyzAST : public ExprAST {
        std::string X;
        std::string Y;

    public:
        xyzAST(std::string x, std::string y) : X(x), Y(y) {}
        Value* Codegen() override;
    };
    /// VariableExprAST - Expression class for referencing a variable, like "a".
    class VariableExprAST : public ExprAST {
        std::string Name;

    public:
        VariableExprAST(const std::string& Name) : Name(Name) {}
        Value* Codegen() override;
        const std::string& getName() const { return Name; }
    };

    /// BinaryExprAST - Expression class for a binary operator.
    class BinaryExprAST : public ExprAST {
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
            std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
        Value* Codegen() override;
    };
    class unaryExprAST : public ExprAST {
        int Op;
        std::unique_ptr<ExprAST> exp;
        bool is_left;

    public:
        unaryExprAST(int op, std::unique_ptr<ExprAST> LHS, bool is)
            : Op(op), exp(std::move(LHS)), is_left(is) {}
        Value* Codegen() override;
    };
    class tripleExprAST : public ExprAST {
        std::unique_ptr<ExprAST> expor;
        std::unique_ptr<ExprAST> exp1;
        std::unique_ptr<ExprAST> exp2;

    public:
        tripleExprAST(std::unique_ptr<ExprAST> Exp, std::unique_ptr<ExprAST> Exp1,
            std::unique_ptr<ExprAST> Exp2)
            : expor(std::move(Exp)), exp1(std::move(Exp1)), exp2(std::move(Exp2)) {}
        Value* Codegen();
    };

    /// CallExprAST - Expression class for function calls.
    class CallExprAST : public ExprAST {
        std::string Callee;
        std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string& Callee,
            std::vector<std::unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(std::move(Args)) {}
        Value* Codegen() override;
    };

    /// PrototypeAST - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    class PrototypeAST {
        std::string Name;
        std::vector<std::string> types;
        std::vector<std::string> Args;

    public:
        PrototypeAST(const std::string& Name, std::vector<std::string> types, std::vector<std::string> Args)
            : Name(Name), types(std::move(types)), Args(std::move(Args)) {}
        Function* Codegen();
        const std::string& getName() const { return Name; }

    };

    /// FunctionAST - This class represents a function definition itself.

    class sentenceAST : public ExprAST {
        std::unique_ptr<ExprAST> Id;
        std::unique_ptr<ExprAST> Cond;

    public:
        sentenceAST(std::unique_ptr<ExprAST> id, std::unique_ptr<ExprAST> cond)
            : Id(std::move(id)), Cond(std::move(cond)) {}
        Value* Codegen() override;
    };
    class FunctionAST : public ExprAST {
        std::string Type;
        std::unique_ptr<PrototypeAST> Proto;
        std::vector<std::unique_ptr<sentenceAST>> Body;

    public:
        FunctionAST(std::string type, std::unique_ptr<PrototypeAST> proto,
            std::vector<std::unique_ptr<sentenceAST>> body)
            : Type(type), Proto(std::move(proto)), Body(std::move(body)) {}
        Function* Codegen();
    };
    class defvariableAST : public ExprAST {
        std::string Type;
        std::string Id;
        std::vector<std::unique_ptr<ExprAST>> Expr;
        std::vector<double> E;
        bool Isvec;
        bool Isconst;

    public:
        defvariableAST(std::string proto, std::string id,
            std::vector<std::unique_ptr<ExprAST>> body, std::vector<double> e, bool isconst,
            bool isvec)
            : Type(std::move(proto)), Id(std::move(id)), Expr(std::move(body)), E(std::move(e)),
            Isconst(std::move(isconst)), Isvec(std::move(isvec)) {}
        defvariableAST(std::string proto, std::string id,
            std::vector<std::unique_ptr<ExprAST>> body, bool isconst,
            bool isvec)
            : Type(std::move(proto)), Id(std::move(id)), Expr(std::move(body)),
            Isconst(std::move(isconst)), Isvec(std::move(isvec)) {}
        Value* Codegen() override;
    };
    class IfAST : public ExprAST {
        std::unique_ptr<ExprAST> Cond;
        std::vector<std::unique_ptr<sentenceAST>> Then;
        std::vector<std::unique_ptr<sentenceAST>> Else;

    public:
        IfAST(std::unique_ptr<ExprAST> cond,
            std::vector<std::unique_ptr<sentenceAST>> then,
            std::vector<std::unique_ptr<sentenceAST>> else_st)
            : Cond(std::move(cond)), Then(std::move(then)), Else(std::move(else_st)) {
        }
        Value* Codegen() override;
    };
    class bindingAST : public ExprAST {
        std::vector<double> Args;

    public:
        bindingAST(std::vector<double> args) : Args(std::move(args)) {}
        Value* Codegen();
    };
    class locationAST : public ExprAST {
        std::vector<double> Args;

    public:
        locationAST(std::vector<double> args) : Args(std::move(args)) {}
        Value* Codegen();
    };
    class layoutAST : public ExprAST {
        std::unique_ptr<bindingAST> Bind_st;
        std::unique_ptr<locationAST> Loc_st;

    public:
        layoutAST(std::unique_ptr<bindingAST> bind_st,
            std::unique_ptr<locationAST> loc_st)
            : Bind_st(std::move(bind_st)), Loc_st(std::move(loc_st)) {}
        Value* Codegen();

    };
    class lydefaultAST : public ExprAST {
        std::unique_ptr<layoutAST> Lay_ast;
        std::string Id_Type;
        std::string Type;
        std::string Name;

    public:
        lydefaultAST(std::unique_ptr<layoutAST> lay_ast, std::string id_type,
            std::string type, std::string name)
            : Lay_ast(std::move(lay_ast)), Id_Type(std::move(id_type)),
            Type(std::move(type)), Name(std::move(name)) {}
        Value* Codegen() override;
    };
    class whileAST : public ExprAST {
        std::unique_ptr<ExprAST> Cond;
        std::vector<std::unique_ptr<sentenceAST>> Body;

    public:
        whileAST(std::unique_ptr<ExprAST> cond,
            std::vector<std::unique_ptr<sentenceAST>> body)
            : Cond(std::move(cond)), Body(std::move(body)) {}
        Value* Codegen();
    };
    class doAST : public ExprAST {
        std::unique_ptr<ExprAST> Cond;
        std::vector<std::unique_ptr<sentenceAST>> Body;

    public:
        doAST(std::unique_ptr<ExprAST> cond,
            std::vector<std::unique_ptr<sentenceAST>> body)
            : Cond(std::move(cond)), Body(std::move(body)) {}
        Value* Codegen();
    };
    class forAST : public ExprAST {
        std::unique_ptr<ExprAST> Ini;
        std::unique_ptr<ExprAST> Cond;
        std::unique_ptr<ExprAST> Ss;
        std::vector<std::unique_ptr<sentenceAST>> Body;

    public:
        forAST(std::unique_ptr<ExprAST> ini, std::unique_ptr<ExprAST> cond,
            std::unique_ptr<ExprAST> ss,
            std::vector<std::unique_ptr<sentenceAST>> body)
            : Ini(std::move(ini)), Cond(std::move(cond)), Ss(std::move(ss)),
            Body(std::move(body)) {}
        Value* Codegen() override;
    };
    class verAST : public ExprAST {
        std::unique_ptr<ExprAST> Ver;
        bool Is_core;

    public:
        verAST(std::unique_ptr<ExprAST> ver, bool is_core)
            : Ver(std::move(ver)), Is_core(is_core) {}
        Value* Codegen();
    };
}; // namespace

// end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.

static int CurTok;
static int getNextToken() { return CurTok = gettok(); }
static bool is_eq = false;
/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (!isascii(CurTok) && (CurTok > -20)) {
        return -1;
    }
    int TokPrec = 0;
    // Make sure it's a declared binop.
    if (CurTok > -20) {
        TokPrec = BinopPrecedence[CurTok];
    }
    else {
        switch (CurTok) {
        case tok_plus_plus:
            TokPrec = 65;
            break;
        case tok_sub_sub:
            TokPrec = 65;
            break;
        case tok_left_left:
            TokPrec = 50;
            break;
        case tok_right_right:
            TokPrec = 50;
            break;
        case tok_ge:
            TokPrec = 45;
            break;
        case tok_le:
            TokPrec = 45;
            break;
        case tok_eq:
            TokPrec = 40;
            break;
        case tok_ne:
            TokPrec = 40;
            break;
        case tok_and:
            TokPrec = 20;
            break;
        case tok_or:
            TokPrec = 15;
            break;
        case tok_plus_eq:
            TokPrec = 5;
            break;
        case tok_sub_eq:
            TokPrec = 5;
            break;
        case tok_mul_eq:
            TokPrec = 5;
            break;
        case tok_div_eq:
            TokPrec = 5;
            break;
        case tok_mod_eq:
            TokPrec = 5;
            break;
        case tok_left_eq:
            TokPrec = 5;
            break;
        case tok_right_eq:
            TokPrec = 5;
            break;
        case tok_or_or_eq:
            TokPrec = 5;
            break;
        case tok_and_and_eq:
            TokPrec = 5;
            break;
        case tok_or_eq:
            TokPrec = 5;
            break;
        case tok_and_eq:
            TokPrec = 5;
            break;
        case tok_dot:
            TokPrec = 90;
            break;
        }
        // fprintf(stderr, "Curtok: %d\n", CurTok);
        // fprintf(stderr, "tokprec: %d\n", TokPrec);
    }
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char* Str) {
    // fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char* Str) {
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<defvariableAST> Parsevec();
static std::unique_ptr<defvariableAST> Parsemat();
static std::unique_ptr<ExprAST> ParsePrimary();
/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurTok != ')') {
        return nullptr;
        is_true = false;
    }
    getNextToken(); // eat ).
    return V;
}
static std::unique_ptr<unaryExprAST>
ParseunaryExpr(int op, bool is_left, std::unique_ptr<ExprAST> LHS) {
    if (is_left == true) {
        getNextToken();
        auto a = ParsePrimary();
        return std::make_unique<unaryExprAST>(std::move(op), std::move(a),
            std::move(is_left));
    }
    else {
        getNextToken();
        return std::make_unique<unaryExprAST>(std::move(op), std::move(LHS),
            std::move(is_left));
    }
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;

    getNextToken(); // eat identifier.

    if (CurTok != '(') // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName);

    // Call.
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == ')')
                break;

            if (CurTok != ',') {
                fprintf(stdout, "Reject");
                exit(0);
            }
            getNextToken();
        }
    }

    // Eat the ')'.
    getNextToken();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    std::unique_ptr<ExprAST>aa;
    std::string y;
    // fprintf(stderr, "Curtok: %d\n", CurTok);
    switch (CurTok) {
    default:
        fprintf(stdout, "Reject");
        exit(0);
    case tok_identifier:
        y = IdentifierStr;
        aa = ParseIdentifierExpr();
        if (CurTok == tok_dot) {
            getNextToken();
            std::string x = IdentifierStr;
            getNextToken();
            return std::make_unique<xyzAST>(std::move(y), std::move(x));
        }
        return aa;
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case tok_plus_plus:
        return ParseunaryExpr(tok_plus_plus, true, nullptr);
    case tok_sub_sub:
        return ParseunaryExpr(tok_sub_sub, true, nullptr);
    case '!':
        return ParseunaryExpr('!', true, nullptr);
    case '~':
        return ParseunaryExpr('~', true, nullptr);
    case '^':
        return ParseunaryExpr('^', true, nullptr);
    case '+':
        return ParseunaryExpr('+', true, nullptr);
    case '-':
        return ParseunaryExpr('-', true, nullptr);
    case tok_vec:
        return Parsevec();
    case tok_mat:
        return Parsemat();
    }
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
    std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence.
    while (true) {
        int TokPrec = GetTokPrecedence();
        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;

        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop
        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if (!RHS)
            return nullptr;
        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }
        //  fprintf(stderr, "Curtok: %d\n", CurTok);
          // Merge LHS/RHS.
        LHS =
            std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    // fprintf(stderr, "parseprimary.\n");
    if (!LHS)
        return nullptr;
    if (GetTokPrecedence() == -1) {
        //  fprintf(stderr, "return LHS.\n");
        return LHS;
    }
    if (CurTok == tok_dot) {
        getNextToken();
        if (IdentifierStr == "x") {
            getNextToken();
        }
        if (IdentifierStr == "y") {
            getNextToken();
        }
        if (IdentifierStr == "z") {
            getNextToken();
        }
        if (IdentifierStr == "w") {
            getNextToken();
        }
        if (IdentifierStr == "xyz") {
            getNextToken();
        }
        if (IdentifierStr == "xy") {
            getNextToken();
        }
        if (IdentifierStr == "xz") {
            getNextToken();
        }
        if (IdentifierStr == "yz") {
            getNextToken();
        }
    }
    if (GetTokPrecedence() == -1) {
        //  fprintf(stderr, "return LHS.\n");
        return LHS;
    }
    if ((CurTok == tok_plus_plus) || (CurTok == tok_sub_sub)) {
        // fprintf(stderr, "before parse.\n");
        auto a = ParseunaryExpr(CurTok, false, std::move(LHS));
        // fprintf(stderr, "after parse.\n");
        if (GetTokPrecedence() == -1) {
            return a;
        }
    }
    if (CurTok == '?') {
        auto exp1 = ParseExpression();
        if (CurTok == ':')
            getNextToken();
        auto exp2 = ParseExpression();
        return std::make_unique<tripleExprAST>(std::move(LHS), std::move(exp1),
            std::move(exp2));
    }
    return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier) {
        fprintf(stdout, "Reject");
        exit(0);
    }
    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    std::vector<std::string> Argtypes;
    std::vector<std::string> ArgNames;
    while (CurTok != ')') {
        if (CurTok == ',') {
            getNextToken();
        }
        if (CurTok == tok_int) {
            Argtypes.push_back("int");
            getNextToken();
        }
        else if (CurTok == tok_vec) {
            Argtypes.push_back(IdentifierStr);
            getNextToken();
        }
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        if (CurTok == tok_identifier) {
            ArgNames.push_back(IdentifierStr);
            getNextToken();
        }
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
    }
    if (CurTok != ')')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }

    // success.
    getNextToken(); // eat ')'.

    return std::make_unique<PrototypeAST>(FnName, std::move(Argtypes), std::move(ArgNames));
}
static std::unique_ptr<defvariableAST> Parsemat() {
    std::string type = IdentifierStr;
    int val = IdentifierStr.back() - '0';
    int count = 0;
    getNextToken();
    std::unique_ptr<ExprAST>t_exp;
    std::vector<std::unique_ptr<ExprAST>> e_vec;
    std::vector <double>e;
    if (CurTok == '(') {
        getNextToken();
        //  fprintf(stderr, "CurTok%d", CurTok);
        while (true) {
            if (CurTok == tok_number) {
                auto t_exp = ParseExpression();
                e.push_back(10);
                count = count + 1;
            }
            else if (CurTok == tok_identifier) {
                t_exp = ParseIdentifierExpr();
                getNextToken();
                if (IdentifierStr == "x") {
                    e.push_back(0);
                    count = count + 1;
                }
                if (IdentifierStr == "y") {
                    e.push_back(1);
                    count = count + 1;
                }
                if (IdentifierStr == "z") {
                    e.push_back(2);
                    count = count + 1;
                }
                if (IdentifierStr == "w") {
                    e.push_back(3);
                    count = count + 1;
                }
                if (IdentifierStr == "xy") {
                    e.push_back(4);
                    count = count + 2;
                }
                if (IdentifierStr == "yz") {
                    e.push_back(5);
                    count = count + 2;
                }
                if (IdentifierStr == "xz") {
                    e.push_back(6);
                    count = count + 2;
                }
                if (IdentifierStr == "xyz") {
                    e.push_back(7);
                    count = count + 3;
                }
                getNextToken();
            }
            e_vec.push_back(std::move(t_exp));
            if (CurTok == ',') {
                getNextToken();
            }
            else if (CurTok == ')') {
                getNextToken();
                if ((val * val) != count) {
                    fprintf(stdout, "Reject");
                    exit(0);

                }
                return std::make_unique<defvariableAST>(
                    std::move(type), std::move(nullptr), std::move(e_vec), std::move(e),
                    std::move(false), std::move(false));

            }
            else {
                break;
            }
        }
    }
    return nullptr;
}

static std::unique_ptr<defvariableAST> Parsevec() {
    std::string type = IdentifierStr;
    int val = IdentifierStr.back() - '0';
    std::string id = IdentifierStr;
    int count = 0;
    getNextToken();
    std::unique_ptr<ExprAST>t_exp;
    std::vector<std::unique_ptr<ExprAST>> e_vec;
    std::vector <double>e;
    if (CurTok == '(') {
        getNextToken();
        //  fprintf(stderr, "CurTok%d", CurTok);
        while (true) {
            t_exp = ParseExpression();
            e_vec.push_back(std::move(t_exp));
            count = count + 1;
            if (CurTok == ',') {
                getNextToken();
            }
            if (CurTok == ')') {
                getNextToken();
                if (val != count) {
                    fprintf(stdout, "Reject");
                    exit(0);
                }
                return std::make_unique<defvariableAST>(
                    std::move(type), std::move(id), std::move(e_vec), std::move(e),
                    std::move(false), std::move(false));

            }
        }
    }
    return nullptr;
}
/*
/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {

    if (auto E = ParseExpression()) {
        // Make an anonymous proto.
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
            std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }

    return nullptr;
}
*/
static std::unique_ptr<defvariableAST> Parsevariabledef(bool isconst) {
    std::string type = "";
    bool isvec = false;
    std::vector<std::unique_ptr<ExprAST>> e_vec;
    if (CurTok == tok_int) {
        type = "int";
        getNextToken();
    }
    if (CurTok == tok_bool) {
        type = "bool";
        getNextToken();
    }
    if (CurTok == tok_double) {
        type = "double";
        getNextToken();
    }
    if (CurTok == tok_float) {
        type = "float";
        getNextToken();
    }
    if (CurTok == tok_in) {
        type = "in";
        getNextToken();
        std::string id;
        if (CurTok == tok_identifier) {
            id = IdentifierStr;
        }
        else if (CurTok == tok_vec) {
            id = IdentifierStr;
            getNextToken();
        }
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        if (CurTok != tok_identifier) {
            fprintf(stdout, "Reject");
            exit(0);
        }
        auto exp = ParseIdentifierExpr();
        e_vec.push_back(std::move(exp));
        if (CurTok != ';') {
            fprintf(stdout, "Reject");
            exit(0);
        }
        // fprintf(stderr, "handle an out");
        return std::make_unique<defvariableAST>(
            std::move(type), std::move(id), std::move(e_vec), std::move(isconst),
            std::move(isvec));
    }
    if (CurTok == tok_out) {
        type = "out";
        getNextToken();
        std::string id;
        if (CurTok == tok_identifier) {
            id = IdentifierStr;
        }
        else if (CurTok == tok_vec) {
            id = IdentifierStr;
            getNextToken();
        }
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        if (CurTok != tok_identifier) {
            fprintf(stdout, "Reject");
            exit(0);
        }
        auto exp = ParseIdentifierExpr();
        e_vec.push_back(std::move(exp));
        if (CurTok != ';') {
            fprintf(stdout, "Reject");
            exit(0);
        }
        // fprintf(stderr, "handle an out");
        return std::make_unique<defvariableAST>(
            std::move(type), std::move(id), std::move(e_vec), std::move(isconst),
            std::move(isvec));
    }
    if (CurTok == tok_uniform) {
        type = "uniform";
        getNextToken();
        std::string id;
        if (CurTok == tok_identifier) {
            id = IdentifierStr;
        }
        else if (CurTok == tok_vec) {
            id = IdentifierStr;
            getNextToken();
        }
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        if (CurTok != tok_identifier) {
            fprintf(stdout, "Reject");
            exit(0);
        }
        auto exp = ParseIdentifierExpr();
        e_vec.push_back(std::move(exp));
        if (CurTok != ';') {
            fprintf(stdout, "Reject");
            exit(0);
        }
        // fprintf(stderr, "handle an out");
        return std::make_unique<defvariableAST>(
            std::move(type), std::move(id), std::move(e_vec), std::move(isconst),
            std::move(isvec));
    }
    int val;
    if (CurTok == tok_vec) {
        type = IdentifierStr;
        val = IdentifierStr.back();
        getNextToken();
        isvec = true;
    }
    if (CurTok == tok_mat) {
        type = IdentifierStr;
        val = IdentifierStr.back();
        getNextToken();
        isvec = false;
    }
    std::string id = IdentifierStr;
    getNextToken();
    // if (CurTok != '=')
    // return nullptr;
    if (CurTok == '=') {
        getNextToken();
    }
    int count = 1;


    auto exp = ParseExpression();
    e_vec.push_back(std::move(exp));
    return std::make_unique<defvariableAST>(std::move(type), std::move(id),
        std::move(e_vec), std::move(isconst),
        std::move(isvec));
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // eat extern.
    return ParsePrototype();
}
static std::unique_ptr<IfAST> ParseIf();
static std::unique_ptr<whileAST> Parsewhile();
static std::unique_ptr<doAST> Parsedo();
static std::unique_ptr<forAST> Parsefor();
static std::unique_ptr<sentenceAST> Parsesinglesentence() {
    if (CurTok == tok_const) {
        getNextToken();
        auto def = Parsevariabledef(true);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("constdef")),
            std::move(def));
    }
    if (CurTok == tok_vec) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("vectordef")),
            std::move(def));
    }
    if (CurTok == tok_int) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_double) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_float) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_bool) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_in) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_out) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_uniform) {
        auto def = Parsevariabledef(false);
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("functiondef")),
            std::move(def));
    }
    if (CurTok == tok_bk) {
        getNextToken();
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("break")),
            std::move(nullptr));
    }
    if (CurTok == tok_con) {
        getNextToken();
        if (CurTok == ';')
            getNextToken();
        else {
            fprintf(stdout, "Reject");
            exit(0);
        }
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("continue")),
            std::move(nullptr));
    }
    if (CurTok == tok_rt) {
        getNextToken();
        if (CurTok == ';') {
            getNextToken();
            return std::make_unique<sentenceAST>(
                std::move(std::make_unique<VariableExprAST>("return")),
                std::move(nullptr));
        }
        else {
            auto exp = ParseExpression();
            if (CurTok == ';') {
                getNextToken();
                return std::make_unique<sentenceAST>(
                    std::move(std::make_unique<VariableExprAST>("return")),
                    std::move(exp));
            }
        }
    }

    if (CurTok == tok_if) {
        auto bin = ParseIf();
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("if")), std::move(bin));
    }
    if (CurTok == tok_wle) {
        auto bin = Parsewhile();
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("while")), std::move(bin));
    }
    if (CurTok == tok_do) {
        auto bin = Parsedo();
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("do")), std::move(bin));
    }
    if (CurTok == tok_fr) {
        auto bin = Parsefor();
        return std::make_unique<sentenceAST>(
            std::move(std::make_unique<VariableExprAST>("for")), std::move(bin));
    }
    /*
    auto id = ParseIdentifierExpr();
    if (CurTok != '=')
        return nullptr;
    getNextToken();
    */
    auto bin = ParseExpression();
    if (CurTok == ';') {
        getNextToken();
    }
    else {
        fprintf(stdout, "Reject");
        exit(0);
    }
    return std::make_unique<sentenceAST>(
        std::move(std::make_unique<VariableExprAST>("normal")), std::move(bin));
}
static std::vector<std::unique_ptr<sentenceAST>> Parsesentence() {
    std::vector<std::unique_ptr<sentenceAST>> sen_vec;
    if (CurTok == '}')
        return sen_vec;
    auto sen1 = Parsesinglesentence();
    sen_vec.push_back(std::move(sen1));
    while (CurTok != '}') {
        auto sen2 = Parsesinglesentence();
        sen_vec.push_back(std::move(sen2));
    }

    return sen_vec;
}
static std::unique_ptr<FunctionAST> ParseDefinition() {
    std::string type;
    if (CurTok == tok_void)
        type = "void";
    if (CurTok == tok_int)
        type = "int";
    getNextToken(); // eat void.
    std::vector<std::unique_ptr<sentenceAST>> null_vec;
    auto Proto = ParsePrototype();

    if (!Proto) {
        // fprintf(stderr, "ERROR 1 Don't Parsed a function definition.\n");
        fprintf(stdout, "Reject");
        exit(0);
        return std::make_unique<FunctionAST>(std::move(type), std::move(nullptr),
            std::move(null_vec));
    }
    if (CurTok != '{') {
        // fprintf(stderr, "%d", CurTok);
        fprintf(stdout, "Reject");
        exit(0);
        return std::make_unique<FunctionAST>(std::move(type), std::move(nullptr),
            std::move(null_vec));
    }
    getNextToken();
    auto E = Parsesentence();
    if (CurTok != '}') {
        fprintf(stdout, "Reject");
        exit(0);
        // fprintf(stderr, "ERROR 3 Don't Parsed a function definition.\n");
        return std::make_unique<FunctionAST>(std::move(type), std::move(nullptr),
            std::move(null_vec));
    }
    getNextToken();
    return std::make_unique<FunctionAST>(std::move(type), std::move(Proto),
        std::move(E));
}
static std::unique_ptr<IfAST> ParseIf() {
    //读取下一个字符，因为当前的Current_token是If_token，所以需要读入下一个
    std::vector<std::unique_ptr<sentenceAST>> null_vec;
    getNextToken();
    if (CurTok != '(') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    auto Cond = ParseExpression();
    if (CurTok != ')') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    if (!Cond) {
        fprintf(stdout, "Reject");
        exit(0);
    }
    if (CurTok != '{') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    //解析then的表达式
    auto Then = Parsesentence();
    if (CurTok != '}') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    if (CurTok != tok_else)
        return std::make_unique<IfAST>(std::move(Cond), std::move(Then),
            std::move(null_vec));
    getNextToken();
    if (CurTok != '{') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    //判断当前的TOKEN是不是ELSE_TOKEN
    auto Else = Parsesentence();
    if (CurTok != '}') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    return std::make_unique<IfAST>(std::move(Cond), std::move(Then),
        std::move(Else));
}
static std::unique_ptr<whileAST> Parsewhile() {
    getNextToken();
    if (CurTok != '(') {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    auto Cond = ParseExpression();
    if (CurTok != ')')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    if (!Cond)
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    if (CurTok != '{')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    //解析then的表达式
    auto Then = Parsesentence();
    if (CurTok != '}')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    return std::make_unique<whileAST>(std::move(Cond), std::move(Then));
}
static std::unique_ptr<doAST> Parsedo() {
    getNextToken();
    if (CurTok != '(')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    auto Body = Parsesentence();
    if (CurTok != ')')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    if (CurTok != '{')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    //解析then的表达式
    auto Cond = ParseExpression();
    if (Cond == 0)
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    if (CurTok != '}')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    return std::make_unique<doAST>(std::move(Cond), std::move(Body));
}
static std::unique_ptr<forAST> Parsefor() {
    getNextToken();
    if (CurTok != '(')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    auto Ini = Parsesinglesentence();
    auto Cond = ParseExpression();
    if (CurTok != ';')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    auto Ss = ParseExpression();
    if (CurTok != ')')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    if (!Cond)
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    if (CurTok != '{')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    //解析then的表达式
    auto Then = Parsesentence();
    if (CurTok != '}')
    {
        fprintf(stdout, "Reject");
        exit(0);
    }
    getNextToken();
    return std::make_unique<forAST>(std::move(Ini), std::move(Cond),
        std::move(Ss), std::move(Then));
}
static std::unique_ptr<lydefaultAST> Parselayout() {
    std::vector<double> bindargs;
    std::vector<double> locargs;
    std::string id_name;
    std::string type;
    getNextToken();
    if (CurTok != '(')
        return nullptr;
    getNextToken();

    while (true) {
        if (CurTok == tok_binding) {
            getNextToken();
            if (CurTok != '=')
                return nullptr;
            getNextToken();
            bindargs.push_back(NumVal);
            getNextToken();
        }

        else if (CurTok == tok_location) {
            getNextToken();
            if (CurTok != '=')
                return nullptr;
            getNextToken();
            bindargs.push_back(NumVal);
            getNextToken();
        }
        else if (CurTok == ',') {
            getNextToken();
        }
        else {
            break;
        }
    }
    if (CurTok != ')')
        return nullptr;
    getNextToken();
    auto bind_ast = std::make_unique<bindingAST>(std::move(bindargs));
    auto loc_ast = std::make_unique<locationAST>(std::move(locargs));
    auto ly_ast =
        std::make_unique<layoutAST>(std::move(bind_ast), std::move(loc_ast));
    if (CurTok == tok_in) {
        id_name = "in";
    }
    else if (CurTok == tok_out) {
        id_name = "out";
    }
    else if (CurTok == tok_uniform) {
        id_name = "uniform";
    }
    else {
        return nullptr;
    }
    getNextToken();
    type = IdentifierStr;

    getNextToken();
    if (CurTok != tok_identifier)
        return nullptr;
    auto name = IdentifierStr;
    getNextToken();
    if (CurTok != ';')
        return nullptr;
    getNextToken();
    return std::make_unique<lydefaultAST>(std::move(ly_ast), std::move(id_name),
        std::move(type), std::move(name));
}
static std::unique_ptr<verAST> Parseversion() {
    getNextToken(); // eat version.
    if (CurTok != tok_number) {
        is_true = false;
        return nullptr;
    }
    auto val = ParseNumberExpr();
    if (IdentifierStr == "core") {
        getNextToken();
        return std::make_unique<verAST>(std::move(val), true);
    }
    return std::make_unique<verAST>(std::move(val), false);
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value*> NamedValues;
static std::map<std::string, llvm::Type*> globaltype;
static std::map<std::string, llvm::Type*> localtype;


Value* LogErrorV(const char* Str) {
    LogError(Str);
    return nullptr;
}

Value* NumberExprAST::Codegen() {
    return ConstantFP::get(Type::getFloatTy(*TheContext), Val);
}

Value* xyzAST::Codegen() {
    if (!is_eq) {
        // Look this variable up in the function.
        Value* vec1 = TheModule->getNamedGlobal(X);
        Value* te = Builder->CreateAlloca(Type::getFloatTy(*TheContext), nullptr, "xyztemp");
        if (vec1) {
            vec1 = Builder->CreateLoad(globaltype[X], vec1);
            if (Y == "x") {
                auto e0 = Builder->CreateExtractElement(vec1, ConstantInt::get(Type::getInt32Ty(*TheContext), 0));
                Builder->CreateStore(e0, te);
                return Builder->CreateLoad(Type::getFloatTy(*TheContext), te);
            }
            if (Y == "y") {
                auto e0 = Builder->CreateExtractElement(vec1, ConstantInt::get(Type::getInt32Ty(*TheContext), 1));
                Builder->CreateStore(e0, te);
                return Builder->CreateLoad(Type::getFloatTy(*TheContext), te);
            }
            if (Y == "z") {
                auto e0 = Builder->CreateExtractElement(vec1, ConstantInt::get(Type::getInt32Ty(*TheContext), 2));
                Builder->CreateStore(e0, te);
                return Builder->CreateLoad(Type::getFloatTy(*TheContext), te);
            }
        }
        Value* V = NamedValues[X];
        V = Builder->CreateLoad(localtype[X], V);
        if (Y == "x") {
            auto e0 = Builder->CreateExtractElement(V, ConstantInt::get(Type::getInt32Ty(*TheContext), 0));
            Builder->CreateStore(e0, te);
            return Builder->CreateLoad(Type::getFloatTy(*TheContext), te);
        }
        if (Y == "y") {
            auto e0 = Builder->CreateExtractElement(V, ConstantInt::get(Type::getInt32Ty(*TheContext), 1));
            Builder->CreateStore(e0, te);
            return Builder->CreateLoad(Type::getFloatTy(*TheContext), te);
        }
        if (Y == "z") {
            auto e0 = Builder->CreateExtractElement(V, ConstantInt::get(Type::getInt32Ty(*TheContext), 2));
            Builder->CreateStore(e0, te);
            return Builder->CreateLoad(Type::getFloatTy(*TheContext), te);
        }
        if (!V)
            return nullptr;
    }
    else {
        Value* vec1 = TheModule->getNamedGlobal(X);
        if (vec1) {
            if (Y == "x") {
                return  Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), vec1, ConstantInt::get(Type::getInt32Ty(*TheContext), 0)), Type::getFloatTy(*TheContext)->getPointerTo());
            }
            if (Y == "y") {
                return  Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), vec1, ConstantInt::get(Type::getInt32Ty(*TheContext), 1)), Type::getFloatTy(*TheContext)->getPointerTo());
            }
            if (Y == "z") {
                return  Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), vec1, ConstantInt::get(Type::getInt32Ty(*TheContext), 2)), Type::getFloatTy(*TheContext)->getPointerTo());
            }
        }
        Value* V = NamedValues[X];
        if (Y == "x") {
            return  Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), V, ConstantInt::get(Type::getInt32Ty(*TheContext), 0)), Type::getFloatTy(*TheContext)->getPointerTo());
        }
        if (Y == "y") {
            return  Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), V, ConstantInt::get(Type::getInt32Ty(*TheContext), 1)), Type::getFloatTy(*TheContext)->getPointerTo());
        }
        if (Y == "z") {
            return  Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), V, ConstantInt::get(Type::getInt32Ty(*TheContext), 2)), Type::getFloatTy(*TheContext)->getPointerTo());
        }
        if (!V)
            return nullptr;
    }
}


Value* lydefaultAST::Codegen() {
    if (Type == "vec3") {
        llvm::Type* vecType = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 3, false);
        auto var = TheModule->getOrInsertGlobal(Name, vecType);
        globaltype[Name] = vecType;
        Value* vec1 = TheModule->getNamedGlobal("gl_Position");
        if (!vec1) {
            llvm::Type* vecType1 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
            auto var = TheModule->getOrInsertGlobal("gl_Position", vecType1);
            globaltype["gl_Position"] = vecType1;
        }
        return var;
    }
    if (Type == "vec2") {
        llvm::Type* vecType = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 2, false);
        auto var = TheModule->getOrInsertGlobal(Name, vecType);
        Value* vec1 = TheModule->getNamedGlobal("gl_position");
        if (!vec1) {
            llvm::Type* vecType1 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
            auto var = TheModule->getOrInsertGlobal("gl_Position", vecType1);
            globaltype["gl_Position"] = vecType1;
        }
        return var;
    }
    if (Type == "vec4") {
        llvm::Type* vecType = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
        auto var = TheModule->getOrInsertGlobal(Name, vecType);
        globaltype[Name] = vecType;
        Value* vec1 = TheModule->getNamedGlobal("gl_position");
        if (!vec1) {
            llvm::Type* vecType1 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
            auto var = TheModule->getOrInsertGlobal("gl_Position", vecType1);
            globaltype["gl_Position"] = vecType1;
        }
        return var;
    }
    if (Type == "int") {
        auto var = TheModule->getOrInsertGlobal(Name, llvm::Type::getInt32Ty(*TheContext));
        globaltype[Name] = llvm::Type::getInt32Ty(*TheContext);
        Value* vec1 = TheModule->getNamedGlobal("gl_position");
        if (!vec1) {
            llvm::Type* vecType1 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
            auto var = TheModule->getOrInsertGlobal("gl_Position", vecType1);
            globaltype["gl_Position"] = vecType1;
        }
        return var;
    }
    return nullptr;
}
Value* VariableExprAST::Codegen() {
    if (!is_eq) {
        // Look this variable up in the function.
        Value* vec1 = TheModule->getNamedGlobal(Name);
        if (vec1) {
            return Builder->CreateLoad(globaltype[Name], vec1);
        }
        Value* V = NamedValues[Name];
        if (!V)
            return nullptr;
        return Builder->CreateLoad(localtype[Name], V);
    }
    else {
        Value* vec1 = TheModule->getNamedGlobal(Name);
        if (vec1) {
            return vec1;
        }
        Value* V = NamedValues[Name];
        if (!V)
            return nullptr;
        return V;
    }
}

Value* unaryExprAST::Codegen() {
    Value* L = exp->Codegen();
    if (L->getType() == Type::getFloatTy(*TheContext)) {

        switch (Op) {
        case '+':
            return Builder->CreateFAdd(L, ConstantFP::get(Type::getFloatTy(*TheContext), 0.0));
        case '-':
            return Builder->CreateFNeg(L);
        case '!':
            return Builder->CreateFCmpOEQ(L, ConstantFP::get(Type::getFloatTy(*TheContext), 0.0));
        case '~': {
            Value* intVal = Builder->CreateFPToSI(L, Type::getFloatTy(*TheContext));
            Value* notVal = Builder->CreateNot(intVal);
            return Builder->CreateSIToFP(notVal, Type::getFloatTy(*TheContext));
        }
        case tok_plus_plus: {
            Value* incVal = Builder->CreateFAdd(L, ConstantFP::get(Type::getFloatTy(*TheContext), 1.0));
            is_eq = true;
            L = exp->Codegen();
            is_eq = false;
            return Builder->CreateStore(incVal, L);
        }
        case tok_sub_sub: {
            Value* decVal = Builder->CreateFSub(L, ConstantFP::get(Type::getFloatTy(*TheContext), 1.0));
            is_eq = true;
            L = exp->Codegen();
            is_eq = false;
            return Builder->CreateStore(decVal, L);

        }
        }
    }
    if (L->getType() == Type::getInt32Ty(*TheContext)) {

        switch (Op) {
        case '+':
            return Builder->CreateAdd(L, ConstantInt::get(Type::getInt32Ty(*TheContext), 0));
        case '-':
            return Builder->CreateNeg(L);
        case '!':
            return Builder->CreateNot(L);
        case '~': {
            Value* notVal = Builder->CreateNot(L);
            return notVal;
        }
        case tok_plus_plus: {
            Value* incVal = Builder->CreateAdd(L, ConstantInt::get(Type::getInt32Ty(*TheContext), 1));
            is_eq = true;
            L = exp->Codegen();
            is_eq = false;
            return Builder->CreateStore(incVal, L);
        }
        case tok_sub_sub: {
            Value* decVal = Builder->CreateSub(L, ConstantInt::get(Type::getInt32Ty(*TheContext), 1));
            is_eq = true;
            L = exp->Codegen();
            is_eq = false;
            return Builder->CreateStore(decVal, L);
        }

        }
    }
}
Value* defvariableAST::Codegen() {
    // Look this variable up in the function.
    if (Type == "int") {
        llvm::Value* var = Builder->CreateAlloca(llvm::Type::getInt32Ty(*TheContext), nullptr, Id);
        llvm::Value* var2 = Expr[0]->Codegen();
        var2 = Builder->CreateFPToSI(var2, Type::getInt32Ty(*TheContext));
        localtype[Id] = llvm::Type::getInt32Ty(*TheContext);
        NamedValues[Id] = var;
        return Builder->CreateStore(var2, var);
        //return Builder->CreateLoad(Type::getInt32Ty(*TheContext), var);
    }
    else if (Type == "bool") {
        llvm::Value* var = Builder->CreateAlloca(llvm::Type::getInt1Ty(*TheContext), nullptr, Id);
        llvm::Value* var2 = Expr[0]->Codegen();
        Value* zero = ConstantFP::get(*TheContext, APFloat(0.0));
        Value* cmp = Builder->CreateFCmpONE(var2, zero);
        Value* boolVal = Builder->CreateZExtOrTrunc(cmp, Type::getInt1Ty(*TheContext));
        localtype[Id] = llvm::Type::getInt1Ty(*TheContext);
        NamedValues[Id] = boolVal;
        return Builder->CreateStore(boolVal, var);
    }
    else if (Type == "float") {
        llvm::Value* var = Builder->CreateAlloca(llvm::Type::getFloatTy(*TheContext), nullptr, Id);
        llvm::Value* var2 = Expr[0]->Codegen();
        localtype[Id] = llvm::Type::getFloatTy(*TheContext);
        NamedValues[Id] = var;
        return Builder->CreateStore(var2, var);
    }
    else if (Type == "double") {
        llvm::Value* var = Builder->CreateAlloca(llvm::Type::getFloatTy(*TheContext), nullptr, Id);
        Value* var2 = Expr[0]->Codegen();
        localtype[Id] = llvm::Type::getFloatTy(*TheContext);
        NamedValues[Id] = var;
        return Builder->CreateStore(var2, var);
    }
    else if (Type == "vec2") {
        Function* Thefunction = Builder->GetInsertBlock()->getParent();
        llvm::Type* vecType = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 2, false);
        llvm::Value* var = Builder->CreateAlloca(vecType, nullptr, Id);
        Value* e0;
        Value* e1;
        Value* var1 = Expr[0]->Codegen();
        if (Expr.size() == 1) {
            Builder->CreateStore(var1, var);
            localtype[Id] = vecType;
            NamedValues[Id] = var;
            return Builder->CreateLoad(vecType, var);
        }
        e0 = var1;
        Value* var2 = Expr[1]->Codegen();
        e1 = var2;
        //var = Builder->CreateInsertElement(var, var2, 2);
        // 将 element0 的值存储到 v 中
        Builder->CreateStore(e0, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 0)), Type::getFloatTy(*TheContext)->getPointerTo()));
        // 将 element1 的值存储到 v 中
        Builder->CreateStore(e1, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 1)), Type::getFloatTy(*TheContext)->getPointerTo()));
        localtype[Id] = vecType;
        NamedValues[Id] = var;
        return Builder->CreateLoad(vecType, var);

    }
    else if (Type == "vec3") {
        Function* Thefunction = Builder->GetInsertBlock()->getParent();
        llvm::Type* vecType = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 3, false);
        llvm::Value* var = Builder->CreateAlloca(vecType, nullptr, Id);
        Value* e0;
        Value* e1;
        Value* e2;
        Value* var1 = Expr[0]->Codegen();
        if (Expr.size() == 1) {
            Builder->CreateStore(var1, var);
            localtype[Id] = vecType;
            NamedValues[Id] = var;
            return Builder->CreateLoad(vecType, var);
        }
        e0 = var1;
        Value* var2 = Expr[1]->Codegen();
        e1 = var2;
        //var = Builder->CreateInsertElement(var, var2, 2);
        Value* var3 = Expr[2]->Codegen();
        e2 = var3;
        // 将 element0 的值存储到 v 中
        Builder->CreateStore(e0, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 0)), Type::getFloatTy(*TheContext)->getPointerTo()));
        // 将 element1 的值存储到 v 中
        Builder->CreateStore(e1, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 1)), Type::getFloatTy(*TheContext)->getPointerTo()));
        Builder->CreateStore(e2, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 2)), Type::getFloatTy(*TheContext)->getPointerTo()));
        localtype[Id] = vecType;
        NamedValues[Id] = var;
        return Builder->CreateLoad(vecType, var);
    }
    else if (Type == "vec4") {
        Function* Thefunction = Builder->GetInsertBlock()->getParent();
        llvm::Type* vecType = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
        llvm::Value* var = Builder->CreateAlloca(vecType, nullptr, Id);
        Value* e0;
        Value* e1;
        Value* e2;
        Value* e3;
        Value* var1 = Expr[0]->Codegen();
        if (Expr.size() == 1) {
            Builder->CreateStore(var1, var);
            localtype[Id] = vecType;
            NamedValues[Id] = var;
            return Builder->CreateLoad(vecType, var);
        }

        e0 = var1;
        Value* var2 = Expr[1]->Codegen();
        e1 = var2;
        //var = Builder->CreateInsertElement(var, var2, 2);
        Value* var3 = Expr[2]->Codegen();
        e2 = var3;
        Value* var4 = Expr[3]->Codegen();
        e3 = var4;
        //var = Builder->CreateInsertElement(var, var2, 2);
    // 将 element0 的值存储到 v 中
        Builder->CreateStore(e0, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 0)), Type::getFloatTy(*TheContext)->getPointerTo()));
        // 将 element1 的值存储到 v 中
        Builder->CreateStore(e1, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 1)), Type::getFloatTy(*TheContext)->getPointerTo()));
        Builder->CreateStore(e2, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 2)), Type::getFloatTy(*TheContext)->getPointerTo()));
        Builder->CreateStore(e3, Builder->CreateBitCast(Builder->CreateGEP(Type::getFloatTy(*TheContext), var, ConstantInt::get(Type::getInt32Ty(*TheContext), 3)), Type::getFloatTy(*TheContext)->getPointerTo()));
        localtype[Id] = vecType;
        NamedValues[Id] = var;
        return Builder->CreateLoad(vecType, var);
    }
    else if (Type == "mat2") {

        return nullptr;

    }
    else if (Type == "mat3") {

        return nullptr;
    }
    else if (Type == "mat4") {
        return nullptr;
    }
    else {
        return nullptr;

    }

}
Value* sentenceAST::Codegen() {
    return Cond->Codegen();
}


Value* whileAST::Codegen() {
    return nullptr;
}

Value* doAST::Codegen() {
    return nullptr;
}
Value* bindingAST::Codegen() {
    return nullptr;
}
Value* locationAST::Codegen() {
    return nullptr;
}
Value* verAST::Codegen() {
    return nullptr;
}
Value* layoutAST::Codegen() {
    return nullptr;
}
Value* tripleExprAST::Codegen() {
    return nullptr;
}


Value* forAST::Codegen() {
    Function* Thefunction = Builder->GetInsertBlock()->getParent();
    //give the name of Function args and save the args as innerargs
    //this function's basic block
    BasicBlock* FBB = BasicBlock::Create(*TheContext, "myentry", Thefunction);
    //create the loop BasicBlock
    BasicBlock* forConditionBlock = BasicBlock::Create(*TheContext, "for.condition");
    BasicBlock* LoopBB = BasicBlock::Create(*TheContext, "loop");
    //exit the block
    BasicBlock* AfterBB = BasicBlock::Create(*TheContext, "afterloop");
    Builder->CreateBr(FBB);
    Builder->SetInsertPoint(FBB);
    Value* ident = ConstantInt::get(Type::getInt32Ty(*TheContext), 0);
    Value* ret = Ini->Codegen();
    Builder->CreateBr(forConditionBlock);
    FBB = Builder->GetInsertBlock();
    Thefunction->insert(Thefunction->end(), forConditionBlock);
    Builder->SetInsertPoint(forConditionBlock);
    Value* Condv = Cond->Codegen();
    if (!Condv) {
        return nullptr;
    }
    //Condv = Builder->CreateFCmpONE(
       // Condv, ConstantFP::get(*TheContext, APFloat(0.0)), "forcond");
    //Condv = Builder->CreateICmpEQ(
        //Condv, ConstantInt::get(Type::getInt32Ty(*TheContext), 0), "forcond");
    Builder->CreateCondBr(Condv, LoopBB, AfterBB);
    forConditionBlock = Builder->GetInsertBlock();
    Builder->SetInsertPoint(LoopBB);
    Thefunction->insert(Thefunction->end(), LoopBB);
    Builder->SetInsertPoint(LoopBB);
    Value* step = Ss->Codegen();
    for (int ii = 0; ii < Body.size(); ii++) {
        Body[ii]->Codegen();
    }
    //add goto LoopBB  in BB
    Builder->CreateBr(forConditionBlock);
    LoopBB = Builder->GetInsertBlock();
    Thefunction->insert(Thefunction->end(), AfterBB);
    Builder->SetInsertPoint(AfterBB);
    //start with LoopBB
    //PHINode* PN = Builder->CreatePHI(Type::getInt32Ty(*TheContext), 1, "fortmp");
    //PN->addIncoming(ident, BB);
    return nullptr;
}


Value* IfAST::Codegen() {
    Value* Condv = Cond->Codegen();
    if (!Condv) return nullptr;
    //Condv = Builder->CreateFCmpONE(Condv,ConstantFP::get(*TheContext,APFloat(0.0)),"ifcond");
    Function* Thefunction = Builder->GetInsertBlock()->getParent();
    BasicBlock* ThenBB = BasicBlock::Create(*TheContext, "Then", Thefunction);
    BasicBlock* ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock* MergeBB = BasicBlock::Create(*TheContext, "ifcont");
    Builder->CreateCondBr(Condv, ThenBB, ElseBB);
    Builder->SetInsertPoint(ThenBB);
    Value* ident1 = ConstantInt::get(Type::getInt32Ty(*TheContext), 0);
    Value* ThenV;
    if (Then.size() == 0) {

    }
    else {
        for (int ii = 0; ii < Then.size(); ii++) {
            Then[ii]->Codegen();
        }

    }
    Builder->CreateBr(MergeBB);
    ThenBB = Builder->GetInsertBlock();
    Thefunction->insert(Thefunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);
    Value* ident2 = ConstantInt::get(Type::getInt32Ty(*TheContext), 0);
    Value* ElseV;
    if (Else.size() == 0) {

    }
    else {
        for (int ii = 0; ii < Else.size(); ii++) {
            Else[ii]->Codegen();
        }
    }
    Builder->CreateBr(MergeBB);
    ElseBB = Builder->GetInsertBlock();
    Thefunction->insert(Thefunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    //PHINode* PN = Builder->CreatePHI(Type::getInt32Ty(*TheContext), 2, "iftmp");
   // PN->addIncoming(ident1, ThenBB);
    //PN->addIncoming(ident2, ElseBB);
    return nullptr;
}
Value* BinaryExprAST::Codegen() {
    llvm::Type* vec2 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 2, false);
    llvm::Type* vec3 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 3, false);
    llvm::Type* vec4 = llvm::VectorType::get(llvm::Type::getFloatTy(*TheContext), 4, false);
    Value* L;
    Value* R;
    if (Op == '=') {
        R = RHS->Codegen();
        is_eq = true;
        L = LHS->Codegen();
        is_eq = false;
        return Builder->CreateStore(R, L, "eqtmp");
    }
    else {
        L = LHS->Codegen();
        R = RHS->Codegen();
    }
    Value* tmp;
    Value* result;
    if (!L || !R)
        return nullptr;
    if ((L->getType() == R->getType()) && ((L->getType() == Type::getFloatTy(*TheContext)) || (L->getType() == vec2) || (L->getType() == vec3) || (L->getType() == vec4))) {
        switch (Op) {
        case '+':
            return Builder->CreateFAdd(L, R, "addtmp");
        case '-':
            return Builder->CreateFSub(L, R, "subtmp");
        case '*':
            return Builder->CreateFMul(L, R, "multmp");
        case '/':
            return Builder->CreateFDiv(L, R, "divtmp");
        case '%':
            return Builder->CreateFRem(L, R, "modtmp");
        case '&':
            return Builder->CreateAnd(L, R, "andtmp");
        case '=':
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(R, L, "eqtmp");
        case '|':
            return Builder->CreateOr(L, R, "ortmp");
        case '^':
            return Builder->CreateXor(L, R, "xortmp");
        case tok_or:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateOr(L, R, "ortmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_and:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateAnd(L, R, "andtmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_eq:
            L = Builder->CreateICmpEQ(L, R, "eqtmp");
            return Builder->CreateIntCast(L, L->getType(), true, "boolcast");
        case '<':
            L = Builder->CreateFCmpOLT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return L;
        case '>':
            L = Builder->CreateFCmpOGT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return L;
        case tok_left_left:
            return Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_right_right:
            return Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_plus_eq:
            tmp = Builder->CreateFAdd(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_sub_eq:
            tmp = Builder->CreateFSub(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mul_eq:
            tmp = Builder->CreateFMul(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_div_eq:
            tmp = Builder->CreateFDiv(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mod_eq:
            tmp = Builder->CreateFRem(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_left_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_right_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        default:
            return LogErrorV("invalid binary operator");
        }
    }
    if ((L->getType() == R->getType()) && (L->getType() == Type::getInt32Ty(*TheContext))) {
        switch (Op) {
        case '+':
            return Builder->CreateAdd(L, R, "addtmp");
        case '-':
            return Builder->CreateSub(L, R, "subtmp");
        case '*':
            return Builder->CreateMul(L, R, "multmp");
        case '/':
            return Builder->CreateSDiv(L, R, "divtmp");
        case '%':
            return Builder->CreateSRem(L, R, "modtmp");
        case '&':
            return Builder->CreateAnd(L, R, "andtmp");
        case '=':
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(R, L, "eqtmp");
        case '|':
            return Builder->CreateOr(L, R, "ortmp");
        case '^':
            return Builder->CreateXor(L, R, "xortmp");
        case tok_or:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateOr(L, R, "ortmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_and:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateAnd(L, R, "andtmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_eq:
            L = Builder->CreateICmpEQ(L, R, "eqtmp");
            return Builder->CreateIntCast(L, L->getType(), true, "boolcast");
        case '<':
            L = Builder->CreateICmpSLT(L, R, "cmptmp");
            return L;
            // Convert bool 0/1 to double 0.0 or 1.0
        case '>':
            L = Builder->CreateICmpSGT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return L;
        case tok_left_left:
            return Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_right_right:
            return Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_plus_eq:
            tmp = Builder->CreateAdd(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_sub_eq:
            tmp = Builder->CreateSub(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mul_eq:
            tmp = Builder->CreateMul(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_div_eq:
            tmp = Builder->CreateSDiv(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mod_eq:
            tmp = Builder->CreateSRem(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_left_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getInt32Ty(*TheContext), Type::getInt32Ty(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_right_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getInt32Ty(*TheContext), Type::getInt32Ty(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        default:
            return LogErrorV("invalid binary operator");
        }
    }
    if ((R->getType() == Type::getFloatTy(*TheContext)) && (L->getType() == Type::getInt32Ty(*TheContext))) {
        R = Builder->CreateFPToSI(R, Type::getInt32Ty(*TheContext));
        switch (Op) {
        case '+':
            return Builder->CreateAdd(L, R, "addtmp");
        case '-':
            return Builder->CreateSub(L, R, "subtmp");
        case '*':
            return Builder->CreateMul(L, R, "multmp");
        case '/':
            return Builder->CreateSDiv(L, R, "divtmp");
        case '%':
            return Builder->CreateSRem(L, R, "modtmp");
        case '&':
            return Builder->CreateAnd(L, R, "andtmp");
        case '=':
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(R, L, "eqtmp");
        case '|':
            return Builder->CreateOr(L, R, "ortmp");
        case '^':
            return Builder->CreateXor(L, R, "xortmp");
        case tok_or:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateOr(L, R, "ortmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_and:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateAnd(L, R, "andtmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_eq:
            L = Builder->CreateICmpEQ(L, R, "eqtmp");
            return Builder->CreateIntCast(L, L->getType(), true, "boolcast");
        case '<':
            L = Builder->CreateICmpSLT(L, R, "cmptmp");
            return L;
            // Convert bool 0/1 to double 0.0 or 1.0
        case '>':
            L = Builder->CreateICmpSGT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return L;
        case tok_left_left:
            return Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_right_right:
            return Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_plus_eq:
            tmp = Builder->CreateAdd(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_sub_eq:
            tmp = Builder->CreateSub(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mul_eq:
            tmp = Builder->CreateMul(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_div_eq:
            tmp = Builder->CreateSDiv(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mod_eq:
            tmp = Builder->CreateSRem(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_left_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getInt32Ty(*TheContext), Type::getInt32Ty(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_right_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getInt32Ty(*TheContext), Type::getInt32Ty(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        default:
            return LogErrorV("invalid binary operator");
        }
    }
    if ((R->getType() == Type::getFloatTy(*TheContext)) && (L->getType()->isVectorTy())) {
        ConstantFP* Val = dyn_cast<ConstantFP>(R);
        if (L->getType() == vec2) {
            Value* v2 = Builder->CreateAlloca(vec2, nullptr, "tempvec2");
            R = ConstantVector::get({ Val, Val });
            Builder->CreateStore(R, v2);
            R = Builder->CreateLoad(vec2, v2);
        }
        if (L->getType() == vec3) {
            Value* v3 = Builder->CreateAlloca(vec3, nullptr, "tempvec3");
            R = ConstantVector::get({ Val, Val, Val });
            Builder->CreateStore(R, v3);
            R = Builder->CreateLoad(vec3, v3);
        }
        if (L->getType() == vec4) {
            Value* v4 = Builder->CreateAlloca(vec4, nullptr, "tempvec4");
            R = ConstantVector::get({ Val, Val,Val,Val });
            Builder->CreateStore(R, v4);
            R = Builder->CreateLoad(vec4, v4);
        }
        switch (Op) {
        case '+':
            return Builder->CreateFAdd(L, R, "addtmp");
        case '-':
            return Builder->CreateFSub(L, R, "subtmp");
        case '*':
            return Builder->CreateFMul(L, R, "multmp");
        case '/':
            return Builder->CreateFDiv(L, R, "divtmp");
        case '%':
            return Builder->CreateFRem(L, R, "modtmp");
        case '&':
            return Builder->CreateAnd(L, R, "andtmp");
        case '=':
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(R, L, "eqtmp");
        case '|':
            return Builder->CreateOr(L, R, "ortmp");
        case '^':
            return Builder->CreateXor(L, R, "xortmp");
        case tok_or:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateOr(L, R, "ortmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_and:
            L = Builder->CreateICmpNE(L, ConstantInt::get(L->getType(), 0), "leftcond");
            R = Builder->CreateICmpNE(R, ConstantInt::get(R->getType(), 0), "rightcond");
            result = Builder->CreateAnd(L, R, "andtmp");
            return Builder->CreateIntCast(result, L->getType(), true, "boolcast");
        case tok_eq:
            L = Builder->CreateICmpEQ(L, R, "eqtmp");
            return Builder->CreateIntCast(L, L->getType(), true, "boolcast");
        case '<':
            L = Builder->CreateFCmpULT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return L;
        case '>':
            L = Builder->CreateFCmpUGT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return L;
        case tok_left_left:
            return Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_right_right:
            return Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getFloatTy(*TheContext), Type::getFloatTy(*TheContext) }, { L, R });
        case tok_plus_eq:
            tmp = Builder->CreateFAdd(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_sub_eq:
            tmp = Builder->CreateFSub(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mul_eq:
            tmp = Builder->CreateFMul(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_div_eq:
            tmp = Builder->CreateFDiv(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_mod_eq:
            tmp = Builder->CreateFRem(L, R, "tmp");
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_left_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshl, { Type::getInt32Ty(*TheContext), Type::getInt32Ty(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        case tok_right_eq:
            tmp = Builder->CreateIntrinsic(Intrinsic::fshr, { Type::getInt32Ty(*TheContext), Type::getInt32Ty(*TheContext) }, { L, R });
            is_eq = true;
            L = LHS->Codegen();
            is_eq = false;
            return Builder->CreateStore(tmp, L);
        default:
            return LogErrorV("invalid binary operator");
        }
    }
}

Value* CallExprAST::Codegen() {
    // Look up the name in the global module table.
    Function* CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF)
        return LogErrorV("Unknown function referenced");

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value*> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->Codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function* PrototypeAST::Codegen() {
    // Make the function type:  double(double,double) etc.
    std::vector<Type*> Doubles(Args.size(), Type::getFloatTy(*TheContext));
    FunctionType* FT =
        FunctionType::get(Type::getVoidTy(*TheContext), Doubles, false);

    Function* F =
        Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto& Arg : F->args())
        Arg.setName(Args[Idx++]);

    return F;
}

Function* FunctionAST::Codegen() {
    // First, check for an existing function from a previous 'extern' declaration.
    Function* TheFunction = TheModule->getFunction(Proto->getName());

    if (!TheFunction)
        TheFunction = Proto->Codegen();

    if (!TheFunction)
        return nullptr;

    // Create a new basic block to start insertion into.
    BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    for (auto& Arg : TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;
    for (int ii = 0; ii < Body.size(); ii++) {
        Body[ii]->Codegen();
    }
    /*
    if (Value* RetVal = Body[1]->Codegen()) {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        //verifyFunction(*TheFunction);

        return TheFunction;
    }
    */
    Builder->CreateRetVoid();
    return TheFunction;
    // Error reading body, remove function.
    TheFunction->eraseFromParent();
    return nullptr;
}












//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//
static void InitializeModule() {
    // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("", *TheContext);
    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (auto* FnIR = FnAST->Codegen()) {
            //  fprintf(stderr, "Read function definition:");
             // FnIR->print(errs());
            //  fprintf(stderr, "\n");
        }
    }
    else {
        // Skip token for error recovery.
        getNextToken();
    }
}


static void Handleid() {
    // fprintf(stderr, "handle an id\n");
    getNextToken();
}

static void Handleout() {
    if (Parsevariabledef(false)) {
        // fprintf(stderr, "Parsed a layout\n"
    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}


static void Handlever() {
    if (Parseversion()) {
        //   fprintf(stderr, "handle a layout\n");

    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}
static void Handlelayout() {
    if (auto lyast = Parselayout()) {
        if (auto* lyIR = lyast->Codegen()) {
            //fprintf(stderr, "handle a layout");
           // lyIR->print(errs());
           // fprintf(stderr, "\n");

        }
    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleIf() {
    if (ParseIf()) {
        //   fprintf(stderr, "Parsed an if\n");
    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}

static void Handlewle() {
    if (Parsewhile()) {
        //   fprintf(stderr, "Parsed an if\n");
    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}
static void Handledo() {
    if (Parsedo()) {
        //   fprintf(stderr, "Parsed an if\n");
    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}
static void Handlefr() {
    if (Parsefor()) {
        //   fprintf(stderr, "Parsed an if\n");
    }
    else {
        is_true = false;
        // Skip token for error recovery.
        getNextToken();
    }
}
/*
static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (ParseTopLevelExpr()) {
        //   fprintf(stderr, "Parsed a top-level expr\n");
    }
    else {
        // Skip token for error recovery.
        getNextToken();
    }
}
*/
/// top ::= definition | external | expression | ';'
static void MainLoop() {
    while (true) {
        switch (CurTok) {
        case tok_eof:
            if (is_true) { //fprintf(stdout, "Accept");
            }
            else {// fprintf(stdout, "Reject"); 
            }
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case tok_version:
            Handlever();
            break;
        case tok_void:
            HandleDefinition();
            break;
        case tok_int:
            HandleDefinition();
            break;
        case tok_layout:
            Handlelayout();
            break;
        case tok_in:
            Parsevariabledef(false);
            break;
        case tok_out:
            Parsevariabledef(false);
            break;
        case tok_uniform:
            Parsevariabledef(false);
            break;
        default:
            //  fprintf(stdout, "Reject");
            return;
        }
    }
    if (is_true) { //fprintf(stdout, "Accept");
    }
    else { //fprintf(stdout, "Reject"); 
    }
    return;
}

int main(int argc, char** argv) {
    cl::ParseCommandLineOptions(argc, argv);
    ifile_ptr = fopen(InputFilename.c_str(), "r");
    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['='] = 5;
    BinopPrecedence['?'] = 7;
    BinopPrecedence[':'] = 7;
    BinopPrecedence['|'] = 25;
    BinopPrecedence['^'] = 30;
    BinopPrecedence['&'] = 35;
    BinopPrecedence['<'] = 45;
    BinopPrecedence['>'] = 45;
    BinopPrecedence['+'] = 55;
    BinopPrecedence['-'] = 55;
    BinopPrecedence['*'] = 60;
    BinopPrecedence['/'] = 60;
    BinopPrecedence['%'] = 60;
    BinopPrecedence['!'] = 65;
    BinopPrecedence['~'] = 65; // highest.
    // Prime the first token.
    getNextToken();
    InitializeModule();
    if (CurTok != tok_version) {
        fprintf(stdout, "Reject");
        return 0;
    }
    // Run the main "interpreter loop" now.
    MainLoop();
    std::error_code ec;
    llvm::raw_fd_ostream out(OutputFilename, ec);
    TheModule->print(out, nullptr);
    return 0;
}


