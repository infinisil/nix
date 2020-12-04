#pragma once

#include "value.hh"
#include "symbol-table.hh"
#include "error.hh"

#include <map>


namespace nix {


MakeError(EvalError, Error);
MakeError(ParseError, Error);
MakeError(AssertionError, EvalError);
MakeError(ThrownError, AssertionError);
MakeError(Abort, EvalError);
MakeError(TypeError, EvalError);
MakeError(UndefinedVarError, Error);
MakeError(RestrictedPathError, Error);


/* Position objects. */

struct Pos
{
    FileOrigin origin;
    Symbol file;
    unsigned int line, column;
    Pos() : origin(foString), line(0), column(0) { };
    Pos(FileOrigin origin, const Symbol & file, unsigned int line, unsigned int column)
        : origin(origin), file(file), line(line), column(column) { };
    operator bool() const
    {
        return line != 0;
    }
    bool operator < (const Pos & p2) const
    {
        if (!line) return p2.line;
        if (!p2.line) return false;
        int d = ((string) file).compare((string) p2.file);
        if (d < 0) return true;
        if (d > 0) return false;
        if (line < p2.line) return true;
        if (line > p2.line) return false;
        return column < p2.column;
    }
};

extern Pos noPos;

std::ostream & operator << (std::ostream & str, const Pos & pos);


struct Env;
struct Value;
class EvalState;
struct StaticEnv;
struct Attr;


/* An attribute path is a sequence of attribute names. */
struct AttrName
{
    Symbol symbol;
    Expr * expr;
    AttrName(const Symbol & s) : symbol(s) {};
    AttrName(Expr * e) : expr(e) {};
};

typedef std::vector<AttrName> AttrPath;

string showAttrPath(const AttrPath & attrPath);


/* Abstract syntax of Nix expressions. */

struct EvalHandler
{
    /*
     * Takes a Value in either WHNF or lazyBinOp and does the necessary evaluation with it
     */
    virtual void fromValue(EvalState & state, Value & v)
    {
        abort();
    }
};


struct WHNFEvalHandler : EvalHandler
{
    static WHNFEvalHandler * instance;
    void fromValue(EvalState & state, Value & v);
    static WHNFEvalHandler * getInstance();
};

struct AttrEvalHandler : EvalHandler
{
    const Symbol & name;
    Attr * attr = nullptr;
    AttrEvalHandler(const Symbol & name) : name(name) { };
    void fromValue(EvalState & state, Value & v);
};

struct Expr
{
    virtual ~Expr() { };
    virtual void show(std::ostream & str) const;
    virtual void bindVars(const StaticEnv & env);
    /*
     * Calling handler is only necessary if the result is either a tLazyBinOp or a tAttrs
     */
    virtual void evalHandler(EvalState & state, Env & env, Value & v, EvalHandler & handler);
    void eval(EvalState & state, Env & env, Value & v);

    /* Get an attribute of an expression, or nullptr if it doesn't exist, while
     * evaluating the expression into v. This is an alternative to eval that
     * doesn't have to evaluate the expression into a non-thunk value
     */
    Attr * evalAttr(EvalState & state, Env & env, Value & v, const Symbol & name);


    virtual Value * maybeThunk(EvalState & state, Env & env);
    // Return a Value if it can be obtained without doing any allocations
    virtual Value * noAllocationValue(EvalState & state, Env & env);
    virtual void setName(Symbol & name);
};

struct ExprLazyBinOp : Expr
{

    void evalHandler(EvalState & state, Env & env, Value & v, EvalHandler & handler);

    virtual void initLazyBinOp(EvalState & state, Env & env, Value & v);

    // v needs to be a tLazyBinOp for these to be callable
    virtual void evalLazyBinOp(EvalState & state, Env & env, Value & v);
    virtual Attr * evalLazyBinOpAttr(EvalState & state, Env & env, const Symbol & name, Value & v);
};

std::ostream & operator << (std::ostream & str, const Expr & e);

#define COMMON_METHODS \
    void show(std::ostream & str) const; \
    void evalHandler(EvalState & state, Env & env, Value & v, EvalHandler & handler); \
    void bindVars(const StaticEnv & env);

struct ExprInt : Expr
{
    NixInt n;
    Value v;
    ExprInt(NixInt n) : n(n) { mkInt(v, n); };
    COMMON_METHODS
    Value * noAllocationValue(EvalState & state, Env & env);
};

struct ExprFloat : Expr
{
    NixFloat nf;
    Value v;
    ExprFloat(NixFloat nf) : nf(nf) { mkFloat(v, nf); };
    COMMON_METHODS
    Value * noAllocationValue(EvalState & state, Env & env);
};

struct ExprString : Expr
{
    Symbol s;
    Value v;
    ExprString(const Symbol & s) : s(s) { mkString(v, s); };
    COMMON_METHODS
    Value * noAllocationValue(EvalState & state, Env & env);
};

/* Temporary class used during parsing of indented strings. */
struct ExprIndStr : Expr
{
    string s;
    ExprIndStr(const string & s) : s(s) { };
};

struct ExprPath : Expr
{
    string s;
    Value v;
    ExprPath(const string & s) : s(s) { mkPathNoCopy(v, this->s.c_str()); };
    COMMON_METHODS
    Value * noAllocationValue(EvalState & state, Env & env);
};

struct ExprVar : Expr
{
    Pos pos;
    Symbol name;

    /* Whether the variable comes from an environment (e.g. a rec, let
       or function argument) or from a "with". */
    bool fromWith;

    /* In the former case, the value is obtained by going `level'
       levels up from the current environment and getting the
       `displ'th value in that environment.  In the latter case, the
       value is obtained by getting the attribute named `name' from
       the set stored in the environment that is `level' levels up
       from the current one.*/
    unsigned int level;
    unsigned int displ;

    ExprVar(const Symbol & name) : name(name) { };
    ExprVar(const Pos & pos, const Symbol & name) : pos(pos), name(name) { };
    COMMON_METHODS
    Value * noAllocationValue(EvalState & state, Env & env);
};

struct ExprSelect : Expr
{
    Pos pos;
    Expr * e, * def;
    AttrPath attrPath;
    ExprSelect(const Pos & pos, Expr * e, const AttrPath & attrPath, Expr * def) : pos(pos), e(e), def(def), attrPath(attrPath) { };
    ExprSelect(const Pos & pos, Expr * e, const Symbol & name) : pos(pos), e(e), def(0) { attrPath.push_back(AttrName(name)); };
    COMMON_METHODS
};

struct ExprOpHasAttr : Expr
{
    Expr * e;
    AttrPath attrPath;
    ExprOpHasAttr(Expr * e, const AttrPath & attrPath) : e(e), attrPath(attrPath) { };
    COMMON_METHODS
};

struct ExprAttrs : Expr
{
    bool recursive;
    struct AttrDef {
        bool inherited;
        Expr * e;
        Pos pos;
        unsigned int displ; // displacement
        AttrDef(Expr * e, const Pos & pos, bool inherited=false)
            : inherited(inherited), e(e), pos(pos) { };
        AttrDef() { };
    };
    typedef std::map<Symbol, AttrDef> AttrDefs;
    AttrDefs attrs;
    struct DynamicAttrDef {
        Expr * nameExpr, * valueExpr;
        Pos pos;
        DynamicAttrDef(Expr * nameExpr, Expr * valueExpr, const Pos & pos)
            : nameExpr(nameExpr), valueExpr(valueExpr), pos(pos) { };
    };
    typedef std::vector<DynamicAttrDef> DynamicAttrDefs;
    DynamicAttrDefs dynamicAttrs;
    ExprAttrs() : recursive(false) { };
    COMMON_METHODS
};

struct ExprList : Expr
{
    std::vector<Expr *> elems;
    ExprList() { };
    COMMON_METHODS
};

struct Formal
{
    Pos pos;
    Symbol name;
    Expr * def;
    Formal(const Pos & pos, const Symbol & name, Expr * def) : pos(pos), name(name), def(def) { };
};

struct Formals
{
    typedef std::list<Formal> Formals_;
    Formals_ formals;
    std::set<Symbol> argNames; // used during parsing
    bool ellipsis;
};

struct ExprLambda : Expr
{
    Pos pos;
    Symbol name;
    Symbol arg;
    bool matchAttrs;
    Formals * formals;
    Expr * body;
    ExprLambda(const Pos & pos, const Symbol & arg, bool matchAttrs, Formals * formals, Expr * body)
        : pos(pos), arg(arg), matchAttrs(matchAttrs), formals(formals), body(body)
    {
        if (!arg.empty() && formals && formals->argNames.find(arg) != formals->argNames.end())
            throw ParseError({
                .hint = hintfmt("duplicate formal function argument '%1%'", arg),
                .errPos = pos
            });
    };
    void setName(Symbol & name);
    string showNamePos() const;
    COMMON_METHODS
};

struct ExprLet : Expr
{
    ExprAttrs * attrs;
    Expr * body;
    ExprLet(ExprAttrs * attrs, Expr * body) : attrs(attrs), body(body) { };
    COMMON_METHODS
};

struct ExprWith : Expr
{
    Pos pos;
    Expr * attrs, * body;
    size_t prevWith;
    ExprWith(const Pos & pos, Expr * attrs, Expr * body) : pos(pos), attrs(attrs), body(body) { };
    COMMON_METHODS
};

struct ExprIf : Expr
{
    Pos pos;
    Expr * cond, * then, * else_;
    ExprIf(const Pos & pos, Expr * cond, Expr * then, Expr * else_) : pos(pos), cond(cond), then(then), else_(else_) { };
    COMMON_METHODS
};

struct ExprAssert : Expr
{
    Pos pos;
    Expr * cond, * body;
    ExprAssert(const Pos & pos, Expr * cond, Expr * body) : pos(pos), cond(cond), body(body) { };
    COMMON_METHODS
};

struct ExprOpNot : Expr
{
    Expr * e;
    ExprOpNot(Expr * e) : e(e) { };
    COMMON_METHODS
};

#define MakeBinOp(name, s) \
    struct name : Expr \
    { \
        Pos pos; \
        Expr * e1, * e2; \
        name(Expr * e1, Expr * e2) : e1(e1), e2(e2) { }; \
        name(const Pos & pos, Expr * e1, Expr * e2) : pos(pos), e1(e1), e2(e2) { }; \
        void show(std::ostream & str) const \
        { \
            str << "(" << *e1 << " " s " " << *e2 << ")";   \
        } \
        void bindVars(const StaticEnv & env) \
        { \
            e1->bindVars(env); e2->bindVars(env); \
        } \
        void evalHandler(EvalState & state, Env & env, Value & v, EvalHandler & handler); \
    };

MakeBinOp(ExprApp, "")
MakeBinOp(ExprOpEq, "==")
MakeBinOp(ExprOpNEq, "!=")
MakeBinOp(ExprOpAnd, "&&")
MakeBinOp(ExprOpOr, "||")
MakeBinOp(ExprOpImpl, "->")
//MakeBinOp(ExprOpUpdate, "//")
MakeBinOp(ExprOpConcatLists, "++")


// TODO: Make the #define above support this
struct ExprOpUpdate : ExprLazyBinOp
{
    Pos pos;
    Expr * e1, * e2;
    ExprOpUpdate(Expr * e1, Expr * e2) : e1(e1), e2(e2) { };
    ExprOpUpdate(const Pos & pos, Expr * e1, Expr * e2) : pos(pos), e1(e1), e2(e2) { };
    void show(std::ostream & str) const
    {
        str << "(" << *e1 << " // " << *e2 << ")";
    }
    void bindVars(const StaticEnv & env)
    {
        e1->bindVars(env); e2->bindVars(env);
    }
    void initLazyBinOp(EvalState & state, Env & env, Value & v);

    void evalLazyBinOp(EvalState & state, Env & env, Value & v);
    Attr * evalLazyBinOpAttr(EvalState & state, Env & env, const Symbol & name, Value & v);

    void updateAttrs(EvalState & state, const Value & v1, const Value & v2, Value & v);
};

struct ExprConcatStrings : Expr
{
    Pos pos;
    bool forceString;
    vector<Expr *> * es;
    ExprConcatStrings(const Pos & pos, bool forceString, vector<Expr *> * es)
        : pos(pos), forceString(forceString), es(es) { };
    COMMON_METHODS
};

struct ExprPos : Expr
{
    Pos pos;
    ExprPos(const Pos & pos) : pos(pos) { };
    COMMON_METHODS
};


/* Static environments are used to map variable names onto (level,
   displacement) pairs used to obtain the value of the variable at
   runtime. */
struct StaticEnv
{
    bool isWith;
    const StaticEnv * up;
    typedef std::map<Symbol, unsigned int> Vars;
    Vars vars;
    StaticEnv(bool isWith, const StaticEnv * up) : isWith(isWith), up(up) { };
};


}
