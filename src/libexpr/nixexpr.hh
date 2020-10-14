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

// Have an abstract class for a thing that can be evaluated, providing methods evalNF and evalAttr
// Can be implemented by both Expr and Value
//

struct Expr
{
    virtual ~Expr() { };
    virtual void show(std::ostream & str) const;
    virtual void bindVars(const StaticEnv & env);

    // Evaluates the expression into either WHNF or a partialBinOp
    // This function is only called at most once, and that with an empty v
    virtual void evalMinimal(EvalState & state, Env & env, Value & v);

    virtual void eval(EvalState & state, Env & env, Value & v);

    // We only need this ^ and an evalValueAttr, which can call an ExprLazyBinOp's functions

    // Maybe later also an evalWHNF for better performance

    // Evaluate an attribute of an expression into either WHNF or a partialBinOp
    //
    // Evaluate an attribute of a value minimally, similar to evalMinimal
    // Here, v is initialized to an evalMinimal result and vAttr is empty and needs to be filled (if result is true)
    //
    // Evaluate an attribute of an expression into either WHNF or a partialBinOp
    //virtual bool evalMinimalAttr(EvalState & state, Env & env, Value & v, const Symbol & name, Value & vAttr);
    // By default, probably calls evalMinimal, checks for it to be WHNF

    // v is empty, evalutes to WHNF
    //virtual void eval(EvalState & state, Env & env, Value & v);

    // This functions v will be uninitialized, and this function should initialize it to weak head normal form
    //virtual void evalWHNF(EvalState & state, Env & env, Value & v);
    // ^ Abort for Expr itself

    // Evaluates the two values of a tPartialBinop returned from a previous partial evaluation
    // Both left and right will be in WHNF already, v will be empty and needs to be initialized
    //virtual void evalBinOpWHNF(EvalState & state, Env & env, const Value & left, const Value & right, Value & v);
    // ^ Abort for Expr itself, but will also only be called if an evalAttr returned a tPartialBinop

    // This functions v will be uninitialized, and it should turn it into either normal form or a partial thunk
    //virtual bool evalAttr(EvalState & state, Env & env, const Symbol & name, Value & v, Value & vAttr);
    // ^ By default an evalWHNF followed by an evalValueAttr

    // Called if a previous evalAttr returned a tPartialBinOp, with the values of that binOp. This function can modify left and right, and if it turns them both to normal form, evalBinOpWHNF is called on them one last time
    // Should return the attribute name `name' of the expression into `vAttr'
    //virtual bool evalBinOpAttr(EvalState & state, Env & env, Value & left, Value & right, const Symbol & name, Value & vAttr);

    //virtual bool createPartialBinOp
    //
    // Binary operations could do with:
    // - mkPartialBinOp: Returns thunks for left and right values, which then get put into a tPartialBinOp
    // - evalBinOpWHNF
    //


    /* LazyBinOp:
     * - evalSubexprsIntoValues: Turns the expression into left and right Values using maybeThunk
     * - evalBinOpAttr: Takes the left and right Values, and returns an attribute name from them
     * - evalBinOpWHNF: Takes the left and right Values in WHNF and combines them into a WHNF result
     *
     * evalWHNF can be implemented with:
     * - evalSubexprsIntoValues
     * - evaluating left and right into WHNF
     * - evalBinOpWHNF
     */

    /* Getting an attribute of a non-LazyBinOp?
     */

    // Evaluate into WHNF, can only be called once, v could be empty or tPartialBinOp
    //virtual void evalWHNF(EvalState & state, Env & env, Value & v);
    // By default, checks if v is empty or tPartialBinOp:
    // - If empty, call standard eval
    // - If tPartialBinOp, evaluate left and right into WHNF, then call evalBinOpWHNF with them

    // Get an attribute, could be called multiple times, v could be empty or tPartialBinOp
    //virtual bool evalAttr(EvalState & state, Env & env, Value & v, const Symbol & name, Value & vAttr);
    // By default checks if v is empty or tPartialBinOp:
    // - If empty, eval, then evalValueAttr
    // - If tPartialBinOp, evalBinOpAttr


    // For lists: evalIndex, evalIndexPartialThunk
    // For strings: evalSubstring, evalSubstringPartialThunk

    virtual Value * maybeThunk(EvalState & state, Env & env);
    virtual void setName(Symbol & name);
};

struct ExprLazyBinOp : Expr
{
    virtual void evalMinimal(EvalState & state, Env & env, Value & v)
    {
        v.type = tPartialBinOp;
        v.partialBinOp.expr = this;
        v.partialBinOp.env = env;
    }

    // Fills in empty v's with left and right minimal evals
    //virtual void evalMinimalLeft(EvalState & state, Env & env, Value & v);
    //virtual void evalMinimalRight(EvalState & state, Env & env, Value & v);
    //virtual void toPartialBinOp(EvalState & state, Env & env, Value & left, Value & right);

    // This functions v will be uninitialized, and this function should initialize it to weak head normal form
    //virtual void evalWHNF(EvalState & state, Env & env, Value & v);
    // ^ Abort for Expr itself

    // Evaluates the two values of a tPartialBinop returned from a previous partial evaluation
    // Both left and right will be in WHNF already, v will be empty and needs to be initialized
    //virtual void evalPartialBinOpWHNF(EvalState & state, Env & env, const Value & left, const Value & right, Value & v);
    // ^ Abort for Expr itself, but will also only be called if an evalAttr returned a tPartialBinop

    // This functions v will be uninitialized, and it should turn it into either normal form or a partial thunk
    //virtual bool evalAttr(EvalState & state, Env & env, const Symbol & name, Value & v, Value & vAttr);
    // ^ By default an evalWHNF followed by an evalValueAttr

    // Called if a previous evalAttr returned a tPartialBinOp, with the values of that binOp. This function can modify left and right, and if it turns them both to normal form, evalBinOpWHNF is called on them one last time
    // Should return the attribute name `name' of the expression into `vAttr'

    // Evaluates an attribute of this expression. left and right have been filled in with evalMinimals of the respective values
    virtual bool evalMinimalAttr(EvalState & state, Env & env, Value * left, Value * right, const Symbol & name, Value & vAttr);
    // Evaluates it to WHNF
    virtual void evalBinOp(EvalState & state, Env & env, Value * left, Value * right, Value & v);
};

std::ostream & operator << (std::ostream & str, const Expr & e);

#define COMMON_METHODS \
    void show(std::ostream & str) const; \
    void eval(EvalState & state, Env & env, Value & v); \
    void bindVars(const StaticEnv & env);

struct ExprInt : Expr
{
    NixInt n;
    Value v;
    ExprInt(NixInt n) : n(n) { mkInt(v, n); };
    COMMON_METHODS
    Value * maybeThunk(EvalState & state, Env & env);
};

struct ExprFloat : Expr
{
    NixFloat nf;
    Value v;
    ExprFloat(NixFloat nf) : nf(nf) { mkFloat(v, nf); };
    COMMON_METHODS
    Value * maybeThunk(EvalState & state, Env & env);
};

struct ExprString : Expr
{
    Symbol s;
    Value v;
    ExprString(const Symbol & s) : s(s) { mkString(v, s); };
    COMMON_METHODS
    Value * maybeThunk(EvalState & state, Env & env);
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
    Value * maybeThunk(EvalState & state, Env & env);
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
    Value * maybeThunk(EvalState & state, Env & env);

    bool evalAttr(EvalState & state, Env & env, const Symbol & name, Value & v, Value & vAttr);
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
    bool evalAttr(EvalState & state, Env & env, const Symbol & name, Value & v, Value & vAttr);
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
        void eval(EvalState & state, Env & env, Value & v); \
    };

//MakeBinOp(ExprApp, "")
MakeBinOp(ExprOpEq, "==")
MakeBinOp(ExprOpNEq, "!=")
MakeBinOp(ExprOpAnd, "&&")
MakeBinOp(ExprOpOr, "||")
MakeBinOp(ExprOpImpl, "->")
//MakeBinOp(ExprOpUpdate, "//")
MakeBinOp(ExprOpConcatLists, "++")

struct ExprApp : Expr
{
    Pos pos;
    Expr * e1, * e2;
    ExprApp(Expr * e1, Expr * e2) : e1(e1), e2(e2) { };
    ExprApp(const Pos & pos, Expr * e1, Expr * e2) : pos(pos), e1(e1), e2(e2) { };
    void show(std::ostream & str) const
    {
        str << "(" << *e1 << " " "" " " << *e2 << ")";
    }
    void bindVars(const StaticEnv & env)
    {
        e1->bindVars(env); e2->bindVars(env);
    }
    void eval(EvalState & state, Env & env, Value & v);
    bool evalAttr(EvalState & state, Env & env, const Symbol & name, Value & v, Value & vAttr);
};

struct ExprOpUpdate : ExprLazyBinOp
{
    Pos pos;
    Expr * e1, * e2;
    ExprOpUpdate(Expr * e1, Expr * e2) : e1(e1), e2(e2) { };
    ExprOpUpdate(const Pos & pos, Expr * e1, Expr * e2) : pos(pos), e1(e1), e2(e2) { };
    void show(std::ostream & str) const
    {
        str << "(" << *e1 << " " "//" " " << *e2 << ")";
    }
    void bindVars(const StaticEnv & env)
    {
        e1->bindVars(env); e2->bindVars(env);
    }
    bool evalMinimalAttr(EvalState & state, Env & env, Value & left, Value & right, const Symbol & name, Value & vAttr);
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
