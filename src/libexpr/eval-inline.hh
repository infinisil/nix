#pragma once

#include "eval.hh"
#include "util.hh"

#define LocalNoInline(f) static f __attribute__((noinline)); f
#define LocalNoInlineNoReturn(f) static f __attribute__((noinline, noreturn)); f

namespace nix {

LocalNoInlineNoReturn(void throwEvalError(const Pos & pos, const char * s))
{
    throw EvalError({
        .hint = hintfmt(s),
        .errPos = pos
    });
}

LocalNoInlineNoReturn(void throwTypeError(const char * s, const Value & v))
{
    throw TypeError(s, showType(v));
}


LocalNoInlineNoReturn(void throwTypeError(const Pos & pos, const char * s, const Value & v))
{
    throw TypeError({
        .hint = hintfmt(s, showType(v)),
        .errPos = pos
    });
}


// Forces the value into WHNF
void EvalState::forceValue(Value & v, const Pos & pos)
{
    if (v.type == tThunk) {
        Env * env = v.thunk.env;
        Expr * expr = v.thunk.expr;
        try {
            v.type = tBlackhole;
            //checkInterrupt();
            expr->eval(*this, *env, v);
        } catch (...) {
            v.type = tThunk;
            v.thunk.env = env;
            v.thunk.expr = expr;
            throw;
        }
    }
    else if (v.type == tPartialBinOp) {
        // TODO: Maybe handle inf rec here
        v.partialBinOp.expr->evalBinOp(*this, *v.partialBinOp.env, v.partialBinOp.left, v.partialBinOp.right, v);
    }
    else if (v.type == tApp)
        callFunction(*v.app.left, *v.app.right, v, noPos);
    else if (v.type == tBlackhole)
        throwEvalError(pos, "infinite recursion encountered");
}

void EvalState::evalValueMinimal(Value & v, const Pos & pos)
{
    // TODO: Maybe handle inf rec here
    if (v.type == tThunk) {
        v.thunk.expr->evalMinimal(*this, *v.thunk.env, v);
    } else if (v.type == tApp) {
        callFunctionMinimal(*v.app.left, *v.app.right, v, pos);
    }
}

bool EvalState::evalValueAttr(Value & v, const Symbol & name, Value & vAttr, const Pos & pos) {
    evalValueMinimal(v, pos);
    if (v.type == tPartialBinOp) {

        ExprLazyBinOp * expr = v.partialBinOp.expr;
        Env * env = v.partialBinOp.env;
        Value * left = v.partialBinOp.left;
        Value * right = v.partialBinOp.right;

        bool result = expr->evalMinimalAttr(*this, *env, left, right, name, vAttr);
        if (left && right && left->isWHNF() && right->isWHNF()) {
            expr->evalBinOp(*this, *env, left, right, v);
        }
        return result;
    } else if (v.type == tAttrs) {

        Bindings::iterator j;
        if ((j = v.attrs->find(name)) == v.attrs->end())
            return false;

        vAttr = *j->value;
        return true;

    } else {
        // TODO: Return false for most things, probably
        printError("EvalState::evalValueAttr called with something we can't handle");
        abort();
    }
}

inline void EvalState::forceAttrs(Value & v)
{
    forceValue(v);
    if (v.type != tAttrs)
        throwTypeError("value is %1% while a set was expected", v);
}


inline void EvalState::forceAttrs(Value & v, const Pos & pos)
{
    forceValue(v, pos);
    if (v.type != tAttrs)
        throwTypeError(pos, "value is %1% while a set was expected", v);
}


inline void EvalState::forceList(Value & v)
{
    forceValue(v);
    if (!v.isList())
        throwTypeError("value is %1% while a list was expected", v);
}


inline void EvalState::forceList(Value & v, const Pos & pos)
{
    forceValue(v, pos);
    if (!v.isList())
        throwTypeError(pos, "value is %1% while a list was expected", v);
}

/* Note: Various places expect the allocated memory to be zeroed. */
inline void * allocBytes(size_t n)
{
    void * p;
#if HAVE_BOEHMGC
    p = GC_MALLOC(n);
#else
    p = calloc(n, 1);
#endif
    if (!p) throw std::bad_alloc();
    return p;
}


}
