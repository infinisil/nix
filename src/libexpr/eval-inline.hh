#pragma once

#include "eval.hh"

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


void EvalState::forceValue(Value & v, const Pos & pos)
{
    evalValueMinimal(v, pos);
    if (v.type == tLazyBinOp) {
        v.lazyBinOp->expr->evalLazyBinOp(*this, *v.lazyBinOp->env, v);
    } else if (v.type == tValue) {
        v.type = tBlackhole;
        forceValue(*v.value.value, pos != noPos ? pos : *v.value.pos);
        v = *v.value.value;
    }
}

void EvalState::evalValueMinimal(Value & v, const Pos & pos)
{
    if (v.type == tThunk) {
        Env * env = v.thunk.env;
        Expr * expr = v.thunk.expr;
        try {
            // tBlackhole indicates that any further forcing of this value should throw inf rec
            // However, this only causes inf rec when the forcing happens *before* the value is assigned its final value
            // Meaning that within the expr->eval you'd have to do `<evaluations>; v.type = <result>`
            // However, if expressions implement their own infinite recursion check (like ExprOpUpdate!), it can do
            //   `v.type = <something>; <evaluations>`
            // Which means that the infinite recursion detection from this forceValue is prevented, since the tBlackhole is unset before the potentially recursive evaluations
            v.type = tBlackhole;
            //checkInterrupt();
            expr->evalMinimal(*this, *env, v);
        } catch (...) {
            v.type = tThunk;
            v.thunk.env = env;
            v.thunk.expr = expr;
            throw;
        }
    } else if (v.type == tApp) {
        callFunctionMinimal(*v.app.left, *v.app.right, v, pos);
    } else if (v.type == tBlackhole) {
        throwEvalError(pos, "infinite recursion encountered (tBlackhole in evalValueAttr)");
    }
}

Attr * EvalState::evalValueAttr(Value & v, const Symbol & name, const Pos & pos)
{
    // No need to set tBlackhole's here, because evaluating attributes of values doesn't require evaluation, and inf rec within lazyBinOps is handled by them directly

    evalValueMinimal(v, pos);
    if (v.type == tAttrs) {
        Bindings::iterator j;
        if ((j = v.attrs->find(name)) == v.attrs->end()) {
            return nullptr;
        }
        return j;
    } else if (v.type == tLazyBinOp) {
        return v.lazyBinOp->expr->evalLazyBinOpAttr(*this, *v.lazyBinOp->env, name, v);
    } else if (v.type == tValue) {
        Attr * result = evalValueAttr(*v.value.value, name, pos != noPos ? pos : *v.value.pos);
        if (v.value.value->type != tLazyBinOp) {
            v = *v.value.value;
        }
        return result;
    } else {
        return nullptr;
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
