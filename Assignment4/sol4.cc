#include <string>
#include <iostream>
#include <variant>
#include <map>
#include <exception>
#include <stdexcept>
#include "box.h"
#include "List.h"

using std::variant;
using std::string;

// Definition of Expr variants
struct Var {
    string name;
    Var(string _name): name(_name){};
    operator std::string() const { return "Var("+name+")"; }
};
struct Int {
    int val;
    Int(int _val): val(_val){};
    operator std::string() const { return "Int("+std::to_string(val)+")"; }
};
struct AUnit {
    AUnit() {};
    operator std::string() const { return "AUnit()"; }
};
using Expr = variant<Var, 
                     Int,
                     AUnit,
                     box<struct IsAUnit>, 
                     box<struct Add>, 
                     box<struct IfGreater>, 
                     box<struct MLet>,
                     box<struct Fun>, 
                     box<struct Closure>,
                     box<struct APair>,
                     box<struct Fst>,
                     box<struct Snd>,
                     box<struct Call>>; 

template<typename T> bool is(Expr e);

std::string toString(Expr e);

struct Add {
    Expr e1, e2;
    Add(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {}; 
    operator std::string() const { 
        return "Add("+toString(e1)+", "+toString(e2)+")";
    }
};
struct IfGreater {
    Expr e1, e2, e3, e4;
    IfGreater(Expr _e1, Expr _e2, Expr _e3, Expr _e4): e1(_e1), e2(_e2), e3(_e3), e4(_e4) {};
    operator std::string() const {
        return "IfGreater("+toString(e1)+", "+toString(e2)+", "
                           +toString(e3)+", "+toString(e4)+")";
    }
};
struct MLet {
    string varName;
    Expr e1, e2;
    MLet(string _varName, Expr _e1, Expr _e2): varName(_varName), e1(_e1), e2(_e2) {}; 
    operator std::string() const { 
        return "MLet("+varName+", "+toString(e1)+", "+toString(e2)+")";
    }
};
struct APair {
    Expr e1, e2;
    APair(Expr _e1, Expr _e2): e1(_e1), e2(_e2) {};
    operator std::string() const { 
        return "APair("+toString(e1)+", "+toString(e2)+")";
    }
};
struct Fst {
    Expr e;
    Fst(Expr _e): e(_e) {};
    operator std::string() const { return "Fst("+toString(e)+")"; }
};
struct Snd {
    Expr e;
    Snd(Expr _e): e(_e) {};
    operator std::string() const { return "Snd("+toString(e)+")"; }
};
struct IsAUnit {
    Expr e;
    IsAUnit(Expr _e): e(_e) {};
    operator std::string() const { return "IsAUnit("+toString(e)+")"; }
};

struct Fun {
    string funName;
    string argName;
    Expr body;
    Fun(string _f, string _a, Expr _b): funName(_f), argName(_a), body(_b) {}; 
    operator std::string() const { 
        return "Fun("+funName+", "+argName+", "+toString(body)+")";
    }
};
struct Closure {
    std::map<string, Expr> env;
    Fun f;
    Closure(std::map<string, Expr> _env, Fun _f): env(_env), f(_f) {};
    operator std::string() const { 
        return "Closure(env, "+std::string(f)+")";
    }
};
struct Call {
    Expr funExpr, actual;
    Call(Expr _fe, Expr _a): funExpr(_fe), actual(_a) {};
    operator std::string() const { 
        return "Call("+toString(funExpr)+", "+toString(actual)+")";
    }
};
// End of Definition of Expr variants 

// Functions for check variants.
// e.g. is<APair>(e) or is<Int>(Expr(Int(42)))
template<typename T>
bool is(Expr e) { return std::holds_alternative<T>(e); }
template<>
bool is<Closure>(Expr e) { return std::holds_alternative<box<struct Closure>>(e); }
template<>
bool is<IsAUnit>(Expr e) { return std::holds_alternative<box<struct IsAUnit>>(e); }
template<>
bool is<Add>(Expr e) { return std::holds_alternative<box<struct Add>>(e); }
template<>
bool is<IfGreater>(Expr e) { return std::holds_alternative<box<struct IfGreater>>(e); }
template<>
bool is<MLet>(Expr e) { return std::holds_alternative<box<struct MLet>>(e); }
template<>
bool is<Fun>(Expr e) { return std::holds_alternative<box<struct Fun>>(e); }
template<>
bool is<APair>(Expr e) { return std::holds_alternative<box<struct APair>>(e); }
template<>
bool is<Fst>(Expr e) { return std::holds_alternative<box<struct Fst>>(e); }
template<>
bool is<Snd>(Expr e) { return std::holds_alternative<box<struct Snd>>(e); }
template<>
bool is<Call>(Expr e) { return std::holds_alternative<box<struct Call>>(e); }

// Converting Expr to std::string representation.
std::string toString(Expr e) {
    if (is<Int>(e)) {
        return std::get<Int>(e);
    } else if (is<Var>(e)) {
        return std::get<Var>(e);
    } else if (is<AUnit>(e)) {
        return std::get<AUnit>(e);
    } else if (is<IsAUnit>(e)) {
        return *std::get<box<struct IsAUnit>>(e);
    } else if (is<box<struct Add>>(e)) {
        Add add = *std::get<box<struct Add>>(e);
        return add;
    } else if (is<box<struct IfGreater>>(e)) {
        IfGreater ifgt = *std::get<box<struct IfGreater>>(e);
        return ifgt;
    } else if (is<box<struct MLet>>(e)) {
        MLet mlet = *std::get<box<struct MLet>>(e);
        return mlet;
    } else if (is<box<struct Fun>>(e)) {
        Fun fun = *std::get<box<struct Fun>>(e);
        return fun;
    } else if (is<box<struct Closure>>(e)) {
        Closure closure = *std::get<box<struct Closure>>(e);
        return closure;
    } else if (is<box<struct APair>>(e)) {
        return *std::get<box<struct APair>>(e);
    } else if (is<box<struct Fst>>(e)) {
        return *std::get<box<struct Fst>>(e);
    } else if (is<box<struct Snd>>(e)) {
        return *std::get<box<struct Snd>>(e);
    } else if (is<box<struct Call>>(e)) {
        Call call = *std::get<box<struct Call>>(e);
        return call;
    } else {
        throw std::runtime_error("toString(Expr): Unexpected Expr is given!");
    }
}

// Asserts that given Expr is a value in MUPL.
void assertValue(Expr e) {
    if (is<APair>(e)) {
        APair ap = *std::get<box<struct APair>>(e);
        assertValue(ap.e1);
        assertValue(ap.e2);
    } else if (!(is<Int>(e) || 
               is<Closure>(e) ||
               is<AUnit>(e))) {
        throw std::runtime_error(toString(e) + " is not a value!");
    }
}

// Make a new environment by copying from the passed environment.
std::map<string, Expr> makeNewEnvFrom(std::map<string, Expr> fromEnv) {
    std::map<string, Expr> newEnv(fromEnv);
    return newEnv;
}

Expr envlookup(std::map<string, Expr> env, Var v) {
    if (env.count(v.name) == 0) {
        throw std::runtime_error(toString(v)+" is not in the environment");
    } else {
        Expr val = env.at(v.name);
        assertValue(val);
        return val;
    }
}

Expr eval_under_env(Expr e, std::map<string, Expr> env) {
    return std::visit(overload {
        [&](Int& i) { return e;},
        [&](Var& v) {
          Expr val = envlookup(env, v);
          return val;
        },
        [&](box<struct Add>& a) {
          Expr e1 = eval_under_env(a->e1, env);
          Expr e2 = eval_under_env(a->e2, env);
          if (is<Int>(e1) && is<Int>(e2)) {
            Int i1 = std::get<Int>(e1);
            Int i2 = std::get<Int>(e2);
            Expr res(Int(i1.val+i2.val));
            return res;
          } else {
            throw std::runtime_error("Unexpected types for sub-expressions of Add");
          }
        },

        // TODO: Students need to implement following functions.
        [&](AUnit& au) { 
            /* TODO */
            return e;
        },
        [&](box<struct IsAUnit>& isa) { 
            /* TODO */
            Expr e1 = eval_under_env(isa->e, env);
            if (is<AUnit>(e1))
                return Expr(Int(1));
            else
                return Expr(Int(0));
        },
        [&](box<struct IfGreater>& ifgt) {
            /* TODO */
            Expr v1 = eval_under_env(ifgt->e1, env);
            Expr v2 = eval_under_env(ifgt->e2, env);
            if (is<Int>(v1) && is<Int>(v2)) {
                Int i1 = std::get<Int>(v1);
                Int i2 = std::get<Int>(v2);
                if (i1.val > i2.val)
                    return eval_under_env(ifgt->e3, env);
                else
                    return eval_under_env(ifgt->e4, env);
            } else {
                throw std::runtime_error("Unexpected types for sub-expressions of IfGreater");
            }
        }, 
        [&](box<struct MLet>& l) {
            /* TODO */
            Expr v = eval_under_env(l->e1, env);
            std::map<string, Expr> new_env = makeNewEnvFrom(env);
            new_env.insert_or_assign(l->varName, v);
            return eval_under_env(l->e2, new_env);
        },
        [&](box<struct Fun>& f) {
            /* TODO */
            return Expr(Closure(env, *f));
        },
        [&](box<struct Closure>& c) {
            /* TODO */
            return e;
        },
        [&](box<struct APair>& ap) {
            /* TODO */
            Expr e1 = eval_under_env(ap->e1, env);
            Expr e2 = eval_under_env(ap->e2, env);
            return Expr(APair(e1, e2));
        },
        [&](box<struct Fst>& fst) { 
            /* TODO */
            Expr v = eval_under_env(fst->e, env);
            if (is<box<struct APair>>(v)) {
                APair p = *std::get<box<struct APair>>(v);
                return Expr(p.e1);
            } else {
                throw std::runtime_error("Unexpected types for sub-expressions of Fst");
            }
        },
        [&](box<struct Snd>& snd) { 
            /* TODO */
            Expr e1 = eval_under_env(snd->e, env);
            if (is<box<struct APair>>(e1)) {
                APair ap = *std::get<box<struct APair>>(e1);
                return Expr(ap.e2);
            } else {
                throw std::runtime_error("Unexpected types for sub-expressions of Snd"); 
            }
        },
        [&](box<struct Call>& call) {
            /* TODO */
            Expr e1 = eval_under_env(call->funExpr, env);
            Expr e2 = eval_under_env(call->actual, env);
            if (is<box<struct Closure>>(e1)) {
                Closure c = *std::get<box<struct Closure>>(e1);
                std::map<string, Expr> new_env = makeNewEnvFrom(c.env);
                if (c.f.funName != "") {
                    new_env.insert_or_assign(c.f.argName, e2);
                    new_env.insert_or_assign(c.f.funName, e1);
                    return eval_under_env(c.f.body, new_env);
                }
                else {
                    new_env.insert_or_assign(c.f.argName, e2);
                    return eval_under_env(c.f.body, new_env);
                }
            } else {
                throw std::runtime_error("Unexpected types for sub-expressions of Call");
            }
        },
      }, e);
}

Expr eval(Expr e) {
    std::map<string, Expr> env;
    return eval_under_env(e, env);
}


Expr makeIntList(int from, int to) {
    Expr next = AUnit();
    Expr res = AUnit();
    for (int i=to-1; i>=from; i--) {
        Expr tmp = APair(Int(i), next);
        res = tmp;
        next = tmp;
    }
    return res;
}

Expr IfAUnit(Expr e1, Expr e2, Expr e3) {
    return IfGreater(IsAUnit(e1), Int(0), e2, e3);
}

Expr MuplMap() {
    return Expr(Fun("", "fun_arg",
                Fun("muplrec", "lst",
                    IfAUnit(Var("lst"),
                        AUnit(),
                        APair(Call(Var("fun_arg"), Fst(Var("lst"))),
                            Call(Var("muplrec"), Snd(Var("lst"))))))));
}

Expr MuplMapAddN() {
    return Expr(MLet(
        "map", MuplMap(), Fun("", "I", Call(Var("map"), Fun("", "n", Add(Var("n"), Var("I")))))
    ));
}

Expr ToMuplList(List<Expr> l) {
    if (l.isEmpty()) {
        return Expr(AUnit());
    }
    return Expr(APair(l.head(), ToMuplList(l.tail())));
}

List<Expr> FromMuplList(Expr e) {
    return std::visit(overload {
        [&](AUnit& a) { 
          return List<Expr>();
        },
        [&](box<struct APair>& p) { // ToDo
          return FromMuplList(p->e2).cons(p->e1);
        },
        [&](auto a) {
          throw std::runtime_error("Unexpected types for sub-expressions of Add");
          return List<Expr>();
        }
    },e);
}

int main() {
    // Test code for eval()
    std::map<string, Expr> env;
    env.insert_or_assign("a", Expr(Int(40)));

    Expr e = Add(Var("a"), Int(2));
    Expr res = eval_under_env(e, env);
    Int i = std::get<Int>(res);
    std::cout << toString(e) << " = " << i.val << std::endl;

    Expr e2 = MLet("a", Int(5), MLet("b", Int(10), Add(Var("a"), Var("b"))));
    res = eval_under_env(e2, env);
    i = std::get<Int>(res);
    std::cout << toString(e2) << " = " << i.val << std::endl;

    res = eval(e2);
    i = std::get<Int>(res);
    std::cout << toString(e2) << " = " << i.val << std::endl;

    Expr e3 = Call(Fun("add1", "x", Add(Var("x"), Int(1))), Int(41));
    res = eval_under_env(e3, env);
    i = std::get<Int>(res);
    std::cout << toString(e3) << " = " << i.val << std::endl; 

    Expr e4 = IfGreater(Int(0), Int(1), Int(42), Int(-42));
    res = eval_under_env(e4, env);
    std::cout << toString(e4) << " = " << toString(res) << std::endl; 

    Expr e5 = MLet("a", Int(5), Add(Var("a"), MLet("a", Int(10), Add(Var("a"), Int(1)))));
    res = eval_under_env(e5, env);
    std::cout << toString(e5) << " = " << toString(res) << std::endl;

    Expr e6 = APair(Add(Int(0), Int(10)), APair(Int(1), AUnit()));
    res = eval_under_env(e6, env);
    std::cout << toString(e6) << " = " << toString(res) << std::endl;

    Expr e7 = makeIntList(0, 2);
    std::cout << toString(e7) << " = " << toString(e7) << std::endl;

    Expr e8 = eval(Call(Call(MuplMapAddN(), Int(10)), makeIntList(0, 5)));
    std::cout << toString(e8) << " = " << toString(e8) << std::endl;

    return 0;
}