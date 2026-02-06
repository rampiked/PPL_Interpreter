// main.cpp
// PPL interpreter (single-file) - C++11

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <memory>
#include <algorithm>
#include <cctype>
#include <stdexcept>

using namespace std;

// Forward
struct Value;
struct ListNode;
typedef shared_ptr<ListNode> ListPtr;

enum ValueType { VT_INT, VT_LIST };

struct Value {
    ValueType type;
    long long ival;
    ListPtr list;

    Value() : type(VT_INT), ival(0), list(nullptr) {}
    static Value make_int(long long v) { Value x; x.type = VT_INT; x.ival = v; return x; }
    static Value make_list(ListPtr l) { Value x; x.type = VT_LIST; x.list = l; return x; }
    // Deep copy
    Value deep_copy() const;
};

struct ListNode {
    Value v;
    ListPtr next;
    ListNode(const Value &val, ListPtr nx = nullptr) : v(val), next(nx) {}
};

Value Value::deep_copy() const {
    if (type == VT_INT) return Value::make_int(ival);
    // deep copy list nodes
    if (!list) return Value::make_list(nullptr);
    // copy nodes recursively
    // We'll copy node-by-node preserving order.
    ListPtr head = nullptr;
    ListPtr *pp = &head;
    ListPtr curSrc = list;
    while (curSrc) {
        Value vcopy = curSrc->v.type == VT_INT ? Value::make_int(curSrc->v.ival)
                                               : Value::make_list(curSrc->v.list ? curSrc->v.list->v.deep_copy().list : nullptr);
        // But nested lists need deep copy; use recursion:
        if (curSrc->v.type == VT_LIST) vcopy = curSrc->v.deep_copy();
        *pp = make_shared<ListNode>(vcopy, nullptr);
        pp = &((*pp)->next);
        curSrc = curSrc->next;
    }
    return Value::make_list(head);
}

// Utility: print value
string value_to_string(const Value &val);

string list_to_string(ListPtr head) {
    if (!head) return "[]";
    string s = "[";
    ListPtr cur = head;
    bool first = true;
    while (cur) {
        if (!first) s += ", ";
        s += value_to_string(cur->v);
        first = false;
        cur = cur->next;
    }
    s += "]";
    return s;
}

string value_to_string(const Value &val) {
    if (val.type == VT_INT) return to_string(val.ival);
    return list_to_string(val.list);
}

// Environment: map from identifier -> Value + type info
struct Env {
    map<string, Value> table;

    bool exists(const string &id) const { return table.find(id) != table.end(); }
    Value &get(const string &id) {
        auto it = table.find(id);
        if (it == table.end()) throw runtime_error("Undefined identifier: " + id);
        return it->second;
    }
    const Value &get_const(const string &id) const {
        auto it = table.find(id);
        if (it == table.end()) throw runtime_error("Undefined identifier: " + id);
        return it->second;
    }
    void set(const string &id, const Value &v) { table[id] = v; }
};

// Base Instruction class (Command)
struct Instruction {
    int lineNo;
    Instruction(int l=0): lineNo(l) {}
    virtual ~Instruction() {}
    // execute returns next instruction index (1-based line number). Return -1 for HLT/terminate.
    virtual int execute(Env &env, int pc, vector<Instruction*> &program) = 0;
};

typedef vector<string> Tokens;

static Tokens tokenize(const string &line) {
    Tokens t;
    istringstream iss(line);
    string w;
    while (iss >> w) t.push_back(w);
    return t;
}

static long long to_int_const(const string &s, bool &ok) {
    // parse decimal integer
    try {
        size_t idx=0;
        long long v = stoll(s, &idx, 10);
        ok = (idx == s.size());
        return v;
    } catch (...) { ok = false; return 0; }
}

// Helper to check identifier name validity (simple)
static bool is_identifier(const string &s) {
    if (s.empty()) return false;
    if (!isalpha(s[0]) && s[0] != '_') return false;
    for (char c: s) if (!isalnum(c) && c != '_') return false;
    return true;
}

// Concrete instructions

struct Instr_INTEGER : Instruction {
    string id;
    Instr_INTEGER(int l, const string &id_) : Instruction(l), id(id_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (env.exists(id)) throw runtime_error("Line " + to_string(lineNo) + ": Identifier already declared: " + id);
        env.set(id, Value::make_int(0));
        return pc + 1;
    }
};

struct Instr_LIST : Instruction {
    string id;
    Instr_LIST(int l, const string &id_) : Instruction(l), id(id_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (env.exists(id)) throw runtime_error("Line " + to_string(lineNo) + ": Identifier already declared: " + id);
        env.set(id, Value::make_list(nullptr));
        return pc + 1;
    }
};

struct Instr_MERGE : Instruction {
    string from, tolist;
    Instr_MERGE(int l, const string &a, const string &b) : Instruction(l), from(a), tolist(b) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(from)) throw runtime_error("Line " + to_string(lineNo) + ": Undefined identifier: " + from);
        if (!env.exists(tolist)) throw runtime_error("Line " + to_string(lineNo) + ": Undefined list identifier: " + tolist);
        Value vfrom = env.get_const(from).deep_copy(); // copy of value inserted
        Value target = env.get_const(tolist);
        if (target.type != VT_LIST) throw runtime_error("Line " + to_string(lineNo) + ": MERGE target is not a list: " + tolist);
        // prepend
        ListPtr old = target.list;
        ListPtr newhead = make_shared<ListNode>(vfrom, old);
        env.set(tolist, Value::make_list(newhead));
        return pc + 1;
    }
};

struct Instr_COPY : Instruction {
    string src, dst;
    Instr_COPY(int l, const string &a, const string &b) : Instruction(l), src(a), dst(b) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(src)) throw runtime_error("Line " + to_string(lineNo) + ": Undefined source: " + src);
        const Value &v = env.get_const(src);
        if (v.type != VT_LIST) throw runtime_error("Line " + to_string(lineNo) + ": COPY source is not a list: " + src);
        Value copy = v.deep_copy();
        env.set(dst, copy);
        return pc + 1;
    }
};

struct Instr_HEAD : Instruction {
    string listid, id;
    Instr_HEAD(int l, const string &listid_, const string &id_) : Instruction(l), listid(listid_), id(id_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(listid)) throw runtime_error("Line " + to_string(lineNo) + ": Undefined list: " + listid);
        const Value &lv = env.get_const(listid);
        if (lv.type != VT_LIST) throw runtime_error("Line " + to_string(lineNo) + ": HEAD target not a list: " + listid);
        if (!lv.list) throw runtime_error("Line " + to_string(lineNo) + ": HEAD on empty list: " + listid);
        Value headval = lv.list->v.deep_copy();
        env.set(id, headval); // create or replace id
        return pc + 1;
    }
};

struct Instr_TAIL : Instruction {
    string src, dst;
    Instr_TAIL(int l, const string &a, const string &b) : Instruction(l), src(a), dst(b) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(src)) throw runtime_error("Line " + to_string(lineNo) + ": Undefined list: " + src);
        const Value &sv = env.get_const(src);
        if (sv.type != VT_LIST) throw runtime_error("Line " + to_string(lineNo) + ": TAIL source not a list: " + src);
        ListPtr head = sv.list;
        if (!head) {
            env.set(dst, Value::make_list(nullptr)); // empty list
            return pc + 1;
        }
        // copy nodes from head->next onward
        ListPtr cur = head->next;
        ListPtr newHead = nullptr;
        ListPtr *pp = &newHead;
        while (cur) {
            // deep copy element value
            Value vcopy = cur->v.deep_copy();
            *pp = make_shared<ListNode>(vcopy, nullptr);
            pp = &((*pp)->next);
            cur = cur->next;
        }
        env.set(dst, Value::make_list(newHead));
        return pc + 1;
    }
};

struct Instr_ASSIGN : Instruction {
    string id;
    long long val;
    Instr_ASSIGN(int l, const string &id_, long long v_) : Instruction(l), id(id_), val(v_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (env.exists(id)) {
            Value &existing = env.get(id);
            if (existing.type != VT_INT) throw runtime_error("Line " + to_string(lineNo) + ": ASSIGN to non-int: " + id);
            existing.ival = val;
        } else {
            env.set(id, Value::make_int(val));
        }
        return pc + 1;
    }
};

struct Instr_CHS : Instruction {
    string id;
    Instr_CHS(int l, const string &id_) : Instruction(l), id(id_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(id)) throw runtime_error("Line " + to_string(lineNo) + ": CHS undefined id: " + id);
        Value &v = env.get(id);
        if (v.type != VT_INT) throw runtime_error("Line " + to_string(lineNo) + ": CHS on non-int: " + id);
        v.ival = -v.ival;
        return pc + 1;
    }
};

struct Instr_ADD : Instruction {
    string a, b;
    Instr_ADD(int l, const string &a_, const string &b_) : Instruction(l), a(a_), b(b_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(a)) throw runtime_error("Line " + to_string(lineNo) + ": ADD undefined id: " + a);
        if (!env.exists(b)) throw runtime_error("Line " + to_string(lineNo) + ": ADD undefined id: " + b);
        Value &va = env.get(a);
        Value &vb = env.get(b);
        if (va.type != VT_INT || vb.type != VT_INT) throw runtime_error("Line " + to_string(lineNo) + ": ADD type error");
        va.ival += vb.ival;
        return pc + 1;
    }
};

struct Instr_IF : Instruction {
    string id;
    int target;
    Instr_IF(int l, const string &id_, int target_) : Instruction(l), id(id_), target(target_) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        if (!env.exists(id)) throw runtime_error("Line " + to_string(lineNo) + ": IF undefined id: " + id);
        const Value &v = env.get_const(id);
        bool cond = false;
        if (v.type == VT_INT) cond = (v.ival == 0);
        else cond = (v.list == nullptr);
        if (cond) {
            if (target < 1 || target > (int)program.size()) throw runtime_error("Line " + to_string(lineNo) + ": IF jump out of range: " + to_string(target));
            return target; // line numbers are 1-based
        } else return pc + 1;
    }
};

struct Instr_HLT : Instruction {
    Instr_HLT(int l) : Instruction(l) {}
    int execute(Env &env, int pc, vector<Instruction*> &program) override {
        return -1; // terminate
    }
};

// Parser: create an Instruction for each line
Instruction* parse_line(const string &line, int lineno) {
    Tokens t = tokenize(line);
    if (t.empty()) return nullptr; // allow blank lines (ignored)
    string op = t[0];
    if (op == "INTEGER") {
        if (t.size() != 2) throw runtime_error("Line " + to_string(lineno) + ": INTEGER requires exactly one argument");
        if (!is_identifier(t[1])) throw runtime_error("Line " + to_string(lineno) + ": invalid identifier: " + t[1]);
        return new Instr_INTEGER(lineno, t[1]);
    } else if (op == "LIST") {
        if (t.size() != 2) throw runtime_error("Line " + to_string(lineno) + ": LIST requires exactly one argument");
        if (!is_identifier(t[1])) throw runtime_error("Line " + to_string(lineno) + ": invalid identifier: " + t[1]);
        return new Instr_LIST(lineno, t[1]);
    } else if (op == "MERGE") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": MERGE requires two arguments");
        return new Instr_MERGE(lineno, t[1], t[2]);
    } else if (op == "COPY") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": COPY requires two arguments");
        return new Instr_COPY(lineno, t[1], t[2]);
    } else if (op == "HEAD") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": HEAD requires two arguments");
        return new Instr_HEAD(lineno, t[1], t[2]);
    } else if (op == "TAIL") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": TAIL requires two arguments");
        return new Instr_TAIL(lineno, t[1], t[2]);
    } else if (op == "ASSIGN") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": ASSIGN requires two arguments");
        bool ok=false; long long v = to_int_const(t[2], ok);
        if (!ok) throw runtime_error("Line " + to_string(lineno) + ": ASSIGN needs integer constant, got: " + t[2]);
        return new Instr_ASSIGN(lineno, t[1], v);
    } else if (op == "CHS") {
        if (t.size() != 2) throw runtime_error("Line " + to_string(lineno) + ": CHS requires one argument");
        return new Instr_CHS(lineno, t[1]);
    } else if (op == "ADD") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": ADD requires two arguments");
        return new Instr_ADD(lineno, t[1], t[2]);
    } else if (op == "IF") {
        if (t.size() != 3) throw runtime_error("Line " + to_string(lineno) + ": IF requires two arguments");
        bool ok=false; int target = (int)to_int_const(t[2], ok);
        if (!ok || target <= 0) throw runtime_error("Line " + to_string(lineno) + ": IF target must be positive integer");
        return new Instr_IF(lineno, t[1], target);
    } else if (op == "HLT") {
        if (t.size() != 1) throw runtime_error("Line " + to_string(lineno) + ": HLT takes no arguments");
        return new Instr_HLT(lineno);
    } else {
        throw runtime_error("Line " + to_string(lineno) + ": Unknown operation: " + op);
    }
}

// Read program file -> vector<Instruction*>
vector<Instruction*> load_program(const string &filename) {
    ifstream in(filename.c_str());
    if (!in) throw runtime_error("Unable to open file: " + filename);
    vector<Instruction*> prog;
    string line;
    int lineno = 0;
    while (std::getline(in, line)) {
        ++lineno;
        // Trim trailing/leading whitespace
        size_t start = line.find_first_not_of(" \t\r\n");
        if (start == string::npos) { prog.push_back(nullptr); continue; } // blank line allowed
        size_t end = line.find_last_not_of(" \t\r\n");
        string trimmed = line.substr(start, end - start + 1);
        //We parse a line to create a specific instruction.
        Instruction *ins = parse_line(trimmed, lineno);
        prog.push_back(ins); // may be nullptr if blank line
    }
    // Replace nullptr blank lines with NOPs (we'll map them to a no-op instruction)
    // For simplicity, convert nullptr to HLT? No: better convert to a no-op object.
    for (size_t i = 0; i < prog.size(); ++i) {
        if (!prog[i]) {
            // create a tiny instruction that does nothing
            struct NOP : Instruction { NOP(int l): Instruction(l){} int execute(Env&, int pc, vector<Instruction*>&){ return pc+1; } };
            prog[i] = new NOP((int)i+1);
        }
    }
    return prog;
}

void free_program(vector<Instruction*> &prog) {
    for (Instruction* p : prog) delete p;
    prog.clear();
}

// Execute program
void run_program(vector<Instruction*> &prog, Env &env) {
    int pc = 1; // 1-based
    int lines = (int)prog.size();
    while (pc >= 1) {
        if (pc > lines) break; // fall off end => terminate
        Instruction* ins = prog[pc-1];
        try {
            int next = ins->execute(env, pc, prog);
            if (next == -1) break; // HLT
            pc = next;
        } catch (const runtime_error &e) {
            cerr << "Runtime error: " << e.what() << endl;
            return;
        }
    }
    // Print all identifiers sorted
    vector<string> names;
    for (auto &p : env.table) names.push_back(p.first);
    sort(names.begin(), names.end());
    for (auto &n : names) {
        cout << n << " = ";
        const Value &v = env.get_const(n);
        if (v.type == VT_INT) cout << v.ival << "\n";
        else cout << list_to_string(v.list) << "\n";
    }
}

// CLI
int main(int argc, char **argv) {
    if (argc != 2) {
        cerr << "Usage: ppl <program-file>\n";
        return 1;
    }
    string fname = argv[1];
    vector<Instruction*> prog;
    try {
        //Loads PPL instructions into prog Instruction* vector.
        prog = load_program(fname);
    } catch (const exception &e) {
        cerr << "Error loading program: " << e.what() << endl;
        return 1;
    }
    Env env;
    //Runs program using a default environment and the loaded instructions.
    run_program(prog, env);
    //Clears the instructions (if re-use were to be desired)
    free_program(prog);
    return 0;
}