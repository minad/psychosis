#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <assert.h>
#include <gc/gc.h>
#include <wchar.h>
#include <wctype.h>
#include <complex.h>
#include <stdint.h>

typedef enum {
        TOKEN_INVALID,
        TOKEN_END,
        TOKEN_QUOTE,
        TOKEN_QUASIQUOTE,
        TOKEN_UNQUOTE,
        TOKEN_UNQUOTE_SPLICING,
        TOKEN_SYNTAX,
        TOKEN_QUASISYNTAX,
        TOKEN_UNSYNTAX,
        TOKEN_UNSYNTAX_SPLICING,
        TOKEN_LPAREN,
        TOKEN_RPAREN,
        TOKEN_LBRACKET,
        TOKEN_RBRACKET,
        TOKEN_VECTOR,
        TOKEN_BYTEVECTOR,
        TOKEN_TRUE,
        TOKEN_FALSE,
        TOKEN_DOT,
        TOKEN_STRING,
        TOKEN_IDENTIFIER,
        TOKEN_INTEGER,
        TOKEN_CHARACTER,
} TOKEN;

#define T(n) ((n << 2) | 2)
typedef enum {
        TYPE_UNSPECIFIED = T(1),
        TYPE_EMPTY = T(2),
        TYPE_SYMBOL = T(3),
        TYPE_TRUE = T(4),
        TYPE_FALSE = T(5),
        TYPE_CHARACTER = T(6),
        TYPE_VECTOR = T(7),
        TYPE_BYTEVECTOR = T(8),
        TYPE_STRING = T(9),
        TYPE_RATIONAL = T(10),
        TYPE_REAL = T(11),
        TYPE_COMPLEX = T(12),
        TYPE_BUILTIN = T(13),
        TYPE_PROCEDURE = T(14),
        TYPE_CONTINUATION = T(15),
        // integer and pair have a special handling
        // enforce pointer size enum
        TYPE_INTEGER = (1LL << ((sizeof(void*) * 8) - 1)),
        TYPE_PAIR,
} TYPE;
#undef T

enum {
        BUILTIN_NEW_SCOPE   = 1,
        BUILTIN_EVAL_RESULT = 2,
        BUILTIN_EVAL_ARGS   = 4,
};

typedef union VALUE* VALUE;
typedef struct SCOPE* SCOPE;
typedef struct HANDLER* HANDLER;
typedef struct VARIABLE* VARIABLE;

typedef struct {
        TYPE     type;
        size_t   size;
        wchar_t* elements;
} STRING;

typedef struct {
        TYPE   type;
        size_t size;
        char*  elements;
} BYTEVECTOR;

typedef struct {
        TYPE             type;
        int              flags : 8;
        int              args  : 8;
        const wchar_t*   name;
        union {
                VALUE (*function0)(SCOPE);
                VALUE (*function1)(SCOPE, VALUE);
                VALUE (*function2)(SCOPE, VALUE, VALUE);
                VALUE (*function3)(SCOPE, VALUE, VALUE, VALUE);
                VALUE (*functionN)(VALUE, SCOPE); // Not the same as function1
        };
} BUILTIN;

typedef struct {
        VALUE car, cdr;
} PAIR;

typedef struct {
        TYPE   type;
        size_t size;
        VALUE* elements;
} VECTOR;

typedef struct {
        TYPE  type;
        VALUE name;
        VALUE args;
        VALUE body;
        SCOPE scope;
} PROCEDURE;

typedef struct {
        TYPE   type;
        char   exact;
        double value;
} REAL;

typedef struct {
        TYPE           type;
        char           exact;
        double complex value;
} COMPLEX;

typedef struct {
        TYPE type;
        int  nominator;
        int  denominator;
} RATIONAL;

typedef struct {
        TYPE    type;
        wchar_t value;
} CHARACTER;

typedef struct {
        TYPE    type;
        jmp_buf jmp;
        void*   stack_pos;
        size_t  stack_size;
        VALUE   result;
        char    stack[0];
} CONTINUATION;

typedef struct {
        TYPE    type;
        VALUE   left, right;
        size_t  size;
        wchar_t elements[0];
} SYMBOL;

union VALUE {
        TYPE type;
        struct { // pair shortcut for simpler access
                VALUE car, cdr;
        };
        VECTOR       vector;
        PROCEDURE    procedure;
        RATIONAL     rational;
        REAL         real;
        COMPLEX      komplex;
        CHARACTER    character;
        SYMBOL       symbol;
        STRING       string;
        BYTEVECTOR   bytevector;
        BUILTIN      builtin;
        CONTINUATION continuation;
};

struct VARIABLE {
        VALUE    symbol;
        VALUE    value;
        VARIABLE left, right;
};

struct HANDLER {
        jmp_buf jmp;
};

struct SCOPE {
        HANDLER  handler;
        SCOPE    outer;
        VARIABLE variables;
};

typedef struct {
        char*  elements;
        size_t size;
        size_t capacity;
} BUFFER;

typedef struct {
        wchar_t* elements;
        size_t   size;
        size_t   capacity;
} STRBUFFER;

#define BUFFER_INIT { .elements = 0, .size = 0, .capacity = 0 }

union VALUE _unspecified = { .type = TYPE_UNSPECIFIED };
union VALUE _empty = { .type = TYPE_EMPTY };
union VALUE _true = { .type = TYPE_TRUE };
union VALUE _false = { .type = TYPE_FALSE };

#define UNSPECIFIED (&_unspecified)
#define EMPTY       (&_empty)
#define TRUE        (&_true)
#define FALSE       (&_false)

STRBUFFER token_buffer;
int token_integer;
wchar_t token_character;

int line = 1, column = 0;
FILE* in;
wint_t lookahead = WEOF;

VALUE symbols = 0;
void* stack_start;

CONTINUATION* active_continuation;

SCOPE TOP = 0;

const wchar_t* named_characters[] = {
        L"\0"   "nul",
        L"\a"   "alarm",
        L"\b"   "backspace",
        L"\t"   "tab",
        L"\n"   "linefeed",
        L"\n"   "newline",
        L"\v"   "vtab",
        L"\f"   "page",
        L"\r"   "return",
        L"\x1B" "esc",
        L" "    "space",
        L"\x7F" "delete",
        0,
};

#define ASSERTION(description, assert)                     \
        do {                                               \
                if (!(assert)) {                           \
                        ASSERTION_VIOLATION(description);  \
                }                                          \
        } while(0)

#define ASSERT_TYPE(value, t) ASSERTION(L"Wrong type", get_type(value) == (t))

#define ASSERTION_VIOLATION(description)                                \
        do {                                                            \
                wprintf(L"Error: %ls at %s\n", description, __FUNCTION__); \
                while (!S->handler)                                     \
                        S = S->outer;                                   \
                longjmp(S->handler->jmp, 1);                            \
        } while(0)

#define LEXICAL_ASSERTION(description, assert) ASSERTION(description, assert)
#define LEXICAL_VIOLATION(description) ASSERTION_VIOLATION(description)

#define EXPECT_ARGUMENTS(count)                    \
        do {                                       \
                int n;                             \
                VALUE a = args;                    \
                for (n = 0; n < (count); ++n) {    \
                        ASSERT_TYPE(a, TYPE_PAIR); \
                        a = a->cdr;                \
                }                                  \
                ASSERTION(L"No list", a == EMPTY); \
        } while (0)

VALUE add_symbol(const wchar_t* name, size_t size);
VALUE apply(SCOPE S, VALUE proc, VALUE args);
void buffer_append(BUFFER* b, char c);
void buffer_set(BUFFER* b, const char* s, size_t size);
VALUE call_builtin(BUILTIN* builtin, SCOPE S, VALUE args);
VALUE call_procedure(VALUE proc, VALUE arg);
void define_variable(SCOPE S, VALUE symbol, VALUE value);
static inline int delimiter_p(wint_t c);
static inline wint_t eat();
VALUE eval(SCOPE S, VALUE value);
VALUE eval_list(SCOPE S, VALUE list);
VALUE get_symbol(const wchar_t* name);
static inline TYPE get_type(VALUE value);
static inline int initial_p(wint_t c);
VALUE let(SCOPE S, SCOPE eval_scope, VALUE args);
TOKEN lex(SCOPE S);
static inline wint_t look();
VALUE lookup(SCOPE S, VALUE symbol);
VALUE macro_begin(VALUE body, SCOPE S);
VALUE macro_define(VALUE args, SCOPE S);
VALUE macro_if(VALUE args, SCOPE S);
VALUE macro_lambda(VALUE args, SCOPE S);
VALUE macro_let(VALUE args, SCOPE S);
VALUE macro_letstar(VALUE args, SCOPE S);
VALUE macro_quasiquote(SCOPE S, VALUE value);
VALUE macro_quasisyntax(SCOPE S, VALUE value);
VALUE macro_quote(SCOPE S, VALUE value);
VALUE macro_set_i(SCOPE S, VALUE symbol, VALUE value);
VALUE macro_syntax(SCOPE S, VALUE value);
VALUE macro_unquote(SCOPE S, VALUE value);
VALUE macro_unquote_splicing(SCOPE S, VALUE value);
VALUE macro_unsyntax(SCOPE S, VALUE value);
VALUE macro_unsyntax_splicing(SCOPE S, VALUE value);
int main(int argc, char* argv[]);
static inline VALUE new_atomic_value(size_t size, TYPE type);
VALUE new_bytevector(const char* s, size_t size);
VALUE new_character(wchar_t character);
HANDLER new_handler();
static inline VALUE new_integer(int integer);
VALUE new_pair(VALUE car, VALUE cdr);
VALUE new_procedure(SCOPE S, VALUE name, VALUE args, VALUE body);
SCOPE new_scope(SCOPE outer);
VALUE new_string(const wchar_t* s, size_t size);
VALUE new_symbol(const wchar_t* name, size_t size);
static inline VALUE new_value(size_t size, TYPE type);
VARIABLE new_variable(VALUE symbol, VALUE value);
VALUE new_vector();
VALUE parse(SCOPE S);
VALUE proc_add(VALUE args, SCOPE S);
VALUE proc_boolean_equal_p(VALUE args, SCOPE S);
VALUE proc_boolean_p(SCOPE S, VALUE value);
VALUE proc_callcc(SCOPE S, VALUE proc);
VALUE proc_char_p(SCOPE S, VALUE value);
VALUE proc_complex_p(SCOPE S, VALUE value);
VALUE proc_cons(SCOPE S, VALUE car, VALUE cdr);
VALUE proc_display(SCOPE S, VALUE value);
VALUE proc_divide(VALUE args, SCOPE S);
VALUE proc_equal(VALUE args, SCOPE S);
VALUE proc_even_p(SCOPE S, VALUE value);
VALUE proc_exact_p(SCOPE S, VALUE value);
VALUE proc_greater(VALUE args, SCOPE S);
VALUE proc_greaterequal(VALUE args, SCOPE S);
VALUE proc_inexact_p(SCOPE S, VALUE value);
VALUE proc_integer_p(SCOPE S, VALUE value);
VALUE proc_length(SCOPE S, VALUE list);
VALUE proc_less(VALUE args, SCOPE S);
VALUE proc_lessequal(VALUE args, SCOPE S);
VALUE proc_list(VALUE args, SCOPE S);
VALUE proc_list_p(SCOPE S, VALUE list);
VALUE proc_multiply(VALUE args, SCOPE S);
VALUE proc_negative_p(SCOPE S, VALUE value);
VALUE proc_not(SCOPE S, VALUE value);
VALUE proc_null_p(SCOPE S, VALUE value);
VALUE proc_number_p(SCOPE S, VALUE value);
VALUE proc_odd_p(SCOPE S, VALUE value);
VALUE proc_pair_p(SCOPE S, VALUE value);
VALUE proc_positive_p(SCOPE S, VALUE value);
VALUE proc_procedure_p(SCOPE S, VALUE value);
VALUE proc_rational_p(SCOPE S, VALUE value);
VALUE proc_real_p(SCOPE S, VALUE value);
VALUE proc_string_copy(SCOPE S, VALUE value);
VALUE proc_string_length(SCOPE S, VALUE value);
VALUE proc_string_p(SCOPE S, VALUE value);
VALUE proc_string_ref(SCOPE S, VALUE value, VALUE index);
VALUE proc_string_to_symbol(SCOPE S, VALUE value);
VALUE proc_subtract(VALUE args, SCOPE S);
VALUE proc_symbol_equal_p(VALUE args, SCOPE S);
VALUE proc_symbol_p(SCOPE S, VALUE value);
VALUE proc_symbol_to_string(SCOPE S, VALUE value);
VALUE proc_vector_fill_i(SCOPE S, VALUE vector, VALUE fill);
VALUE proc_vector_length(SCOPE S, VALUE value);
VALUE proc_vector_p(SCOPE S, VALUE value);
VALUE proc_vector_ref(SCOPE S, VALUE vector, VALUE index);
VALUE proc_vector_set_i(SCOPE S, VALUE vector, VALUE index, VALUE value);
VALUE proc_zero_p(SCOPE S, VALUE value);
void register_builtins(BUILTIN* builtin);
void strbuffer_append(STRBUFFER* b, wchar_t c);
void strbuffer_set(STRBUFFER* b, const wchar_t* s, size_t size);
static inline int subsequent_p(wint_t c);
static inline int to_integer(VALUE value);

void buffer_append(BUFFER* b, char c) {
        if (b->size == b->capacity) {
                if (b->capacity)
                        b->capacity *= 2;
                else
                        b->capacity = 32;
                b->elements = GC_REALLOC(b->elements, b->capacity);
        }
        b->elements[b->size++] = c;
}

void buffer_set(BUFFER* b, const char* s, size_t size) {
        if (size > b->capacity) {
                b->capacity = size;
                b->elements = GC_REALLOC(b->elements, b->capacity);
        }
        b->size = size;
        memcpy(b->elements, s, size);
}

void strbuffer_append(STRBUFFER* b, wchar_t c) {
        if (b->size == b->capacity) {
                if (b->capacity)
                        b->capacity *= 2;
                else
                        b->capacity = 32;
                b->elements = GC_REALLOC(b->elements, b->capacity * sizeof (wchar_t));
        }
        b->elements[b->size++] = c;
}

void strbuffer_set(STRBUFFER* b, const wchar_t* s, size_t size) {
        if (size > b->capacity) {
                b->capacity = size;
                b->elements = GC_REALLOC(b->elements, b->capacity * sizeof (wchar_t));
        }
        b->size = size;
        memcpy(b->elements, s, size * sizeof (wchar_t));
}

static inline wint_t look() {
        return lookahead == WEOF ? lookahead = fgetwc(in) : lookahead;
}

static inline wint_t eat() {
        wint_t c = lookahead == WEOF ? fgetwc(in) : lookahead;
        lookahead = WEOF;
        if (c != WEOF) {
                if (c == L'\n') {
                        ++line;
                        column = 0;
                } else {
                        ++column;
                }
        }
        return c;
}

static inline int delimiter_p(wint_t c) {
        switch (c) {
        case L'(': case L')': case L'[': case L']':
        case L'"': case L';': case L'#': return 1;
        }
        return iswspace(c);
}

static inline int initial_p(wint_t c) {
        switch (c) {
        case L'!': case L'$': case L'%': case L'&':
        case L'*': case L'/': case L':': case L'<':
        case L'=': case L'>': case L'?': case L'^':
        case L'_': case L'~': return 1;
        }
        return iswalpha(c);
}

static inline int subsequent_p(wint_t c) {
        switch (c) {
        case L'+': case L'-': case L'.': case L'@':
                return 1;
        }
        return initial_p(c) || iswdigit(c);
}

static inline VALUE new_value(size_t size, TYPE type) {
        VALUE value = (VALUE)GC_MALLOC(size);
        value->type = type;
        return value;
}

static inline VALUE new_atomic_value(size_t size, TYPE type) {
        VALUE value = (VALUE)GC_MALLOC_ATOMIC(size);
        value->type = type;
        return value;
}

VALUE new_vector() {
        return new_value(sizeof (VECTOR), TYPE_VECTOR);
}

VALUE new_string(const wchar_t* s, size_t size) {
        VALUE value = new_value(sizeof (STRING), TYPE_STRING);
        value->string.size = size;
        value->string.elements = GC_MALLOC_ATOMIC(size * sizeof (wchar_t));
        memcpy(value->string.elements, s, size * sizeof (wchar_t));
        return value;
}

VALUE new_bytevector(const char* s, size_t size) {
        VALUE value = new_value(sizeof (STRING), TYPE_STRING);
        value->bytevector.elements = GC_MALLOC_ATOMIC(size);
        value->bytevector.size = size;
        memcpy(value->bytevector.elements, s, size);
        return value;
}

VALUE new_pair(VALUE car, VALUE cdr) {
        VALUE value = (VALUE)GC_MALLOC(sizeof (PAIR));
        value->car = car;
        value->cdr = cdr;
        return value;
}

VALUE new_character(wchar_t character) {
        VALUE value = new_atomic_value(sizeof (CHARACTER), TYPE_CHARACTER);
        value->character.value = character;
        return value;
}

static inline VALUE new_integer(int integer) {
        return (VALUE)((((intptr_t)integer) << 2) | 1);
}

static inline int to_integer(VALUE value) {
        return (int)(((intptr_t)value) >> 2);
}

static inline TYPE get_type(VALUE value) {
        if (((intptr_t)value) & 1)
                return TYPE_INTEGER;
        return (value->type & 2) == 0 ? TYPE_PAIR : value->type;
}

VALUE new_procedure(SCOPE S, VALUE name, VALUE args, VALUE body) {
        VALUE value = new_value(sizeof (PROCEDURE), TYPE_PROCEDURE);
        value->procedure.name = name;
        value->procedure.args = args;
        value->procedure.body = body;
        value->procedure.scope = S;
        return value;
}

SCOPE new_scope(SCOPE outer) {
        SCOPE scope = (SCOPE)GC_MALLOC(sizeof (struct SCOPE));
        scope->outer = outer;
        return scope;
}

HANDLER new_handler() {
        return (HANDLER)GC_MALLOC(sizeof (struct HANDLER));
}

VALUE new_symbol(const wchar_t* name, size_t size) {
        VALUE value = new_atomic_value(sizeof (SYMBOL) + size * sizeof (wchar_t), TYPE_SYMBOL);
        value->symbol.size = size;
        memcpy(value->symbol.elements, name, size * sizeof (wchar_t));
        return value;
}

VALUE add_symbol(const wchar_t* name, size_t size) {
        VALUE s = symbols;
        if (!s) {
                symbols = s = new_symbol(name, size);
        } else {
                for (;;) {
                        int c = 0;
                        if (size > s->symbol.size)
                                c = 1;
                        else if (size < s->symbol.size)
                                c = -1;
                        else
                                c = memcmp(name, s->symbol.elements, size * sizeof (wchar_t));

                        if (!c) {
                                break;
                        } else if (c < 0) {
                                if (!s->symbol.left) {
                                        s = s->symbol.left = new_symbol(name, size);
                                        break;
                                }
                                s = s->symbol.left;
                        } else {
                                if (!s->symbol.right) {
                                        s = s->symbol.right = new_symbol(name, size);
                                        break;
                                }
                                s = s->symbol.right;
                        }
                }
        }
        return s;
}

VALUE get_symbol(const wchar_t* name) {
        return add_symbol(name, wcslen(name));
}

VARIABLE new_variable(VALUE symbol, VALUE value) {
        VARIABLE v = (VARIABLE)GC_MALLOC(sizeof (struct VARIABLE));
        v->symbol = symbol;
        v->value = value;
        return v;
}

void define_variable(SCOPE S, VALUE symbol, VALUE value) {
        ASSERT_TYPE(symbol, TYPE_SYMBOL);

        VARIABLE v = S->variables;
        if (!v) {
                S->variables = new_variable(symbol, value);
        } else {
                for (;;) {
                        if (v->symbol == symbol) {
                                v->value = value;
                                break;
                        } else if (symbol < v->symbol) {
                                if (!v->left) {
                                        v = v->left = new_variable(symbol, value);
                                        break;
                                }
                                v = v->left;
                        } else {
                                if (!v->right) {
                                        v = v->right = new_variable(symbol, value);
                                        break;
                                }
                                v = v->right;
                        }
                }
        }
}

VALUE lookup(SCOPE S, VALUE symbol) {
        SCOPE scope = S;
        while (scope) {
                VARIABLE v = scope->variables;
                while (v) {
                        if (v->symbol == symbol)
                                return v->value;
                        v = symbol < v->symbol ? v->left : v->right;
                }
                scope = scope->outer;
        }
        ASSERTION_VIOLATION(L"Unbound variable");
        return UNSPECIFIED;
}

TOKEN lex(SCOPE S) {
        wint_t c;
next:
        c = eat();
        switch (c) {
        case WEOF: return TOKEN_END;
        case L';':
                for (;;) {
                        switch (eat()) {
                        case WEOF:
                                return TOKEN_END;
                        case L'\n':
                                goto next;
                        }
                }
                break; // never reached
        case L'(':  return TOKEN_LPAREN;
        case L')':  return TOKEN_RPAREN;
        case L'[':  return TOKEN_LBRACKET;
        case L']':  return TOKEN_RBRACKET;
        case L'\'': return TOKEN_QUOTE;
        case L'`':  return TOKEN_QUASIQUOTE;
        case L',':
                if (look() == L'@') {
                        eat();
                        return TOKEN_UNQUOTE_SPLICING;
                }
                return TOKEN_UNQUOTE;
        case L'.':
                if (delimiter_p(look()))
                        return TOKEN_DOT;
                if (eat() == L'.' && eat() == L'.') {
                        strbuffer_set(&token_buffer, L"...", 3);
                        return TOKEN_IDENTIFIER;
                }
                return TOKEN_INVALID;
        case L'+':
                if (delimiter_p(look())) {
                        strbuffer_set(&token_buffer, L"+", 1);
                        return TOKEN_IDENTIFIER;
                }
                return TOKEN_INVALID;
        case L'-':
                if (delimiter_p(look())) {
                        strbuffer_set(&token_buffer, L"-", 1);
                        return TOKEN_IDENTIFIER;
                } else if (look() == L'>') {
                        eat();
                        strbuffer_set(&token_buffer, L"->", 2);
                        while (subsequent_p(look()))
                                strbuffer_append(&token_buffer, eat());
                        return TOKEN_IDENTIFIER;
                }
                return TOKEN_INVALID;
        case L'#':
                switch (eat()) {
                case L'(':  return TOKEN_VECTOR;
                case L'\'': return TOKEN_SYNTAX;
                case L'`':  return TOKEN_QUASISYNTAX;
                case L',':
                        if (look() == L'@') {
                                eat();
                                return TOKEN_UNSYNTAX_SPLICING;
                        }
                        return TOKEN_UNSYNTAX;
                case L'v':
                        if (eat() == L'u' && eat() == L'8' && eat() == L'(')
                                return TOKEN_BYTEVECTOR;
                        return TOKEN_INVALID;
                case L't': case L'T':
                        if (delimiter_p(look()))
                                return TOKEN_TRUE;
                        return TOKEN_INVALID;
                case L'f': case L'F':
                        if (delimiter_p(look()))
                                return TOKEN_FALSE;
                        return TOKEN_INVALID;
                case L'\\':
                        token_buffer.size = 0;
                        while ((token_buffer.size == 0 || !delimiter_p(look())) && look() != WEOF)
                                strbuffer_append(&token_buffer, eat());
                        if (token_buffer.size > 0) {
                                if (token_buffer.size == 1) {
                                        token_character = token_buffer.elements[0];
                                        return TOKEN_CHARACTER;
                                } else if (token_buffer.elements[0] == L'x') {
                                        // TODO HEX CHARACTER
                                } else {
                                        const wchar_t** c;
                                        for (c = named_characters; *c; ++c) {
                                                if (!wcsncmp(*c + 1, token_buffer.elements, token_buffer.size)) {
                                                        token_character = **c;
                                                        return TOKEN_CHARACTER;
                                                }
                                        }
                                }
                        }
                        return TOKEN_INVALID;
                }
                return TOKEN_INVALID;
        case L'"':
                token_buffer.size = 0;
                for (;;) {
                        c = eat();
                        if (c == WEOF) {
                                LEXICAL_VIOLATION(L"Unexpected end");
                        } else if (c == '"') {
                                break;
                        } else if (c == L'\\') {
                                switch (eat()) {
                                case L'a':  c = L'\a'; break;
                                case L'b':  c = L'\b'; break;
                                case L't':  c = L'\t'; break;
                                case L'n':  c = L'\n'; break;
                                case L'v':  c = L'\v'; break;
                                case L'f':  c = L'\f'; break;
                                case L'r':  c = L'\r'; break;
                                case L'"':  c = L'"';  break;
                                case L'\\': c = L'\\'; break;
                                default:
                                        LEXICAL_VIOLATION(L"Invalid escape sequence");
                                        break;
                                }
                        }
                        strbuffer_append(&token_buffer, c);
                }
                return TOKEN_STRING;
        default:
                if (iswspace(c)) {
                        goto next;
                } else if (initial_p(c)) {
                        token_buffer.size = 0;
                        for (;;) {
                                strbuffer_append(&token_buffer, c);
                                if (!subsequent_p(look()))
                                        break;
                                c = eat();
                        }
                        return TOKEN_IDENTIFIER;
                } else if (iswdigit(c)) {
                        token_integer = 0;
                        for (;;) {
                                token_integer *= 10;
                                token_integer += c - L'0';
                                if (!iswdigit(look()))
                                    break;
                                c = eat();
                        }
                        return TOKEN_INTEGER;
                }
                return TOKEN_INVALID;
        }
}

VALUE proc_display(SCOPE S, VALUE value) {
        size_t i;
        switch (get_type(value)) {
        case TYPE_PROCEDURE:
                wprintf(L"#<procedure ");
                proc_display(S, value->procedure.name);
                putwchar(L' ');
                proc_display(S, value->procedure.args);
                putwchar(L'>');
                break;
        case TYPE_UNSPECIFIED:
                wprintf(L"#<unspecified>");
                break;
        case TYPE_BUILTIN:
                wprintf(L"#<builtin %ls>", value->builtin.name);
                break;
        case TYPE_CONTINUATION:
                wprintf(L"#<continuation>");
                break;
        case TYPE_BYTEVECTOR:
                wprintf(L"#vu8(");
                if (value->bytevector.size > 0) {
                        wprintf(L"%d", value->bytevector.elements[0]);
                        for (i = 1; i < value->bytevector.size; ++i)
                                wprintf(L" %d", value->bytevector.elements[i]);
                }
                putwchar(L')');
                break;
        case TYPE_VECTOR:
                wprintf(L"#(");
                if (value->vector.size > 0) {
                        proc_display(S, value->vector.elements[0]);
                        for (i = 1; i < value->vector.size; ++i) {
                                putwchar(L' ');
                                proc_display(S, value->vector.elements[i]);
                        }
                }
                putwchar(L')');
                break;
        case TYPE_TRUE:
                wprintf(L"#t");
                break;
        case TYPE_FALSE:
                wprintf(L"#f");
                break;
        case TYPE_CHARACTER:
                putwchar(value->character.value);
                break;
        case TYPE_STRING:
                for (i = 0; i < value->string.size; ++i)
                        putwchar(value->string.elements[i]);
                break;
        case TYPE_EMPTY:
                wprintf(L"()");
                break;
        case TYPE_PAIR:
                putwchar(L'(');
                for (;;) {
                        proc_display(S,value->car);
                        value = value->cdr;
                        if (get_type(value) != TYPE_PAIR) {
                                if (value != EMPTY) {
                                        wprintf(L" . ");
                                        proc_display(S, value);
                                }
                                break;
                        }
                        putwchar(L' ');
                }
                putwchar(L')');
                break;
        case TYPE_SYMBOL:
                for (i = 0; i < value->symbol.size; ++i)
                        putwchar(value->symbol.elements[i]);
                break;
        case TYPE_INTEGER:
                wprintf(L"%d", to_integer(value));
                break;
        case TYPE_COMPLEX:
                break;
        case TYPE_RATIONAL:
                break;
        case TYPE_REAL:
                break;
        }
        return UNSPECIFIED;
}

VALUE parse(SCOPE S) {
        typedef struct {
                VALUE value;
                TOKEN token;
        } STACK;

        STACK* stack = 0;
        size_t depth = 0, capacity = 32;
        int dotted = 0;
        VALUE result, quote = 0;
        TOKEN token;
        BUFFER bytevector = BUFFER_INIT;

        stack = (STACK*)GC_MALLOC(sizeof (STACK) * capacity);
        stack->token = TOKEN_INVALID;
        result = stack->value = new_pair(UNSPECIFIED, EMPTY);
        ++depth;

        while ((token = lex(S)) != TOKEN_END) {
                if (depth == capacity) {
                        capacity *= 2;
                        stack = (STACK*)GC_REALLOC(stack, sizeof (STACK) * capacity);
                }

                VALUE value = 0;
                STACK* top = stack + depth - 1;

                switch (token) {
                case TOKEN_RPAREN:
                case TOKEN_RBRACKET:
                        LEXICAL_ASSERTION(L"Unexpected closing )", depth != 0 && top->token + 1 == token);
                        --depth;
                        continue;
                case TOKEN_DOT:
                        LEXICAL_ASSERTION(L"Unexpected .", get_type(top->value) == TYPE_PAIR);
                        dotted = 1;
                        continue;
                case TOKEN_VECTOR:
                        stack[depth].value = value = new_vector();
                        stack[depth].token = TOKEN_LPAREN;
                        ++depth;
                        break;
                case TOKEN_BYTEVECTOR:
                        bytevector.size = 0;
                        while ((token = lex(S)) != TOKEN_RPAREN) {
                                switch (token) {
                                case TOKEN_INTEGER:
                                        if (token_integer < 0 || token_integer > 255)
                                                LEXICAL_VIOLATION(L"Byte out of range");
                                        buffer_append(&bytevector, token_integer);
                                        break;
                                case TOKEN_END:
                                        LEXICAL_VIOLATION(L"Unexpected end");
                                        break;
                                default:
                                        LEXICAL_VIOLATION(L"Invalid token");
                                        break;
                                }
                        }
                        value = new_bytevector(bytevector.elements, bytevector.size);
                        break;
                case TOKEN_LPAREN:
                case TOKEN_LBRACKET:
                        stack[depth].value = value = new_pair(UNSPECIFIED, EMPTY);
                        stack[depth].token = token;
                        ++depth;
                        break;
                case TOKEN_IDENTIFIER:
                        value = add_symbol(token_buffer.elements, token_buffer.size);
                        break;
                case TOKEN_TRUE:
                        value = TRUE;
                        break;
                case TOKEN_FALSE:
                        value = FALSE;
                        break;
                case TOKEN_STRING:
                        value = new_string(token_buffer.elements, token_buffer.size);
                        break;
                case TOKEN_CHARACTER:
                        value = new_character(token_character);
                        break;
                case TOKEN_INTEGER:
                        value = new_integer(token_integer);
                        break;
                case TOKEN_QUOTE:             quote = get_symbol(L"quote");             continue;
                case TOKEN_QUASIQUOTE:        quote = get_symbol(L"quasiquote");        continue;
                case TOKEN_UNQUOTE:           quote = get_symbol(L"unquote");           continue;
                case TOKEN_UNQUOTE_SPLICING:  quote = get_symbol(L"unquote-splicing");  continue;
                case TOKEN_SYNTAX:            quote = get_symbol(L"syntax");            continue;
                case TOKEN_QUASISYNTAX:       quote = get_symbol(L"quasisyntax");       continue;
                case TOKEN_UNSYNTAX:          quote = get_symbol(L"unsyntax");          continue;
                case TOKEN_UNSYNTAX_SPLICING: quote = get_symbol(L"unsyntax-splicing"); continue;
                case TOKEN_INVALID:
                        LEXICAL_VIOLATION(L"Invalid token");
                        break;
                default:
                        LEXICAL_VIOLATION(L"Unknown token");
                        break;
                }

                if (quote) {
                        VALUE pair = new_pair(value, EMPTY);
                        value = new_pair(quote, pair);
                        quote = 0;
                }

                switch (get_type(top->value)) {
                case TYPE_PAIR:
                        if (top->value->car == UNSPECIFIED) {
                                top->value->car = value;
                        } else {
                                if (dotted) {
                                        LEXICAL_ASSERTION(L"CDR already set", top->value->cdr == EMPTY);
                                        top->value->cdr = value;
                                        dotted = 0;
                                } else {
                                        top->value->cdr = new_pair(value, EMPTY);
                                        top->value = top->value->cdr;
                                }
                        }
                        break;
                case TYPE_VECTOR:
                        top->value->vector.elements = (VALUE*)GC_REALLOC(top->value->vector.elements,
                                                                      sizeof (VALUE) * (top->value->vector.size + 1));
                        top->value->vector.elements[top->value->vector.size++] = value;
                        break;
                default:
                        LEXICAL_VIOLATION(L"Trying to append to invalid value");
                        break;
                }
        }
        LEXICAL_ASSERTION(L"Expected closing )", depth == 1);
        LEXICAL_ASSERTION(L"Unexpected .", !dotted);
        LEXICAL_ASSERTION(L"Unexpected quotation", !quote);
        return result;
}

VALUE eval_list(SCOPE S, VALUE list) {
        if (list == EMPTY)
                return EMPTY;

        VALUE result = new_pair(UNSPECIFIED, EMPTY);
        VALUE value = result;
        if (get_type(list) == TYPE_PAIR) {
                for (;;) {
                        value->car = eval(S, list->car);
                        list = list->cdr;

                        if (get_type(list) != TYPE_PAIR)
                                break;

                        value->cdr = new_pair(UNSPECIFIED, EMPTY);
                        value = value->cdr;
                }
        }
        ASSERTION(L"No list", list == EMPTY);
        return result;
}

VALUE call_builtin(BUILTIN* builtin, SCOPE S, VALUE args) {
        if (builtin->args < 0)
                return builtin->functionN(args, S);

        EXPECT_ARGUMENTS(builtin->args);
        switch (builtin->args) {
        case 0: return builtin->function0(S);
        case 1: return builtin->function1(S, args->car);
        case 2: return builtin->function2(S, args->car, args->cdr->car);
        case 3: return builtin->function3(S, args->car, args->cdr->car, args->cdr->cdr->car);
        default: assert(0); return UNSPECIFIED;
        }
}

VALUE apply(SCOPE S, VALUE proc, VALUE args) {
        VALUE result;
        switch (get_type(proc)) {
        case TYPE_BUILTIN:
                if (proc->builtin.flags & BUILTIN_EVAL_ARGS)
                        args = eval_list(S, args);
                if (proc->builtin.flags & BUILTIN_NEW_SCOPE)
                        S = new_scope(S);
                result = call_builtin(&proc->builtin, S, args);
                return (proc->builtin.flags & BUILTIN_EVAL_RESULT) ? eval(S, result) : result;
        case TYPE_PROCEDURE:
                return call_procedure(proc, eval_list(S, args));
        case TYPE_CONTINUATION:
                ASSERTION(L"Wrong number of arguments", get_type(args) == TYPE_PAIR && args->cdr == EMPTY);
                proc->continuation.result = args->car;
                active_continuation = &proc->continuation;
                longjmp(proc->continuation.jmp, 1);
                assert(0);
                return UNSPECIFIED;
        default:
                ASSERTION_VIOLATION(L"Wrong type");
                break;
        }
}

VALUE call_procedure(VALUE proc, VALUE arg) {
        SCOPE S;
tail_recursion:
        S = new_scope(proc->procedure.scope);

        if (arg != EMPTY) {
                VALUE name = proc->procedure.args;

                while (get_type(arg) == TYPE_PAIR && get_type(name) == TYPE_PAIR) {
                        define_variable(S, name->car, arg->car);
                        arg = arg->cdr;
                        name = name->cdr;
                }
                ASSERTION(L"Wrong number of arguments", arg == EMPTY && name == EMPTY && arg == name);
        }

        VALUE tail = macro_begin(proc->procedure.body, S);
        while (get_type(tail) == TYPE_PAIR) {
                VALUE car = eval(S, tail->car);

                switch (get_type(car)) {
                case TYPE_PROCEDURE:
                        proc = car;
                        arg = eval_list(S, tail->cdr);
                        goto tail_recursion;
                case TYPE_BUILTIN:
                        if (car->builtin.flags & BUILTIN_EVAL_RESULT) {
                                VALUE args = tail->cdr;
                                if (car->builtin.flags & BUILTIN_EVAL_ARGS)
                                        args = eval_list(S, args);
                                if (car->builtin.flags & BUILTIN_NEW_SCOPE)
                                        S = new_scope(S);
                                tail = call_builtin(&(car->builtin), S, args);
                                continue;
                        }
                        // fall through
                default:
                        return apply(S, car, tail->cdr);
                }
        }
        return eval(S, tail);
}

VALUE eval(SCOPE S, VALUE value) {
        int x;
        ASSERTION(L"Stack level too deep", ((char*)stack_start - (char*)&x) < 1024*1024);

        switch (get_type(value)) {
        case TYPE_INTEGER:
        case TYPE_TRUE:
        case TYPE_FALSE:
        case TYPE_UNSPECIFIED:
        case TYPE_STRING:
        case TYPE_CHARACTER:
        case TYPE_BYTEVECTOR:
        case TYPE_BUILTIN:
        case TYPE_CONTINUATION:
                return value;
        case TYPE_SYMBOL:
                return lookup(S, value);
        case TYPE_PAIR:
                return apply(S, eval(S, value->car), value->cdr);
        default:
                ASSERTION_VIOLATION(L"Wrong type");
                break;
        }
}

void register_builtins(BUILTIN* builtin) {
        while (builtin->type == TYPE_BUILTIN) {
                define_variable(TOP, get_symbol(builtin->name), (VALUE)builtin);
                ++builtin;
        }
}

#define CAR(spec)                                                  \
        VALUE proc_c##spec##r(SCOPE S, VALUE pair) {                    \
                int i = sizeof(#spec) - 2;                              \
                do {                                                    \
                        ASSERT_TYPE(pair, TYPE_PAIR);                   \
                        pair = #spec[i--] == 'a' ? pair->car : pair->cdr; \
                } while (i >= 0);                                       \
                return pair;                                            \
        }

CAR(a)
CAR(d)
CAR(aa)
CAR(ad)
CAR(da)
CAR(dd)
CAR(aaa)
CAR(aad)
CAR(ada)
CAR(add)
CAR(daa)
CAR(dad)
CAR(dda)
CAR(ddd)
CAR(aaaa)
CAR(aaad)
CAR(aada)
CAR(aadd)
CAR(adaa)
CAR(adad)
CAR(adda)
CAR(addd)
CAR(daaa)
CAR(daad)
CAR(dada)
CAR(dadd)
CAR(ddaa)
CAR(ddad)
CAR(ddda)
CAR(dddd)
#undef CAR

VALUE proc_newline(SCOPE S) {
        putwchar(L'\n');
        return UNSPECIFIED;
}

VALUE proc_cons(SCOPE S, VALUE car, VALUE cdr) {
        return new_pair(car, cdr);
}

VALUE proc_add(VALUE args, SCOPE S) {
        int n = 0;
        while (get_type(args) == TYPE_PAIR) {
                ASSERT_TYPE(args->car, TYPE_INTEGER);
                n += to_integer(args->car);
                args = args->cdr;
        }
        return new_integer(n);
}

VALUE proc_subtract(VALUE args, SCOPE S) {
        ASSERT_TYPE(args, TYPE_PAIR);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        if (args->cdr == EMPTY)
                return new_integer(-to_integer(args->car));
        int n = to_integer(args->car);
        args = args->cdr;
        while (get_type(args) == TYPE_PAIR) {
                ASSERT_TYPE(args->car, TYPE_INTEGER);
                n -= to_integer(args->car);
                args = args->cdr;
        }
        return new_integer(n);
}

VALUE proc_multiply(VALUE args, SCOPE S) {
        int n = 1;
        while (get_type(args) == TYPE_PAIR) {
                ASSERT_TYPE(args->car, TYPE_INTEGER);
                n *= to_integer(args->car);
                args = args->cdr;
        }
        return new_integer(n);
}

VALUE proc_divide(VALUE args, SCOPE S) {
        ASSERT_TYPE(args, TYPE_PAIR);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        if (args->cdr == EMPTY)
                return new_integer(1/to_integer(args->car));
        int n = to_integer(args->car);
        args = args->cdr;
        while (get_type(args) == TYPE_PAIR) {
                ASSERT_TYPE(args->car, TYPE_INTEGER);
                n /= to_integer(args->car);
                args = args->cdr;
        }
        return new_integer(n);
}

VALUE proc_zero_p(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_INTEGER);
        return !to_integer(value) ? TRUE : FALSE;
}

VALUE proc_positive_p(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_INTEGER);
        return to_integer(value) > 0 ? TRUE : FALSE;
}

VALUE proc_negative_p(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_INTEGER);
        return to_integer(value) < 0 ? TRUE : FALSE;
}

VALUE proc_even_p(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_INTEGER);
        return to_integer(value) % 2 == 0 ? TRUE : FALSE;
}

VALUE proc_odd_p(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_INTEGER);
        return to_integer(value) % 2 == 1 ? TRUE : FALSE;
}

VALUE proc_equal(VALUE args, SCOPE S) {
        EXPECT_ARGUMENTS(2);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        ASSERT_TYPE(args->cdr->car, TYPE_INTEGER);
        return to_integer(args->car) == to_integer(args->cdr->car) ? TRUE : FALSE;
}

VALUE proc_less(VALUE args, SCOPE S) {
        EXPECT_ARGUMENTS(2);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        ASSERT_TYPE(args->cdr->car, TYPE_INTEGER);
        return to_integer(args->car) < to_integer(args->cdr->car) ? TRUE : FALSE;
}

VALUE proc_lessequal(VALUE args, SCOPE S) {
        EXPECT_ARGUMENTS(2);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        ASSERT_TYPE(args->cdr->car, TYPE_INTEGER);
        return to_integer(args->car) <= to_integer(args->cdr->car) ? TRUE : FALSE;
}

VALUE proc_greater(VALUE args, SCOPE S) {
        EXPECT_ARGUMENTS(2);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        ASSERT_TYPE(args->cdr->car, TYPE_INTEGER);
        return to_integer(args->car) > to_integer(args->cdr->car) ? TRUE : FALSE;
}

VALUE proc_greaterequal(VALUE args, SCOPE S) {
        EXPECT_ARGUMENTS(2);
        ASSERT_TYPE(args->car, TYPE_INTEGER);
        ASSERT_TYPE(args->cdr->car, TYPE_INTEGER);
        return to_integer(args->car) >= to_integer(args->cdr->car) ? TRUE : FALSE;
}

VALUE proc_procedure_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_PROCEDURE || get_type(value) == TYPE_CONTINUATION ||
               (get_type(value) == TYPE_BUILTIN && value->builtin.flags == BUILTIN_EVAL_ARGS) ? TRUE : FALSE;
}

VALUE proc_pair_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_PAIR ? TRUE : FALSE;
}

VALUE proc_boolean_p(SCOPE S, VALUE value) {
        return value == TRUE || value == FALSE ? TRUE : FALSE;
}

VALUE proc_null_p(SCOPE S, VALUE value) {
        return value == EMPTY ? TRUE : FALSE;
}

VALUE proc_char_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_CHARACTER ? TRUE : FALSE;
}

VALUE proc_symbol_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_SYMBOL ? TRUE : FALSE;
}

VALUE proc_string_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_STRING ? TRUE : FALSE;
}

VALUE proc_vector_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_VECTOR ? TRUE : FALSE;
}

VALUE proc_integer_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_INTEGER ? TRUE : FALSE;
}

VALUE proc_rational_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_INTEGER ||
               get_type(value) == TYPE_RATIONAL ? TRUE : FALSE;
}

VALUE proc_real_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_INTEGER  ||
               get_type(value) == TYPE_RATIONAL ||
               get_type(value) == TYPE_REAL     ? TRUE : FALSE;
}

VALUE proc_number_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_INTEGER  ||
               get_type(value) == TYPE_RATIONAL ||
               get_type(value) == TYPE_REAL     ||
               get_type(value) == TYPE_COMPLEX  ? TRUE : FALSE;
}

VALUE proc_complex_p(SCOPE S, VALUE value) {
        return get_type(value) == TYPE_INTEGER  ||
               get_type(value) == TYPE_RATIONAL ||
               get_type(value) == TYPE_REAL     ||
               get_type(value) == TYPE_COMPLEX  ? TRUE : FALSE;
}

VALUE proc_symbol_to_string(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_SYMBOL);
        return new_string(value->symbol.elements, value->symbol.size);
}

VALUE proc_string_to_symbol(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_STRING);
        return add_symbol(value->string.elements, value->string.size);
}

VALUE proc_string_copy(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_STRING);
        return new_string(value->string.elements, value->string.size);
}

VALUE proc_not(SCOPE S, VALUE value) {
        return value == FALSE ? TRUE : FALSE;
}

VALUE proc_list_p(SCOPE S, VALUE list) {
        while (get_type(list) == TYPE_PAIR)
                list = list->cdr;
        return list == EMPTY ? TRUE : FALSE;
}

VALUE proc_list(VALUE args, SCOPE S) {
        return args;
}

VALUE proc_length(SCOPE S, VALUE list) {
        int length = 0;
        while (get_type(list) == TYPE_PAIR) {
                ++length;
                list = list->cdr;
        }
        ASSERTION(L"No list", list == EMPTY);
        return new_integer(length);
}

VALUE proc_callcc(SCOPE S, VALUE proc) {
        VALUE value;

        value = new_atomic_value(sizeof (CONTINUATION) + (char*)stack_start - (char*)&value, TYPE_CONTINUATION);
        value->continuation.stack_pos = &value;
        value->continuation.stack_size = (char*)stack_start - (char*)&value;

        memcpy(value->continuation.stack, value->continuation.stack_pos, value->continuation.stack_size);

        if (setjmp(value->continuation.jmp)) {
                memcpy(active_continuation->stack_pos, active_continuation->stack, active_continuation->stack_size);
                return active_continuation->result;
        }
        return apply(S, proc, new_pair(value, EMPTY));
}

VALUE proc_boolean_equal_p(VALUE args, SCOPE S) {
        return FALSE;
}

VALUE proc_symbol_equal_p(VALUE args, SCOPE S) {
        return FALSE;
}

VALUE proc_vector_length(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_VECTOR);
        return new_integer((int)value->vector.size);
}

VALUE proc_vector_ref(SCOPE S, VALUE vector, VALUE index) {
        ASSERT_TYPE(vector, TYPE_VECTOR);
        ASSERT_TYPE(index, TYPE_INTEGER);
        int i = to_integer(index);
        ASSERTION(L"Out of range", i >= 0 && i < (int)vector->vector.size);
        return vector->vector.elements[i];
}

VALUE proc_vector_set_i(SCOPE S, VALUE vector, VALUE index, VALUE value) {
        ASSERT_TYPE(vector, TYPE_VECTOR);
        ASSERT_TYPE(index, TYPE_INTEGER);
        int i = to_integer(index);
        ASSERTION(L"Out of range", i >= 0 && i < (int)vector->vector.size);
        vector->vector.elements[i] = value;
        return UNSPECIFIED;
}

VALUE proc_vector_fill_i(SCOPE S, VALUE vector, VALUE fill) {
        ASSERT_TYPE(vector, TYPE_VECTOR);
        size_t i;
        for (i = 0; i < vector->vector.size; ++i)
                vector->vector.elements[i] = fill;
        return UNSPECIFIED;
}

VALUE proc_string_length(SCOPE S, VALUE value) {
        ASSERT_TYPE(value, TYPE_STRING);
        return new_integer((int)value->string.size);
}

VALUE proc_string_ref(SCOPE S, VALUE value, VALUE index) {
        ASSERT_TYPE(value, TYPE_STRING);
        ASSERT_TYPE(index, TYPE_INTEGER);
        int i = to_integer(index);
        ASSERTION(L"Out of range", i >= 0 && i < (int)value->string.size);
        return new_character(value->string.elements[i]);
}

VALUE proc_exact_p(SCOPE S, VALUE value) {
        switch (get_type(value)) {
        case TYPE_INTEGER:
        case TYPE_RATIONAL:
                return TRUE;
        case TYPE_REAL:
                return value->real.exact ? TRUE : FALSE;
        case TYPE_COMPLEX:
                return value->komplex.exact ? TRUE : FALSE;
        default:
                ASSERTION_VIOLATION(L"Wrong type");
                break;
        }
}

VALUE proc_inexact_p(SCOPE S, VALUE value) {
        return proc_exact_p(S, value) == TRUE ? FALSE : TRUE;
}

VALUE macro_if(VALUE args, SCOPE S) {
        ASSERTION(L"Wrong number of arguments", get_type(args) == TYPE_PAIR);
        VALUE condition = args->car;
        args = args->cdr;
        ASSERTION(L"Wrong number of arguments", get_type(args) == TYPE_PAIR);
        VALUE consequent = args->car;
        args = args->cdr;
        ASSERTION(L"Wrong number of arguments", args == EMPTY || get_type(args) == TYPE_PAIR);

        VALUE alternate = UNSPECIFIED;
        if (get_type(args) == TYPE_PAIR) {
                alternate = args->car;
                args = args->cdr;
                ASSERTION(L"Wrong number of arguments", args == EMPTY);
        }

        if (eval(S, condition) != FALSE)
                return consequent;
        return alternate == UNSPECIFIED ? UNSPECIFIED : alternate;
}

VALUE macro_define(VALUE args, SCOPE S) {
        ASSERT_TYPE(args, TYPE_PAIR);
        VALUE first = args->car;
        switch (get_type(first)) {
        case TYPE_PAIR:
                define_variable(TOP, first->car, new_procedure(S, first->car, first->cdr, args->cdr));
                break;
        case TYPE_SYMBOL:
                ASSERTION(L"Wrong number of arguments", get_type(args->cdr) == TYPE_PAIR && args->cdr->cdr == EMPTY);
                define_variable(TOP, first, eval(S, args->cdr->car));
                break;
        default:
                ASSERTION_VIOLATION(L"Wrong type");
                break;
        }
        return UNSPECIFIED;
}

VALUE macro_set_i(SCOPE S, VALUE symbol, VALUE value) {
        ASSERT_TYPE(symbol, TYPE_SYMBOL);

        SCOPE scope = S;
        while (scope) {
                VARIABLE v = scope->variables;
                while (v) {
                        if (v->symbol == symbol) {
                                v->value = eval(S, value);
                                return UNSPECIFIED;
                        }
                        v = symbol < v->symbol ? v->left : v->right;
                }
                scope = scope->outer;
        }
        ASSERTION_VIOLATION(L"Variable not found");
        return UNSPECIFIED;
}

VALUE macro_lambda(VALUE args, SCOPE S) {
        ASSERT_TYPE(args, TYPE_PAIR);
        return new_procedure(S, get_symbol(L"lambda"), args->car, args->cdr);
}

VALUE macro_quote(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_quasiquote(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_unquote(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_unquote_splicing(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_syntax(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_quasisyntax(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_unsyntax(SCOPE S, VALUE value) {
        return value;
}

VALUE macro_unsyntax_splicing(SCOPE S, VALUE value) {
        return value;
}

VALUE let(SCOPE S, SCOPE eval_scope, VALUE args) {
        ASSERTION(L"Wrong number of arguments", get_type(args) == TYPE_PAIR && get_type(args->cdr) == TYPE_PAIR);
        VALUE list = args->car;
        VALUE body = args->cdr;
        ASSERTION(L"Wrong number of arguments", get_type(list) == TYPE_PAIR);

        while (get_type(list) == TYPE_PAIR) {
                VALUE pair = list->car;
                ASSERTION(L"Wrong type", get_type(pair) == TYPE_PAIR && get_type(pair->cdr) == TYPE_PAIR && pair->cdr->cdr == EMPTY);
                define_variable(S, pair->car, eval(eval_scope, pair->cdr->car));
                list = list->cdr;
        }
        ASSERTION(L"No list", list == EMPTY);

        return macro_begin(body, S);
}

VALUE macro_let(VALUE args, SCOPE S) {
        return let(S, S->outer, args);
}

VALUE macro_letstar(VALUE args, SCOPE S) {
        return let(S, S, args);
}

VALUE macro_begin(VALUE body, SCOPE S) {
        while (get_type(body) == TYPE_PAIR) {
                if (body->cdr == EMPTY)
                        return body->car;
                else
                        eval(S, body->car);
                body = body->cdr;
        }
        return UNSPECIFIED;
}

#define ARGS0 0
#define ARGS1 1
#define ARGS2 2
#define ARGS3 3
#define ARGSN -1
#define BUILTIN(name_, fn_, args_, flags_) { .type = TYPE_BUILTIN, .name = name_, .function##args_ = fn_, .args = ARGS##args_, .flags = flags_ },
#define MACRO(name, fn, args)       BUILTIN(name, macro_##fn, args, 0)
#define PROC(name, fn, args)        BUILTIN(name, proc_##fn, args, BUILTIN_EVAL_ARGS)
#define CAR(spec)              PROC(L"c"#spec"r", c##spec##r, 1)

BUILTIN core_builtins[] = {
        BUILTIN(L"begin", macro_begin, N, BUILTIN_EVAL_RESULT)
        BUILTIN(L"if", macro_if, N, BUILTIN_EVAL_RESULT)
        BUILTIN(L"let", macro_let, N, BUILTIN_EVAL_RESULT | BUILTIN_NEW_SCOPE)
        BUILTIN(L"let*", macro_letstar, N, BUILTIN_EVAL_RESULT | BUILTIN_NEW_SCOPE)
        // MACRO(L"and", and)
        // MACRO(L"case", case)
        // MACRO(L"cond", cond)
        MACRO(L"define", define, N)
        // MACRO(L"define-syntax", define_syntax)
        MACRO(L"lambda", lambda, N)
        // MACRO(L"let*-values", letstar_values)
        // MACRO(L"let-syntax", let_syntax)
        // MACRO(L"let-values", let_values)
        // MACRO(L"letrec", letrec)
        // MACRO(L"letrec*", letrecstar)
        // MACRO(L"letrec-syntax", letrec_syntax)
        // MACRO(L"or", or)
        MACRO(L"quasiquote", quasiquote, 1)
        MACRO(L"quasisyntax", quasisyntax, 1)
        MACRO(L"quote", quote, 1)
        MACRO(L"set!", set_i, 2)
        // MACRO(L"syntax", syntax)
        MACRO(L"unquote", unquote, 1)
        MACRO(L"unquote-splicing", unquote_splicing, 1)
        MACRO(L"unsyntax", unsyntax, 1)
        MACRO(L"unsyntax-splicing", unsyntax_splicing, 1)

        PROC(L"*", multiply, N)
        PROC(L"+", add, N)
        PROC(L"-", subtract, N)
        PROC(L"/", divide, N)
        PROC(L"<", less, N)
        PROC(L"<=", lessequal, N)
        PROC(L"=", equal, N)
        PROC(L">", greater, N)
        PROC(L">=", greaterequal, N)

        PROC(L"even?", even_p, 1)
        PROC(L"negative?", negative_p, 1)
        PROC(L"odd?", odd_p, 1)
        PROC(L"positive?", positive_p, 1)
        PROC(L"zero?", zero_p, 1)

        PROC(L"boolean?", boolean_p, 1)
        PROC(L"char?", char_p, 1)
        PROC(L"null?", null_p, 1)
        PROC(L"number?", number_p, 1)
        PROC(L"pair?", pair_p, 1)
        PROC(L"procedure?", procedure_p, 1)
        PROC(L"string?", string_p, 1)
        PROC(L"symbol?", symbol_p, 1)
        PROC(L"vector?", vector_p, 1)
        PROC(L"complex?", complex_p, 1)
        PROC(L"real?", real_p, 1)
        PROC(L"rational?", rational_p, 1)
        PROC(L"integer?", integer_p, 1)

        CAR(a)
        CAR(d)
        CAR(aa)
        CAR(ad)
        CAR(da)
        CAR(dd)
        CAR(aaa)
        CAR(aad)
        CAR(ada)
        CAR(add)
        CAR(daa)
        CAR(dad)
        CAR(dda)
        CAR(ddd)
        CAR(aaaa)
        CAR(aaad)
        CAR(aada)
        CAR(aadd)
        CAR(adaa)
        CAR(adad)
        CAR(adda)
        CAR(addd)
        CAR(daaa)
        CAR(daad)
        CAR(dada)
        CAR(dadd)
        CAR(ddaa)
        CAR(ddad)
        CAR(ddda)
        CAR(dddd)

        PROC(L"call-with-current-continuation", callcc, 1)
        PROC(L"call/cc", callcc, 1)

        PROC(L"cons", cons, 2)

        PROC(L"symbol->string", symbol_to_string, 1)
        PROC(L"string->symbol", string_to_symbol, 1)
        PROC(L"string-copy", string_copy, 1)

        PROC(L"not", not, 1)

        PROC(L"list?", list_p, 1)
        PROC(L"list", list, N)
        PROC(L"length", length, 1)
        // PROC(L"reverse", reverse)

        PROC(L"boolean=?", boolean_equal_p, N)
        PROC(L"symbol=?", symbol_equal_p, N)

        // PROC(L"char->integer", char_to_integer)
        // PROC(L"char<=?", char_lessequal_p)
        // PROC(L"char<?", char_less_p)
        // PROC(L"char=?", char_equal_p)
        // PROC(L"char>=?", char_greaterequal_p)
        // PROC(L"char>?", char_greater_p)

        // PROC(L"vector", vector)
        // PROC(L"vector->list", vector_to_list)
        PROC(L"vector-fill!", vector_fill_i, 2)
        // PROC(L"vector-for-each", vector_for_each)
        PROC(L"vector-length", vector_length, 1)
        // PROC(L"vector-map", vector_map)
        PROC(L"vector-ref", vector_ref, 2)
        PROC(L"vector-set!", vector_set_i, 3)

        PROC(L"string-length", string_length, 1)
        PROC(L"string-ref", string_ref, 2)

        PROC(L"exact?", exact_p, 1)
        PROC(L"inexact?", inexact_p, 1)

        // PROC(L"abs", abs)
        // PROC(L"acos", acos)
        // PROC(L"angle", angle)
        // PROC(L"append", append)
        // PROC(L"apply", apply)
        // PROC(L"asin", asin)
        // PROC(L"assert", assert)
        // PROC(L"atan", atan)
        // PROC(L"call-with-values", call_with_values)
        // PROC(L"ceiling", ceiling)
        // PROC(L"cos", cos)
        // PROC(L"denominator", denominator)
        // PROC(L"div", div)
        // PROC(L"div-and-mod", div_and_mod)
        // PROC(L"div0", div0)
        // PROC(L"div0-and-mod0", div0_and_mod0)
        // PROC(L"dynamic-wind", dynamic_wind)
        // PROC(L"eq?", eq_p)
        // PROC(L"equal?", equal_p)
        // PROC(L"eqv?", eqv_p)
        // PROC(L"exact", exact)
        // PROC(L"exact-integer-sqrt", exact_integer_sqrt)
        // PROC(L"exp", exp)
        // PROC(L"expt", expt)
        // PROC(L"finite?", finite_p)
        // PROC(L"for-each", for_each)
        // PROC(L"gcd", gcd)
        // PROC(L"imag-part", imag_part)
        // PROC(L"inexact", inexact)
        // PROC(L"infinite?", infinite_p)
        // PROC(L"integer->char", integer_to_char)
        // PROC(L"integer-valued?", integer_valued_p)
        // PROC(L"lcm", lcm)
        // PROC(L"list->string", list_to_string)
        // PROC(L"list->vector", list_to_vector)
        // PROC(L"list-ref", list_ref)
        // PROC(L"list-tail", list_tail)
        // PROC(L"log", log)
        // PROC(L"magnitude", magnitude)
        // PROC(L"make-polar", make_polar)
        // PROC(L"make-rectangular", make_rectangular)
        // PROC(L"make-string", make_string)
        // PROC(L"make-vector", make_vector)
        // PROC(L"map", map)
        // PROC(L"max", max)
        // PROC(L"min", min)
        // PROC(L"mod", mod)
        // PROC(L"mod0", mod0)
        // PROC(L"nan?", nan_p)
        // PROC(L"number->string", number_to_string)
        // PROC(L"numerator", numerator)
        // PROC(L"rational-valued?", rational_valued_p)
        // PROC(L"rationalize", rationalize)
        // PROC(L"real-part", real_part)
        // PROC(L"real-valued?", real_valued_p)
        // PROC(L"round", round)
        // PROC(L"sin", sin)
        // PROC(L"sqrt", sqrt)
        // PROC(L"string", string)
        // PROC(L"string->list", string_to_list)
        // PROC(L"string->number", string_to_number)
        // PROC(L"string-append", string_append)
        // PROC(L"string-for-each", string_for_each)
        // PROC(L"string<=?", string_lessequal_p)
        // PROC(L"string<?", string_less_p)
        // PROC(L"string=?", string_equal_p)
        // PROC(L"string>=?", string_greaterequal_p)
        // PROC(L"string>?", string_greater_p)
        // PROC(L"substring", substring)
        // PROC(L"tan", tan)
        // PROC(L"truncate", truncate)
        // PROC(L"values", values)

        /** from (rnrs io simple (6)) **/
        PROC(L"display", display, 1)
        PROC(L"newline", newline, 0)

        { .type = 0 }
};

#undef PROC
#undef BUILTIN
#undef MACRO
#undef ARGS0
#undef ARGS1
#undef ARGS2
#undef ARGS3
#undef ARGSN

int main(int argc, char* argv[]) {
        if (argc == 2) {
                in = fopen(argv[1], "r");
                if (!in) {
                        fwprintf(stderr, L"Could not read file %s\n", argv[1]);
                        return 1;
                }
        } else {
                in = stdin;
        }

        TOP = new_scope(0);

        TOP->handler = new_handler();
        if (setjmp(TOP->handler->jmp)) {
                wprintf(L"Terminating\n");
                return 1;
        }

        register_builtins(core_builtins);

        VALUE forms = parse(TOP);
        stack_start = &forms;

        while (get_type(forms) == TYPE_PAIR) {
                eval(TOP, forms->car);
                forms = forms->cdr;
        }
        return 0;
}
