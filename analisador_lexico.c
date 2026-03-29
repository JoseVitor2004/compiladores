#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <limits.h>

#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))

#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0; \
                            int _qy_ ## name ## _max = 0
#define da_rewind(name)     _qy_ ## name ## _p = 0
#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)
#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)
#define da_len(name)        _qy_ ## name ## _p

typedef enum {
    tk_ID = 256, tk_NUM, tk_RELOP, tk_LOGOP,
    tk_BOOL, tk_INT, tk_FLOAT, tk_IF, tk_FOR, 
    tk_INC, tk_DEC, tk_EOI
} TokenName;

typedef enum { attr_LT, attr_LE, attr_EQ, attr_GT, attr_GE } RelopAttr;
typedef enum { attr_AND, attr_OR } LogopAttr;

typedef struct {
    int name;
    int line, col;
    union {
        int attr_val;
        char *lexeme;
    };
} Token;

static FILE *source_fp, *dest_fp;
static int line = 1, col = 0, the_ch = ' ';
da_dim(text, char);

static void error(int err_line, int err_col, const char *fmt, ... ) {
    char buf[1000];
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    printf("(%d,%d) erro: %s\n", err_line, err_col, buf);
    exit(1);
}

static int next_ch(void) {
    the_ch = getc(source_fp);
    ++col;
    if (the_ch == '\n') {
        ++line;
        col = 0;
    }
    return the_ch;
}

static int kwd_cmp(const void *p1, const void *p2) {
    return strcmp(*(char **)p1, *(char **)p2);
}

static int get_ident_type(const char *ident) {
    static struct {
        const char *s;
        int sym;
    } kwds[] = {
        {"bool",  tk_BOOL},
        {"float", tk_FLOAT},
        {"for",   tk_FOR},
        {"if",    tk_IF},
        {"int",   tk_INT},
    }, *kwp;

    return (kwp = bsearch(&ident, kwds, NELEMS(kwds), sizeof(kwds[0]), kwd_cmp)) == NULL ? tk_ID : kwp->sym;
}

static Token ident_or_int(int err_line, int err_col) {
    int n, is_number = true;

    da_rewind(text);
    while (isalnum(the_ch) || the_ch == '_') {
        da_append(text, (char)the_ch);
        if (!isdigit(the_ch)) is_number = false;
        next_ch();
    }
    if (da_len(text) == 0)
        error(err_line, err_col, "Caractere nao reconhecido: '%c'", the_ch);
    da_append(text, '\0');
    
    if (isdigit(text[0])) {
        if (!is_number) error(err_line, err_col, "Numero invalido: %s", text);
        n = strtol(text, NULL, 0);
        return (Token){tk_NUM, err_line, err_col, {.attr_val = n}};
    }
    
    int type = get_ident_type(text);
    if (type == tk_ID) {
        return (Token){tk_ID, err_line, err_col, {.lexeme = strdup(text)}};
    } else {
        return (Token){type, err_line, err_col, {.attr_val = 0}};
    }
}

static Token relop_follow(int expect, RelopAttr ifyes, RelopAttr ifno, int err_line, int err_col) {
    if (the_ch == expect) {
        next_ch();
        return (Token){tk_RELOP, err_line, err_col, {.attr_val = ifyes}};
    }
    return (Token){tk_RELOP, err_line, err_col, {.attr_val = ifno}};
}

Token gettok(void) {
    while (isspace(the_ch)) next_ch();
    
    int err_line = line;
    int err_col  = col;
    
    switch (the_ch) {
        case '<': next_ch(); return relop_follow('=', attr_LE, attr_LT, err_line, err_col);
        case '>': next_ch(); return relop_follow('=', attr_GE, attr_GT, err_line, err_col);
        
        /* == ou atribuição = */
        case '=': 
            next_ch(); 
            if (the_ch == '=') {
                next_ch();
                return (Token){tk_RELOP, err_line, err_col, {.attr_val = attr_EQ}};
            }
            return (Token){'=', err_line, err_col, {.attr_val = 0}};

        /* Incremento ++ e Soma + */
        case '+':
            next_ch();
            if (the_ch == '+') {
                next_ch();
                return (Token){tk_INC, err_line, err_col, {.attr_val = 0}};
            }
            return (Token){'+', err_line, err_col, {.attr_val = 0}};

        /* Decremento -- e Subtração - */
        case '-':
            next_ch();
            if (the_ch == '-') {
                next_ch();
                return (Token){tk_DEC, err_line, err_col, {.attr_val = 0}};
            }
            return (Token){'-', err_line, err_col, {.attr_val = 0}};

        /* Operadores Lógicos && e || */
        case '&':
            next_ch();
            if (the_ch == '&') {
                next_ch();
                return (Token){tk_LOGOP, err_line, err_col, {.attr_val = attr_AND}};
            }
            error(err_line, err_col, "Esperado '&' apos '&'");
        case '|':
            next_ch();
            if (the_ch == '|') {
                next_ch();
                return (Token){tk_LOGOP, err_line, err_col, {.attr_val = attr_OR}};
            }
            error(err_line, err_col, "Esperado '|' apos '|'");

        /* Caracteres Simples (Matemática básica, Not e delimitadores comuns) */
        case '*': case '/': case '!': 
        case '(': case ')': case '{': case '}': case ';':
            int current_char = the_ch;
            next_ch();
            return (Token){current_char, err_line, err_col, {.attr_val = 0}};
            
        case EOF: return (Token){tk_EOI, err_line, err_col, {.attr_val = 0}};
        
        default:  return ident_or_int(err_line, err_col);
    }
}

void run(void) {
    Token t;
    do {
        t = gettok();
        if (t.name == tk_EOI) break;
        
        fprintf(dest_fp, "L%02d:C%02d | ", t.line, t.col);
        
        if (t.name == tk_ID) {
            fprintf(dest_fp, "<ID, \"%s\">\n", t.lexeme);
            free(t.lexeme);
        } 
        else if (t.name == tk_NUM)   fprintf(dest_fp, "<NUM, %d>\n", t.attr_val);
        else if (t.name == tk_RELOP) {
            const char *relop_names[] = {"LT", "LE", "EQ", "GT", "GE"};
            fprintf(dest_fp, "<RELOP, %s>\n", relop_names[t.attr_val]);
        } 
        else if (t.name == tk_LOGOP) {
            fprintf(dest_fp, "<LOGOP, %s>\n", t.attr_val == attr_AND ? "AND" : "OR");
        }
        else if (t.name == tk_INC)   fprintf(dest_fp, "<INC, ->\n");
        else if (t.name == tk_DEC)   fprintf(dest_fp, "<DEC, ->\n");
        else if (t.name == tk_BOOL)  fprintf(dest_fp, "<BOOL, ->\n");
        else if (t.name == tk_INT)   fprintf(dest_fp, "<INT, ->\n");
        else if (t.name == tk_FLOAT) fprintf(dest_fp, "<FLOAT, ->\n");
        else if (t.name == tk_IF)    fprintf(dest_fp, "<IF, ->\n");
        else if (t.name == tk_FOR)   fprintf(dest_fp, "<FOR, ->\n");
        else {
            fprintf(dest_fp, "<'%c', ->\n", (char)t.name);
        }
    } while (1);
    
    if (dest_fp != stdout) fclose(dest_fp);
}

void init_io(FILE **fp, FILE *std, const char mode[], const char fn[]) {
    if (fn[0] == '\0') *fp = std;
    else if ((*fp = fopen(fn, mode)) == NULL) error(0, 0, "Can't open %s\n", fn);
}

int main(int argc, char *argv[]) {
    init_io(&source_fp, stdin, "r", argc > 1 ? argv[1] : "");
    init_io(&dest_fp, stdout, "wb", argc > 2 ? argv[2] : "");
    run();
    return 0;
}