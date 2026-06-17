#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <errno.h>
#include <limits.h>
#define TAMANHO_BUFFER 4096

typedef enum {
    tk_FimArquivo,
    tk_Mult, tk_Div, tk_Mod, tk_Soma, tk_Sub,
    tk_Inc, tk_Dec,
    tk_MenosUnario, tk_Nao,
    tk_Menor, tk_MenorIgual,
    tk_Maior, tk_MaiorIgual,
    tk_Igual, tk_Diferente,
    tk_Atribuicao,
    tk_E, tk_Ou,
    tk_If, tk_For, tk_Int, tk_Float, tk_Bool,
    tk_AbreChave, tk_FechaChave,
    tk_AbreParen, tk_FechaParen,
    tk_PontoVirgula, tk_Virgula,
    tk_Identificador,
    tk_Inteiro,
    tk_TextoString
} TipoToken;

static const char *nome_token[] = {
    "Fim_Arquivo",
    "Op_Multiplicacao", "Op_Divisao", "Op_Resto", "Op_Soma", "Op_Subtracao",
    "Op_Incremento", "Op_Decremento",
    "Op_MenosUnario", "Op_NaoLogico",
    "Op_Menor", "Op_MenorIgual",
    "Op_Maior", "Op_MaiorIgual",
    "Op_Igual", "Op_Diferente",
    "Op_Atribuicao",
    "Op_E_Logico", "Op_Ou_Logico",
    "Palavra_if", "Palavra_for", "Palavra_int", "Palavra_float", "Palavra_bool",
    "AbreChave", "FechaChave",
    "AbreParenteses", "FechaParenteses",
    "PontoVirgula", "Virgula",
    "Identificador", "NumeroInteiro", "TextoString"
};

typedef struct token {
    TipoToken tipo;
    int linha, coluna;
    union {
        long  valor_inteiro;
        char *valor_texto;
    } valor;
} Token;

typedef enum {
    ESTADO_INICIO,
    ESTADO_IDENTIFICADOR,
    ESTADO_INTEIRO,
    ESTADO_STRING,
    ESTADO_CHAR_CORPO,
    ESTADO_CHAR_ESC,
    ESTADO_CHAR_FIM,
    ESTADO_BARRA,
    ESTADO_CMT_CORPO,
    ESTADO_CMT_ASTERISCO,
    ESTADO_MENOR,
    ESTADO_MAIOR,
    ESTADO_SINAL_IGUAL,
    ESTADO_EXCLAMACAO,
    ESTADO_E_COMERCIAL,
    ESTADO_PIPE,
    ESTADO_MAIS,
    ESTADO_MENOS
} EstadoLexico;

typedef struct {
    FILE *arquivo_fonte;
    char buffer[TAMANHO_BUFFER];
    char *pivo;
    char *batedor;
    char *limite_buffer;
    int caractere_atual;
    int linha, coluna;
} AnalisadorLexico;

typedef struct { const char *palavra; TipoToken tipo; } PalavraChave;

static const PalavraChave tabela_palavras[] = {
    {"bool", tk_Bool },
    {"float", tk_Float},
    {"for", tk_For  },
    {"if", tk_If   },
    {"int", tk_Int  }
};
#define QTD_PALAVRAS (int)(sizeof tabela_palavras / sizeof tabela_palavras[0])

static int comparar_palavras(const void *a, const void *b) {
    return strcmp((const char *)a, ((const PalavraChave *)b)->palavra);
}

static void erro_lexico(int linha, int col, const char *msg) {
    fprintf(stderr, "(%d,%d) Erro Lexico: %s\n", linha, col, msg);
}

static void avancar_caractere(AnalisadorLexico *lex) {
    if (lex->caractere_atual == '\n') { lex->linha++; lex->coluna = 0; }
    else if (lex->caractere_atual != EOF) { lex->coluna++; }
    lex->batedor++;
    if (lex->batedor >= lex->limite_buffer) {
        size_t nao_lidos = lex->limite_buffer - lex->pivo;
        if (nao_lidos >= TAMANHO_BUFFER)
            erro_lexico(lex->linha, lex->coluna, "Token maior que a capacidade do buffer");
        if (nao_lidos > 0) memmove(lex->buffer, lex->pivo, nao_lidos);
        lex->pivo    = lex->buffer;
        lex->batedor = lex->buffer + nao_lidos;
        size_t bytes_lidos = fread(lex->batedor, 1, TAMANHO_BUFFER - nao_lidos, lex->arquivo_fonte);
        lex->limite_buffer = lex->batedor + bytes_lidos;
        if (bytes_lidos == 0) { lex->caractere_atual = EOF; return; }
    }
    lex->caractere_atual = *(lex->batedor);
}

static char *extrair_lexema(AnalisadorLexico *lex) {
    size_t tamanho = lex->batedor - lex->pivo;
    char *texto = malloc(tamanho + 1);
    if (texto) { memcpy(texto, lex->pivo, tamanho); texto[tamanho] = '\0'; }
    return texto;
}

Token proximo_token(AnalisadorLexico *lex) {
    int erro_linha, erro_coluna, c;
    EstadoLexico estado;
    Token token_atual;
    char char_val = 0, esc;
    char msg[80];
reiniciar:
    while (lex->caractere_atual != EOF && isspace(lex->caractere_atual))
        avancar_caractere(lex);
    lex->pivo = lex->batedor;
    erro_linha = lex->linha;
    erro_coluna   = lex->coluna;
    estado        = ESTADO_INICIO;
    for (;;) {
        c = lex->caractere_atual;
        switch (estado) {
        case ESTADO_INICIO:
            if (c == EOF) {
                token_atual.tipo = tk_FimArquivo; token_atual.linha = erro_linha;
                token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
                return token_atual;
            }
            if (isalpha(c) || c == '_') { estado = ESTADO_IDENTIFICADOR; continue; }
            if (isdigit(c)) { estado = ESTADO_INTEIRO; continue; }
            switch (c) {
            case '"':  avancar_caractere(lex); estado = ESTADO_STRING; continue;
            case '\'': avancar_caractere(lex); estado = ESTADO_CHAR_CORPO; continue;
            case '/':  avancar_caractere(lex); estado = ESTADO_BARRA; continue;
            case '<':  avancar_caractere(lex); estado = ESTADO_MENOR; continue;
            case '>':  avancar_caractere(lex); estado = ESTADO_MAIOR; continue;
            case '=':  avancar_caractere(lex); estado = ESTADO_SINAL_IGUAL; continue;
            case '!':  avancar_caractere(lex); estado = ESTADO_EXCLAMACAO; continue;
            case '&':  avancar_caractere(lex); estado = ESTADO_E_COMERCIAL; continue;
            case '|':  avancar_caractere(lex); estado = ESTADO_PIPE; continue;
            case '+':  avancar_caractere(lex); estado = ESTADO_MAIS; continue;
            case '-':  avancar_caractere(lex); estado = ESTADO_MENOS; continue;
            case '*': avancar_caractere(lex); token_atual.tipo=tk_Mult; token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case '%': avancar_caractere(lex); token_atual.tipo=tk_Mod;  token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case '{': avancar_caractere(lex); token_atual.tipo=tk_AbreChave;  token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case '}': avancar_caractere(lex); token_atual.tipo=tk_FechaChave; token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case '(': avancar_caractere(lex); token_atual.tipo=tk_AbreParen;  token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case ')': avancar_caractere(lex); token_atual.tipo=tk_FechaParen; token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case ';': avancar_caractere(lex); token_atual.tipo=tk_PontoVirgula; token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            case ',': avancar_caractere(lex); token_atual.tipo=tk_Virgula;    token_atual.linha=erro_linha; token_atual.coluna=erro_coluna; return token_atual;
            default:
                snprintf(msg, sizeof msg, "Caractere nao reconhecido '%c' (codigo %d)", c, c);
                erro_lexico(erro_linha, erro_coluna, msg);
                avancar_caractere(lex); goto reiniciar;
            }
            break;
        case ESTADO_MAIS:
            if (c == '+') { avancar_caractere(lex); token_atual.tipo = tk_Inc; }
            else { token_atual.tipo = tk_Soma; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna;
            return token_atual;
        case ESTADO_MENOS:
            if (c == '-') { avancar_caractere(lex); token_atual.tipo = tk_Dec; }
            else { token_atual.tipo = tk_Sub; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna;
            return token_atual;
        case ESTADO_IDENTIFICADOR:
            if (isalnum(c) || c == '_') { avancar_caractere(lex); }
            else {
                char *t = extrair_lexema(lex);
                const PalavraChave *kw = bsearch(t, tabela_palavras, QTD_PALAVRAS, sizeof tabela_palavras[0], comparar_palavras);
                token_atual.linha = erro_linha; token_atual.coluna = erro_coluna;
                token_atual.valor.valor_inteiro = 0;
                if (kw) { token_atual.tipo = kw->tipo; free(t); }
                else { token_atual.tipo = tk_Identificador; token_atual.valor.valor_texto = t; }
                return token_atual;
            }
            continue;
        case ESTADO_INTEIRO:
            if (isdigit(c)) { avancar_caractere(lex); }
            else if (isalpha(c) || c == '_') {
                erro_lexico(erro_linha, erro_coluna, "Numero invalido: letra apos digito");
                avancar_caractere(lex); goto reiniciar;
            } else {
                char *t = extrair_lexema(lex);
                errno = 0;
                long v = strtol(t, NULL, 10);
                free(t);
                if (errno == ERANGE) erro_lexico(erro_linha, erro_coluna, "Estouro de limite de numero inteiro");
                token_atual.tipo = tk_Inteiro; token_atual.linha = erro_linha;
                token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = v;
                return token_atual;
            }
            continue;
        case ESTADO_STRING:
            if (c == '"') {
                avancar_caractere(lex);
                size_t sz = lex->batedor - lex->pivo - 2;
                char *t = malloc(sz + 1);
                memcpy(t, lex->pivo + 1, sz); t[sz] = '\0';
                token_atual.tipo = tk_TextoString; token_atual.linha = erro_linha;
                token_atual.coluna = erro_coluna; token_atual.valor.valor_texto = t;
                return token_atual;
            }
            if (c == '\n' || c == EOF) {
                erro_lexico(erro_linha, erro_coluna, "String nao foi fechada com aspas");
                goto reiniciar;
            }
            avancar_caractere(lex); continue;
        case ESTADO_CHAR_CORPO:
            if (c == '\'') erro_lexico(erro_linha, erro_coluna, "Constante de caractere vazia");
            if (c == '\\') { avancar_caractere(lex); estado = ESTADO_CHAR_ESC; }
            else           { char_val = c; avancar_caractere(lex); estado = ESTADO_CHAR_FIM; }
            continue;
        case ESTADO_CHAR_ESC:
            switch (c) {
            case 'n':  esc = '\n'; break;
            case '\\': esc = '\\'; break;
            default:
                snprintf(msg, sizeof msg, "Sequencia de escape desconhecida '\\%c'", c);
                erro_lexico(erro_linha, erro_coluna, msg); esc = 0;
            }
            char_val = esc; avancar_caractere(lex); estado = ESTADO_CHAR_FIM; continue;
        case ESTADO_CHAR_FIM:
            if (c != '\'') erro_lexico(erro_linha, erro_coluna, "Constante contem multiplos caracteres");
            avancar_caractere(lex);
            token_atual.tipo = tk_Inteiro; token_atual.linha = erro_linha;
            token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = (unsigned char)char_val;
            return token_atual;
        case ESTADO_BARRA:
            if (c != '*') {
                token_atual.tipo = tk_Div; token_atual.linha = erro_linha;
                token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
                return token_atual;
            }
            avancar_caractere(lex); estado = ESTADO_CMT_CORPO; continue;
        case ESTADO_CMT_CORPO:
            if (c == EOF) erro_lexico(erro_linha, erro_coluna, "Fim de arquivo dentro do comentario");
            if (c == '*') { avancar_caractere(lex); estado = ESTADO_CMT_ASTERISCO; }
            else avancar_caractere(lex);
            continue;
        case ESTADO_CMT_ASTERISCO:
            if (c == '/') { avancar_caractere(lex); goto reiniciar; }
            if (c == '*') avancar_caractere(lex);
            else { avancar_caractere(lex); estado = ESTADO_CMT_CORPO; }
            continue;
        case ESTADO_MENOR:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_MenorIgual; }
            else { token_atual.tipo = tk_Menor; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_MAIOR:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_MaiorIgual; }
            else { token_atual.tipo = tk_Maior; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_SINAL_IGUAL:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_Igual; }
            else { token_atual.tipo = tk_Atribuicao; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_EXCLAMACAO:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_Diferente; }
            else { token_atual.tipo = tk_Nao; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_E_COMERCIAL:
            if (c != '&') erro_lexico(erro_linha, erro_coluna, "Esperado '&&'");
            avancar_caractere(lex);
            token_atual.tipo = tk_E; token_atual.linha = erro_linha;
            token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_PIPE:
            if (c != '|') erro_lexico(erro_linha, erro_coluna, "Esperado '||'");
            avancar_caractere(lex);
            token_atual.tipo = tk_Ou; token_atual.linha = erro_linha;
            token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        }
    }
}

typedef enum {
    AST_INTEIRO,        
    AST_STRING,        
    AST_IDENT,          
    AST_BINOP,          
    AST_UNOP,           
    AST_POS_INC,        
    AST_POS_DEC,        
    AST_DECLARACAO,     
    AST_ATRIBUICAO,     
    AST_IF,             
    AST_FOR,            
    AST_BLOCO,          
    AST_PROGRAMA        
} TipoNo;

typedef struct No {
    TipoNo tipo;
    int linha, coluna;
    TipoToken op;
    union {
        long  valor_inteiro;
        char *valor_texto;      
    } valor;
    struct No **filhos;
    int num_filhos;
    TipoToken tipo_decl;        
} No;

static No *novo_no(TipoNo tipo, int linha, int coluna) {
    No *n = calloc(1, sizeof *n);
    n->tipo = tipo;
    n->linha = linha;
    n->coluna = coluna;
    return n;
}

static void adicionar_filho(No *pai, No *filho) {
    pai->filhos = realloc(pai->filhos, (pai->num_filhos + 1) * sizeof(No *));
    pai->filhos[pai->num_filhos++] = filho;
}

static void liberar_no(No *n) {
    if (!n) return;
    for (int i = 0; i < n->num_filhos; i++) liberar_no(n->filhos[i]);
    free(n->filhos);
    if (n->tipo == AST_IDENT || n->tipo == AST_STRING || n->tipo == AST_DECLARACAO || n->tipo == AST_ATRIBUICAO || n->tipo == AST_POS_INC    || n->tipo == AST_POS_DEC)
        free(n->valor.valor_texto);
    free(n);
}

typedef struct {
    AnalisadorLexico *lex;
    Token atual;           
    int erros;           
} Parser;

static No *parse_stmt(Parser *p);
static No *parse_expr(Parser *p);

static void erro_sintatico(Parser *p, const char *msg) {
    fprintf(stderr, "(%d,%d) Erro Sintatico: %s (encontrado: %s)\n", p->atual.linha, p->atual.coluna, msg, nome_token[p->atual.tipo]);
    p->erros++;
}

static void avancar(Parser *p) {
    (void)(p->atual.tipo == tk_Identificador || p->atual.tipo == tk_TextoString);
    p->atual = proximo_token(p->lex);
}

static bool consumir(Parser *p, TipoToken esperado) {
    if (p->atual.tipo == esperado) {
        avancar(p);
        return true;
    }
    char msg[120];
    snprintf(msg, sizeof msg, "esperado '%s'", nome_token[esperado]);
    erro_sintatico(p, msg);
    return false;
}

static void sincronizar(Parser *p, const TipoToken *conjunto, int n) {
    while (p->atual.tipo != tk_FimArquivo) {
        for (int i = 0; i < n; i++)
            if (p->atual.tipo == conjunto[i]) return;
        if (p->atual.tipo == tk_Identificador || p->atual.tipo == tk_TextoString)
            free(p->atual.valor.valor_texto);
        avancar(p);
    }
}

static No *parse_primario(Parser *p) {
    Token t = p->atual;

    if (t.tipo == tk_Inteiro) {
        No *n = novo_no(AST_INTEIRO, t.linha, t.coluna);
        n->valor.valor_inteiro = t.valor.valor_inteiro;
        avancar(p);
        return n;
    }
    if (t.tipo == tk_TextoString) {
        No *n = novo_no(AST_STRING, t.linha, t.coluna);
        n->valor.valor_texto = t.valor.valor_texto; 
        avancar(p);
        return n;
    }
    if (t.tipo == tk_Identificador) {
        No *n = novo_no(AST_IDENT, t.linha, t.coluna);
        n->valor.valor_texto = t.valor.valor_texto; 
        avancar(p);
        if (p->atual.tipo == tk_Inc || p->atual.tipo == tk_Dec) {
            TipoNo tnop = (p->atual.tipo == tk_Inc) ? AST_POS_INC : AST_POS_DEC;
            No *postfix = novo_no(tnop, p->atual.linha, p->atual.coluna);
            postfix->valor.valor_texto = strdup(n->valor.valor_texto);
            liberar_no(n);
            avancar(p);
            return postfix;
        }
        return n;
    }
    if (t.tipo == tk_AbreParen) {
        avancar(p);
        No *e = parse_expr(p);
        static const TipoToken sync_paren[] = { tk_FechaParen, tk_PontoVirgula, tk_FechaChave };
        if (p->atual.tipo != tk_FechaParen) {
            erro_sintatico(p, "esperado ')' para fechar expressao");
            sincronizar(p, sync_paren, 3);
        }
        if (p->atual.tipo == tk_FechaParen) avancar(p);
        return e;
    }

    erro_sintatico(p, "expressao invalida: token inesperado");
    No *dummy = novo_no(AST_INTEIRO, t.linha, t.coluna);
    dummy->valor.valor_inteiro = 0;
    if (t.tipo == tk_Identificador || t.tipo == tk_TextoString)
        free(t.valor.valor_texto);
    avancar(p);
    return dummy;
}

static No *parse_unario(Parser *p) {
    Token t = p->atual;
    if (t.tipo == tk_Sub || t.tipo == tk_Nao) {
        TipoToken op = t.tipo;
        avancar(p);
        No *filho = parse_unario(p);
        No *n = novo_no(AST_UNOP, t.linha, t.coluna);
        n->op = op;
        adicionar_filho(n, filho);
        return n;
    }
    return parse_primario(p);
}

static No *parse_multiplicativo(Parser *p) {
    No *esq = parse_unario(p);
    while (p->atual.tipo == tk_Mult || p->atual.tipo == tk_Div  || p->atual.tipo == tk_Mod) {
        Token t = p->atual;
        avancar(p);
        No *dir = parse_unario(p);
        No *n = novo_no(AST_BINOP, t.linha, t.coluna);
        n->op = t.tipo;
        adicionar_filho(n, esq);
        adicionar_filho(n, dir);
        esq = n;
    }
    return esq;
}

static No *parse_aditivo(Parser *p) {
    No *esq = parse_multiplicativo(p);
    while (p->atual.tipo == tk_Soma || p->atual.tipo == tk_Sub) {
        Token t = p->atual;
        avancar(p);
        No *dir = parse_multiplicativo(p);
        No *n = novo_no(AST_BINOP, t.linha, t.coluna);
        n->op = t.tipo;
        adicionar_filho(n, esq);
        adicionar_filho(n, dir);
        esq = n;
    }
    return esq;
}

static No *parse_relacional(Parser *p) {
    No *esq = parse_aditivo(p);
    while (p->atual.tipo == tk_Menor || p->atual.tipo == tk_MenorIgual || p->atual.tipo == tk_Maior || p->atual.tipo == tk_MaiorIgual) {
        Token t = p->atual;
        avancar(p);
        No *dir = parse_aditivo(p);
        No *n = novo_no(AST_BINOP, t.linha, t.coluna);
        n->op = t.tipo;
        adicionar_filho(n, esq);
        adicionar_filho(n, dir);
        esq = n;
    }
    return esq;
}

static No *parse_igualdade(Parser *p) {
    No *esq = parse_relacional(p);
    while (p->atual.tipo == tk_Igual || p->atual.tipo == tk_Diferente) {
        Token t = p->atual;
        avancar(p);
        No *dir = parse_relacional(p);
        No *n = novo_no(AST_BINOP, t.linha, t.coluna);
        n->op = t.tipo;
        adicionar_filho(n, esq);
        adicionar_filho(n, dir);
        esq = n;
    }
    return esq;
}

static No *parse_e_logico(Parser *p) {
    No *esq = parse_igualdade(p);
    while (p->atual.tipo == tk_E) {
        Token t = p->atual;
        avancar(p);
        No *dir = parse_igualdade(p);
        No *n = novo_no(AST_BINOP, t.linha, t.coluna);
        n->op = t.tipo;
        adicionar_filho(n, esq);
        adicionar_filho(n, dir);
        esq = n;
    }
    return esq;
}

static No *parse_ou_logico(Parser *p) {
    No *esq = parse_e_logico(p);
    while (p->atual.tipo == tk_Ou) {
        Token t = p->atual;
        avancar(p);
        No *dir = parse_e_logico(p);
        No *n = novo_no(AST_BINOP, t.linha, t.coluna);
        n->op = t.tipo;
        adicionar_filho(n, esq);
        adicionar_filho(n, dir);
        esq = n;
    }
    return esq;
}

static No *parse_expr(Parser *p) {
    return parse_ou_logico(p);
}

static const TipoToken SYNC_STMT[] = {
    tk_PontoVirgula, tk_FechaChave, tk_If, tk_For,
    tk_Int, tk_Float, tk_Bool, tk_FimArquivo
};
#define N_SYNC_STMT (int)(sizeof SYNC_STMT / sizeof SYNC_STMT[0])

static No *parse_bloco(Parser *p) {
    Token t = p->atual;
    No *bloco = novo_no(AST_BLOCO, t.linha, t.coluna);

    if (!consumir(p, tk_AbreChave)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return bloco;
    }
    while (p->atual.tipo != tk_FechaChave && p->atual.tipo != tk_FimArquivo) {
        No *s = parse_stmt(p);
        if (s) adicionar_filho(bloco, s);
    }
    consumir(p, tk_FechaChave);
    return bloco;
}

static No *parse_declaracao(Parser *p) {
    Token t = p->atual;
    TipoToken tipo_prim = t.tipo;   
    avancar(p);

    No *n = novo_no(AST_DECLARACAO, t.linha, t.coluna);
    n->tipo_decl = tipo_prim;

    if (p->atual.tipo != tk_Identificador) {
        erro_sintatico(p, "esperado nome de variavel apos tipo");
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        if (p->atual.tipo == tk_PontoVirgula) avancar(p);
        return n;
    }
    n->valor.valor_texto = p->atual.valor.valor_texto; 
    avancar(p);

    if (p->atual.tipo == tk_Atribuicao) {
        avancar(p);
        No *init = parse_expr(p);
        adicionar_filho(n, init);
    }

    if (!consumir(p, tk_PontoVirgula)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        if (p->atual.tipo == tk_PontoVirgula) avancar(p);
    }
    return n;
}

static No *parse_atribuicao(Parser *p) {
    Token t = p->atual;
    char *nome = t.valor.valor_texto;   
    avancar(p);

    No *n = novo_no(AST_ATRIBUICAO, t.linha, t.coluna);
    n->valor.valor_texto = nome;        

    if (p->atual.tipo == tk_Inc || p->atual.tipo == tk_Dec) {
        n->op = p->atual.tipo;
        avancar(p);
    } else if (p->atual.tipo == tk_Atribuicao) {
        n->op = tk_Atribuicao;
        avancar(p);
        No *expr = parse_expr(p);
        adicionar_filho(n, expr);
    } else {
        erro_sintatico(p, "esperado '=', '++' ou '--' apos identificador");
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        if (p->atual.tipo == tk_PontoVirgula) avancar(p);
        return n;
    }

    if (!consumir(p, tk_PontoVirgula)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        if (p->atual.tipo == tk_PontoVirgula) avancar(p);
    }
    return n;
}

static No *parse_if(Parser *p) {
    Token t = p->atual;
    avancar(p); 
    No *n = novo_no(AST_IF, t.linha, t.coluna);

    if (!consumir(p, tk_AbreParen)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return n;
    }
    No *cond = parse_expr(p);
    adicionar_filho(n, cond);

    if (!consumir(p, tk_FechaParen)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return n;
    }
    No *corpo = parse_bloco(p);
    adicionar_filho(n, corpo);
    return n;
}

static No *parse_for(Parser *p) {
    Token t = p->atual;
    avancar(p); 
    No *n = novo_no(AST_FOR, t.linha, t.coluna);

    if (!consumir(p, tk_AbreParen)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return n;
    }

    if (p->atual.tipo == tk_Int  || p->atual.tipo == tk_Float|| p->atual.tipo == tk_Bool) {
        adicionar_filho(n, parse_declaracao(p));   
    } else if (p->atual.tipo == tk_Identificador) {
        adicionar_filho(n, parse_atribuicao(p));   
    } else {
        adicionar_filho(n, NULL);                  
        consumir(p, tk_PontoVirgula);
    }

    if (p->atual.tipo != tk_PontoVirgula) {
        adicionar_filho(n, parse_expr(p));
    } else {
        adicionar_filho(n, NULL); 
    }
    if (!consumir(p, tk_PontoVirgula)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return n;
    }

    if (p->atual.tipo == tk_Identificador) {
        Token tid = p->atual;
        char *nome_upd = tid.valor.valor_texto;
        avancar(p);
        No *upd = novo_no(AST_ATRIBUICAO, tid.linha, tid.coluna);
        upd->valor.valor_texto = nome_upd;
        if (p->atual.tipo == tk_Inc || p->atual.tipo == tk_Dec) {
            upd->op = p->atual.tipo;
            avancar(p);
        } else if (p->atual.tipo == tk_Atribuicao) {
            upd->op = tk_Atribuicao;
            avancar(p);
            adicionar_filho(upd, parse_expr(p));
        } else {
            erro_sintatico(p, "update do for invalido");
        }
        adicionar_filho(n, upd);
    } else if (p->atual.tipo != tk_FechaParen) {
        adicionar_filho(n, parse_expr(p));
    } else {
        adicionar_filho(n, NULL); 
    }

    if (!consumir(p, tk_FechaParen)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return n;
    }
    No *corpo = parse_bloco(p);
    adicionar_filho(n, corpo);
    return n;
}

static No *parse_stmt(Parser *p) {
    switch (p->atual.tipo) {
    case tk_Int: case tk_Float: case tk_Bool:
        return parse_declaracao(p);
    case tk_Identificador:
        return parse_atribuicao(p);
    case tk_If:
        return parse_if(p);
    case tk_For:
        return parse_for(p);
    case tk_AbreChave:
        return parse_bloco(p);
    case tk_PontoVirgula:
        avancar(p); 
        return NULL;
    case tk_FechaChave:
        return NULL;
    default:
        erro_sintatico(p, "statement invalido");
        if (p->atual.tipo != tk_FimArquivo && p->atual.tipo != tk_FechaChave) {
            if (p->atual.tipo == tk_Identificador || p->atual.tipo == tk_TextoString)
                free(p->atual.valor.valor_texto);
            avancar(p);
        }
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        if (p->atual.tipo == tk_PontoVirgula) avancar(p);
        return NULL;
    }
}

static No *parse_programa(Parser *p) {
    No *prog = novo_no(AST_PROGRAMA, 1, 1);
    while (p->atual.tipo != tk_FimArquivo) {
        TipoToken antes = p->atual.tipo;
        int linha_antes = p->atual.linha, col_antes = p->atual.coluna;
        No *s = parse_stmt(p);
        if (s) adicionar_filho(prog, s);
        if (p->atual.tipo == antes &&
            p->atual.linha == linha_antes &&
            p->atual.coluna == col_antes &&
            p->atual.tipo != tk_FimArquivo) {
            if (p->atual.tipo == tk_Identificador || p->atual.tipo == tk_TextoString)
                free(p->atual.valor.valor_texto);
            avancar(p);
        }
    }
    return prog;
}

static void imprimir_ast(No *n, int indent) __attribute__((unused));
static void imprimir_ast(No *n, int indent) {
    if (!n) return;
    for (int i = 0; i < indent; i++) printf("  ");
    switch (n->tipo) {
    case AST_INTEIRO:
        printf("INTEIRO(%ld)\n", n->valor.valor_inteiro); break;
    case AST_STRING:
        printf("STRING(\"%s\")\n", n->valor.valor_texto); break;
    case AST_IDENT:
        printf("IDENT(%s)\n", n->valor.valor_texto); break;
    case AST_POS_INC:
        printf("POS_INC(%s)\n", n->valor.valor_texto); break;
    case AST_POS_DEC:
        printf("POS_DEC(%s)\n", n->valor.valor_texto); break;
    case AST_BINOP:
        printf("BINOP(%s)\n", nome_token[n->op]); break;
    case AST_UNOP:
        printf("UNOP(%s)\n", nome_token[n->op]); break;
    case AST_DECLARACAO: {
        const char *tipo_str = "?";
        if (n->tipo_decl == tk_Int)   tipo_str = "int";
        if (n->tipo_decl == tk_Float) tipo_str = "float";
        if (n->tipo_decl == tk_Bool)  tipo_str = "bool";
        printf("DECLARACAO(%s %s)\n", tipo_str, n->valor.valor_texto ? n->valor.valor_texto : "?");
        break;
    }
    case AST_ATRIBUICAO:
        if (n->op == tk_Inc)
            printf("ATRIBUICAO(%s ++)\n", n->valor.valor_texto);
        else if (n->op == tk_Dec)
            printf("ATRIBUICAO(%s --)\n", n->valor.valor_texto);
        else
            printf("ATRIBUICAO(%s =)\n", n->valor.valor_texto);
        break;
    case AST_IF:    printf("IF\n");      break;
    case AST_FOR:   printf("FOR\n");     break;
    case AST_BLOCO: printf("BLOCO\n");   break;
    case AST_PROGRAMA: printf("PROGRAMA\n"); break;
    }
    for (int i = 0; i < n->num_filhos; i++)
        imprimir_ast(n->filhos[i], indent + 1);
}

typedef enum {
    SEM_INT,
    SEM_FLOAT,
    SEM_BOOL,
    SEM_ERRO    
} TipoSemantico;

static const char *nome_sem[] = { "int", "float", "bool", "<erro>" };

typedef struct Simbolo {
    char *nome;
    TipoSemantico  tipo;
    int linha, coluna;
    struct Simbolo *proximo;
} Simbolo;

typedef struct Escopo {
    Simbolo *simbolos;  
    struct Escopo *pai;       
} Escopo;

typedef struct {
    char *nome;
    TipoSemantico tipo;
    int linha, coluna, nivel;
} EntradaTabela;

typedef struct {
    Escopo *topo;          
    int nivel;         
    int erros;         
    EntradaTabela *tabela;        
    int num_entradas;
    int cap_entradas;
} AnalisadorSemantico;

//imprime onde ocorreu um erro semântico e soma 1 no contador de erros
static void erro_semantico(AnalisadorSemantico *sem, int linha, int col, const char *msg) {
    fprintf(stderr, "(%d,%d) Erro Semantico: %s\n", linha, col, msg);
    sem->erros++;
}

//converte os tokens do analisador léxico para os tipos do analisador semântico
static TipoSemantico token_para_sem(TipoToken t) {
    if (t == tk_Int) return SEM_INT;
    if (t == tk_Float) return SEM_FLOAT;
    if (t == tk_Bool) return SEM_BOOL;
    return SEM_ERRO;
}

// cria um nível de escopo, então toda vez que entra em um novo bloco ele chama essa variável para que as variáveis declarada ali dentro sejam isoladas
static void entrar_escopo(AnalisadorSemantico *sem) {
    Escopo *e  = calloc(1, sizeof *e);
    e->pai = sem->topo;
    sem->topo = e;
    sem->nivel++;
}

//destrói o escopo atual quando um bloco termina
static void sair_escopo(AnalisadorSemantico *sem) {
    Escopo *e = sem->topo;
    sem->topo = e->pai;
    sem->nivel--;
    Simbolo *s = e->simbolos;
    while (s) {
        Simbolo *prox = s->proximo;
        free(s->nome);
        free(s);
        s = prox;
    }
    free(e);
}

//procura uma variável no bloco que o programa está para impedirque crie duas variáveis com o mesmo nome
static Simbolo *buscar_escopo_atual(AnalisadorSemantico *sem, const char *nome) {
    for (Simbolo *s = sem->topo->simbolos; s; s = s->proximo)
        if (strcmp(s->nome, nome) == 0) return s;
    return NULL;
}

//procura uma variável no escopo atual e, se não achar, vai subindo para os escopos de fora, pra saber se pode usar uma variável global
static Simbolo *buscar_simbolo(AnalisadorSemantico *sem, const char *nome) {
    for (Escopo *e = sem->topo; e; e = e->pai)
        for (Simbolo *s = e->simbolos; s; s = s->proximo)
            if (strcmp(s->nome, nome) == 0) return s;
    return NULL;
}

//registra uma nova variável no escopo atual para verificar se a variável já foi declarada no escopo
static void declarar_variavel(AnalisadorSemantico *sem, const char *nome, TipoSemantico tipo, int linha, int col) {
    if (buscar_escopo_atual(sem, nome)) {
        char msg[160];
        snprintf(msg, sizeof msg, "variavel '%s' ja foi declarada neste escopo", nome);
        erro_semantico(sem, linha, col, msg);
        return;
    }
    Simbolo *s = malloc(sizeof *s);
    s->nome = strdup(nome);
    s->tipo = tipo;
    s->linha = linha;
    s->coluna = col;
    s->proximo = sem->topo->simbolos;
    sem->topo->simbolos = s;

    if (sem->num_entradas == sem->cap_entradas) {
        sem->cap_entradas = sem->cap_entradas ? sem->cap_entradas * 2 : 8;
        sem->tabela = realloc(sem->tabela, sem->cap_entradas * sizeof(EntradaTabela));
    }
    EntradaTabela *e = &sem->tabela[sem->num_entradas++];
    e->nome = strdup(nome);
    e->tipo = tipo;
    e->linha = linha;
    e->coluna = col;
    e->nivel = sem->nivel;
}

//verifica se as expressões estão sendo usadas da maneira certa. tipo, ++ só deve ser usada em números, + deve ter números dos dois lados e se ! não está sendo usada em bool
static TipoSemantico verificar_expr(AnalisadorSemantico *sem, No *n) {
    if (!n) return SEM_ERRO;
    char msg[220];

    switch (n->tipo) {

    case AST_INTEIRO: return SEM_INT;
    case AST_STRING:  return SEM_ERRO;  

    case AST_IDENT: {
        Simbolo *s = buscar_simbolo(sem, n->valor.valor_texto);
        if (!s) {
            snprintf(msg, sizeof msg, "variavel '%s' nao foi declarada", n->valor.valor_texto);
            erro_semantico(sem, n->linha, n->coluna, msg);
            return SEM_ERRO;
        }
        return s->tipo;
    }

    case AST_POS_INC:
    case AST_POS_DEC: {
        Simbolo *s = buscar_simbolo(sem, n->valor.valor_texto);
        if (!s) {
            snprintf(msg, sizeof msg, "variavel '%s' nao foi declarada", n->valor.valor_texto);
            erro_semantico(sem, n->linha, n->coluna, msg);
            return SEM_ERRO;
        }
        if (s->tipo != SEM_INT && s->tipo != SEM_FLOAT) {
            snprintf(msg, sizeof msg, "operador ++/-- requer tipo numerico, " "'%s' tem tipo '%s'", n->valor.valor_texto, nome_sem[s->tipo]);
            erro_semantico(sem, n->linha, n->coluna, msg);
            return SEM_ERRO;
        }
        return s->tipo;
    }

    case AST_UNOP: {
        TipoSemantico t = verificar_expr(sem, n->filhos[0]);
        if (t == SEM_ERRO) return SEM_ERRO;

        if (n->op == tk_Sub) {
            if (t != SEM_INT && t != SEM_FLOAT) {
                snprintf(msg, sizeof msg, "operador '-' unario requer tipo numerico, " "encontrado '%s'", nome_sem[t]);
                erro_semantico(sem, n->linha, n->coluna, msg);
                return SEM_ERRO;
            }
            return t;
        }
        if (n->op == tk_Nao) {
            if (t != SEM_BOOL) {
                snprintf(msg, sizeof msg, "operador '!' requer bool, encontrado '%s'", nome_sem[t]);
                erro_semantico(sem, n->linha, n->coluna, msg);
                return SEM_ERRO;
            }
            return SEM_BOOL;
        }
        return SEM_ERRO;
    }

    case AST_BINOP: {
        TipoSemantico esq = verificar_expr(sem, n->filhos[0]);
        TipoSemantico dir = verificar_expr(sem, n->filhos[1]);

        if (esq == SEM_ERRO || dir == SEM_ERRO) return SEM_ERRO;

        bool esq_num = (esq == SEM_INT || esq == SEM_FLOAT);
        bool dir_num = (dir == SEM_INT || dir == SEM_FLOAT);

        switch (n->op) {

        case tk_Soma: case tk_Sub:
        case tk_Mult: case tk_Div: case tk_Mod:
            if (!esq_num || !dir_num) {
                snprintf(msg, sizeof msg, "operacao aritmetica requer tipos numericos, " "encontrado '%s' e '%s'", nome_sem[esq], nome_sem[dir]);
                erro_semantico(sem, n->linha, n->coluna, msg);
                return SEM_ERRO;
            }
            return (esq == SEM_FLOAT || dir == SEM_FLOAT) ? SEM_FLOAT : SEM_INT;

        case tk_Menor: case tk_MenorIgual:
        case tk_Maior: case tk_MaiorIgual:
            if (!esq_num || !dir_num) {
                snprintf(msg, sizeof msg, "comparacao relacional requer tipos numericos, " "encontrado '%s' e '%s'", nome_sem[esq], nome_sem[dir]);
                erro_semantico(sem, n->linha, n->coluna, msg);
                return SEM_ERRO;
            }
            return SEM_BOOL;

        case tk_Igual: case tk_Diferente:
            if (esq != dir && !(esq_num && dir_num)) {
                snprintf(msg, sizeof msg, "comparacao de igualdade entre tipos incompativeis " "'%s' e '%s'",
                         nome_sem[esq], nome_sem[dir]);
                erro_semantico(sem, n->linha, n->coluna, msg);
                return SEM_ERRO;
            }
            return SEM_BOOL;

        case tk_E: case tk_Ou:
            if (esq != SEM_BOOL || dir != SEM_BOOL) {
                snprintf(msg, sizeof msg, "operacao logica requer bool nos dois lados, " "encontrado '%s' e '%s'", nome_sem[esq], nome_sem[dir]);
                erro_semantico(sem, n->linha, n->coluna, msg);
                return SEM_ERRO;
            }
            return SEM_BOOL;

        default: return SEM_ERRO;
        }
    }

    default: return SEM_ERRO;
    }
}

//verifica se é permitido guardar um valor em uma variável, por exemplo, permite guardar um valor inteiro em uma variável float mas não permite o contrário
static bool tipo_atribuivel(TipoSemantico destino, TipoSemantico origem) {
    if (destino == origem) return true;
    if (destino == SEM_FLOAT && origem == SEM_INT) return true;
    return false;
}

//percorre a ast para analisar o nó atual, e chama a função entrar_escopo se achar um bloco ou checa se a condição do if resulta em um booleano
static void analisar_no(AnalisadorSemantico *sem, No *n) {
    if (!n) return;
    char msg[220];

    switch (n->tipo) {

    case AST_PROGRAMA:
        entrar_escopo(sem);
        for (int i = 0; i < n->num_filhos; i++)
            analisar_no(sem, n->filhos[i]);
        sair_escopo(sem);
        break;

    case AST_BLOCO:
        entrar_escopo(sem);
        for (int i = 0; i < n->num_filhos; i++)
            analisar_no(sem, n->filhos[i]);
        sair_escopo(sem);
        break;

    case AST_DECLARACAO: {
        if (!n->valor.valor_texto) break;
        TipoSemantico tipo = token_para_sem(n->tipo_decl);
        declarar_variavel(sem, n->valor.valor_texto, tipo, n->linha, n->coluna);

        if (n->num_filhos > 0 && n->filhos[0]) {
            TipoSemantico t_init = verificar_expr(sem, n->filhos[0]);
            if (t_init != SEM_ERRO && !tipo_atribuivel(tipo, t_init)) {
                snprintf(msg, sizeof msg, "tipo incompativel na inicializacao de '%s': " "esperado '%s', encontrado '%s'", n->valor.valor_texto, nome_sem[tipo], nome_sem[t_init]);
                erro_semantico(sem, n->linha, n->coluna, msg);
            }
        }
        break;
    }

    case AST_ATRIBUICAO: {
        const char *nome = n->valor.valor_texto;
        Simbolo *s = buscar_simbolo(sem, nome);
        if (!s) {
            snprintf(msg, sizeof msg, "variavel '%s' nao foi declarada", nome);
            erro_semantico(sem, n->linha, n->coluna, msg);
            break;
        }
        if (n->op == tk_Inc || n->op == tk_Dec) {
            if (s->tipo != SEM_INT && s->tipo != SEM_FLOAT) {
                snprintf(msg, sizeof msg, "operador ++/-- requer tipo numerico, " "'%s' tem tipo '%s'", nome, nome_sem[s->tipo]);
                erro_semantico(sem, n->linha, n->coluna, msg);
            }
        } else {
            if (n->num_filhos > 0 && n->filhos[0]) {
                TipoSemantico t = verificar_expr(sem, n->filhos[0]);
                if (t != SEM_ERRO && !tipo_atribuivel(s->tipo, t)) {
                    snprintf(msg, sizeof msg, "tipo incompativel na atribuicao a '%s': " "esperado '%s', encontrado '%s'", nome, nome_sem[s->tipo], nome_sem[t]);
                    erro_semantico(sem, n->linha, n->coluna, msg);
                }
            }
        }
        break;
    }

    case AST_IF: {
        if (n->num_filhos >= 1 && n->filhos[0]) {
            TipoSemantico t = verificar_expr(sem, n->filhos[0]);
            if (t != SEM_ERRO && t != SEM_BOOL) {
                snprintf(msg, sizeof msg, "condicao do 'if' deve ser bool, encontrado '%s'", nome_sem[t]);
                erro_semantico(sem, n->linha, n->coluna, msg);
            }
        }
        if (n->num_filhos >= 2) analisar_no(sem, n->filhos[1]);
        break;
    }

    case AST_FOR: {
        entrar_escopo(sem);  

        if (n->num_filhos >= 1) analisar_no(sem, n->filhos[0]);

        if (n->num_filhos >= 2 && n->filhos[1]) {
            TipoSemantico t = verificar_expr(sem, n->filhos[1]);
            if (t != SEM_ERRO && t != SEM_BOOL) {
                snprintf(msg, sizeof msg, "condicao do 'for' deve ser bool, encontrado '%s'", nome_sem[t]);
                erro_semantico(sem, n->linha, n->coluna, msg);
            }
        }

        if (n->num_filhos >= 3) analisar_no(sem, n->filhos[2]);
        if (n->num_filhos >= 4) analisar_no(sem, n->filhos[3]);

        sair_escopo(sem);
        break;
    }

    default:
        for (int i = 0; i < n->num_filhos; i++)
            analisar_no(sem, n->filhos[i]);
        break;
    }
}

//imprime a tabela de símbolos
static void imprimir_tabela(AnalisadorSemantico *sem) {
    printf("=== TABELA DE SIMBOLOS ===\n");
    printf("%-4s %-8s %-16s %s\n", "Niv", "Tipo", "Nome", "Declarado em");
    printf("%-4s %-8s %-16s %s\n", "---", "------", "---------------", "------------");
    for (int i = 0; i < sem->num_entradas; i++) {
        EntradaTabela *e = &sem->tabela[i];
        printf("[%d]  %-8s %-16s linha %d, coluna %d\n", e->nivel - 1, nome_sem[e->tipo], e->nome, e->linha, e->coluna);
    }
    printf("\n");
}

//limpa a memória alocada para a tabela de símbolos
static void liberar_analisador(AnalisadorSemantico *sem) {
    for (int i = 0; i < sem->num_entradas; i++)
        free(sem->tabela[i].nome);
    free(sem->tabela);
}

int main(int argc, char *argv[]) {
    FILE *arquivo_fonte = stdin;
    if (argc > 1) {
        arquivo_fonte = fopen(argv[1], "r");
        if (!arquivo_fonte) {
            fprintf(stderr, "Erro ao abrir '%s'\n", argv[1]);
            return EXIT_FAILURE;
        }
    }

    AnalisadorLexico lex;
    lex.arquivo_fonte = arquivo_fonte;
    lex.linha = 1; lex.coluna = 0;
    lex.pivo = lex.buffer;
    lex.batedor = lex.buffer - 1;
    lex.limite_buffer = lex.buffer;
    lex.caractere_atual = ' ';
    avancar_caractere(&lex);

    Parser p;
    p.lex = &lex;
    p.erros = 0;
    p.atual = proximo_token(&lex);

    No *ast = parse_programa(&p);

    AnalisadorSemantico sem = { 0 };
    analisar_no(&sem, ast);

    imprimir_tabela(&sem);

    if (p.erros == 0 && sem.erros == 0) {
        printf("=== SUCESSO: programa sintatica e semanticamente correto ===\n");
    } else {
        if (p.erros > 0)
            printf("=== %d erro(s) sintatico(s) ===\n", p.erros);
        if (sem.erros > 0)
            printf("=== %d erro(s) semantico(s) ===\n", sem.erros);
    }

    liberar_no(ast);
    liberar_analisador(&sem);
    if (arquivo_fonte != stdin) fclose(arquivo_fonte);
    return (p.erros == 0 && sem.erros == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}