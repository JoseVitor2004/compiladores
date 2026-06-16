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
    "Op_Incremento",    "Op_Decremento",
    "Op_MenosUnario",   "Op_NaoLogico",
    "Op_Menor",         "Op_MenorIgual",
    "Op_Maior",         "Op_MaiorIgual",
    "Op_Igual",         "Op_Diferente",
    "Op_Atribuicao",
    "Op_E_Logico",      "Op_Ou_Logico",
    "Palavra_if",       "Palavra_for", "Palavra_int", "Palavra_float", "Palavra_bool",
    "AbreChave",        "FechaChave",
    "AbreParenteses",   "FechaParenteses",
    "PontoVirgula",     "Virgula",
    "Identificador",    "NumeroInteiro", "TextoString"
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
    char  buffer[TAMANHO_BUFFER];
    char *pivo;
    char *batedor;
    char *limite_buffer;
    int   caractere_atual;
    int   linha, coluna;
} AnalisadorLexico;
 
typedef struct { const char *palavra; TipoToken tipo; } PalavraChave;
 
static const PalavraChave tabela_palavras[] = {
    {"bool",  tk_Bool },
    {"float", tk_Float},
    {"for",   tk_For  },
    {"if",    tk_If   },
    {"int",   tk_Int  }
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
    lex->pivo     = lex->batedor;
    erro_linha    = lex->linha;
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
            if (isdigit(c))             { estado = ESTADO_INTEIRO;        continue; }
            switch (c) {
            case '"':  avancar_caractere(lex); estado = ESTADO_STRING;       continue;
            case '\'': avancar_caractere(lex); estado = ESTADO_CHAR_CORPO;   continue;
            case '/':  avancar_caractere(lex); estado = ESTADO_BARRA;        continue;
            case '<':  avancar_caractere(lex); estado = ESTADO_MENOR;        continue;
            case '>':  avancar_caractere(lex); estado = ESTADO_MAIOR;        continue;
            case '=':  avancar_caractere(lex); estado = ESTADO_SINAL_IGUAL;  continue;
            case '!':  avancar_caractere(lex); estado = ESTADO_EXCLAMACAO;   continue;
            case '&':  avancar_caractere(lex); estado = ESTADO_E_COMERCIAL;  continue;
            case '|':  avancar_caractere(lex); estado = ESTADO_PIPE;         continue;
            case '+':  avancar_caractere(lex); estado = ESTADO_MAIS;         continue;
            case '-':  avancar_caractere(lex); estado = ESTADO_MENOS;        continue;
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
            else          { token_atual.tipo = tk_Soma; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna;
            return token_atual;
        case ESTADO_MENOS:
            if (c == '-') { avancar_caractere(lex); token_atual.tipo = tk_Dec; }
            else          { token_atual.tipo = tk_Sub; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna;
            return token_atual;
        case ESTADO_IDENTIFICADOR:
            if (isalnum(c) || c == '_') { avancar_caractere(lex); }
            else {
                char *t = extrair_lexema(lex);
                const PalavraChave *kw = bsearch(t, tabela_palavras, QTD_PALAVRAS,
                                                  sizeof tabela_palavras[0], comparar_palavras);
                token_atual.linha = erro_linha; token_atual.coluna = erro_coluna;
                token_atual.valor.valor_inteiro = 0;
                if (kw) { token_atual.tipo = kw->tipo; free(t); }
                else    { token_atual.tipo = tk_Identificador; token_atual.valor.valor_texto = t; }
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
            else            avancar_caractere(lex);
            continue;
        case ESTADO_CMT_ASTERISCO:
            if (c == '/') { avancar_caractere(lex); goto reiniciar; }
            if (c == '*')   avancar_caractere(lex);
            else          { avancar_caractere(lex); estado = ESTADO_CMT_CORPO; }
            continue;
        case ESTADO_MENOR:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_MenorIgual; }
            else          { token_atual.tipo = tk_Menor; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_MAIOR:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_MaiorIgual; }
            else          { token_atual.tipo = tk_Maior; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_SINAL_IGUAL:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_Igual; }
            else          { token_atual.tipo = tk_Atribuicao; }
            token_atual.linha = erro_linha; token_atual.coluna = erro_coluna; token_atual.valor.valor_inteiro = 0;
            return token_atual;
        case ESTADO_EXCLAMACAO:
            if (c == '=') { avancar_caractere(lex); token_atual.tipo = tk_Diferente; }
            else          { token_atual.tipo = tk_Nao; }
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

//estrutura com todos os tipos de nó
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

//struct do nó, com tipo, posição, operador, valor, etc.
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

//aloca e inicia o nó
static No *novo_no(TipoNo tipo, int linha, int coluna) {
    No *n = calloc(1, sizeof *n);
    n->tipo   = tipo;
    n->linha  = linha;
    n->coluna = coluna;
    return n;
}

//encaixa um nó filho dentro de outro
static void adicionar_filho(No *pai, No *filho) {
    pai->filhos = realloc(pai->filhos, (pai->num_filhos + 1) * sizeof(No *));
    pai->filhos[pai->num_filhos++] = filho;
}

//libera memória da árvore
static void liberar_no(No *n) {
    if (!n) return;
    for (int i = 0; i < n->num_filhos; i++) liberar_no(n->filhos[i]);
    free(n->filhos);
    if (n->tipo == AST_IDENT || n->tipo == AST_STRING ||
        n->tipo == AST_DECLARACAO || n->tipo == AST_ATRIBUICAO ||
        n->tipo == AST_POS_INC    || n->tipo == AST_POS_DEC)
        free(n->valor.valor_texto);
    free(n);
}

//struct do parser, com o léxico, o token atual e o contador de erros
typedef struct {
    AnalisadorLexico *lex;
    Token  atual;          
    int    erros;           
} Parser;
 
static No *parse_stmt(Parser *p);
static No *parse_expr(Parser *p);
  
static void erro_sintatico(Parser *p, const char *msg) {
    fprintf(stderr, "(%d,%d) Erro Sintatico: %s (encontrado: %s)\n",
            p->atual.linha, p->atual.coluna, msg, nome_token[p->atual.tipo]);
    p->erros++;
}

//essa e a próxima são funções de movimentação
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

//recuperação de erros pelo modo pânico
static void sincronizar(Parser *p, const TipoToken *conjunto, int n) {
    while (p->atual.tipo != tk_FimArquivo) {
        for (int i = 0; i < n; i++)
            if (p->atual.tipo == conjunto[i]) return;
        if (p->atual.tipo == tk_Identificador || p->atual.tipo == tk_TextoString)
            free(p->atual.valor.valor_texto);
        avancar(p);
    }
}

//função para reconhecer números, identificadores e expressões entre parênteses
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

//reconhece -x e !x
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

//reconhece *, / e %
static No *parse_multiplicativo(Parser *p) {
    No *esq = parse_unario(p);
    while (p->atual.tipo == tk_Mult ||
           p->atual.tipo == tk_Div  ||
           p->atual.tipo == tk_Mod) {
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

//reconhece + e -
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

//reconhece <, <=, > e >=
static No *parse_relacional(Parser *p) {
    No *esq = parse_aditivo(p);
    while (p->atual.tipo == tk_Menor    || p->atual.tipo == tk_MenorIgual ||
           p->atual.tipo == tk_Maior    || p->atual.tipo == tk_MaiorIgual) {
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

//reconhece == e !=
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

//reconhece &&
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

//reconhece ||
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

//reconhece uma lista de declarações
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

//reconhece int x e int x = 0
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

//reconhece x = qualquer expressão, x++ e x--
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

//reconhece a estrutura do if
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

//reconhece a estrutura do for
static No *parse_for(Parser *p) {
    Token t = p->atual;
    avancar(p); 
    No *n = novo_no(AST_FOR, t.linha, t.coluna);
 
    if (!consumir(p, tk_AbreParen)) {
        sincronizar(p, SYNC_STMT, N_SYNC_STMT);
        return n;
    }
 
    if (p->atual.tipo == tk_Int  ||
        p->atual.tipo == tk_Float||
        p->atual.tipo == tk_Bool) {
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

//função para decidir qual função acima escolher de acordo com  o que foi lido
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

//chama o parse_stmt em loop até o fim do arquivo
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
 
static void imprimir_ast(No *n, int indent) {
    if (!n) return;
    for (int i = 0; i < indent; i++) printf("  ");
    switch (n->tipo) {
    case AST_INTEIRO:
        printf("Inteiro(%ld)\n", n->valor.valor_inteiro); break;
    case AST_STRING:
        printf("String(\"%s\")\n", n->valor.valor_texto); break;
    case AST_IDENT:
        printf("Ident(%s)\n", n->valor.valor_texto); break;
    case AST_POS_INC:
        printf("Pos_Inc(%s)\n", n->valor.valor_texto); break;
    case AST_POS_DEC:
        printf("Pos_Dec(%s)\n", n->valor.valor_texto); break;
    case AST_BINOP:
        printf("BinOp(%s)\n", nome_token[n->op]); break;
    case AST_UNOP:
        printf("UnOp(%s)\n", nome_token[n->op]); break;
    case AST_DECLARACAO: {
        const char *tipo_str = "?";
        if (n->tipo_decl == tk_Int)   tipo_str = "int";
        if (n->tipo_decl == tk_Float) tipo_str = "float";
        if (n->tipo_decl == tk_Bool)  tipo_str = "bool";
        printf("Declaracao(%s %s)\n", tipo_str, n->valor.valor_texto ? n->valor.valor_texto : "?");
        break;
    }
    case AST_ATRIBUICAO:
        if (n->op == tk_Inc)
            printf("Atribuicao(%s ++)\n", n->valor.valor_texto);
        else if (n->op == tk_Dec)
            printf("Atribuicao(%s --)\n", n->valor.valor_texto);
        else
            printf("Atribuicao(%s =)\n", n->valor.valor_texto);
        break;
    case AST_IF:    printf("If\n");      break;
    case AST_FOR:   printf("For\n");     break;
    case AST_BLOCO: printf("Bloco\n");   break;
    case AST_PROGRAMA: printf("Programa\n"); break;
    }
    for (int i = 0; i < n->num_filhos; i++)
        imprimir_ast(n->filhos[i], indent + 1);
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
    lex.pivo     = lex.buffer;
    lex.batedor  = lex.buffer - 1;
    lex.limite_buffer = lex.buffer;
    lex.caractere_atual = ' ';
    avancar_caractere(&lex);
 
    Parser p;
    p.lex   = &lex;
    p.erros = 0;
    p.atual = proximo_token(&lex);
 
    No *ast = parse_programa(&p);
 
    printf("\nAST:\n");
    imprimir_ast(ast, 0);
    printf("\n");
 
    if (p.erros == 0) {
        printf("Sucesso: programa sintaticamente correto\n");
    } else {
        printf("Erro: %d erro(s) sintatico(s) encontrado(s)\n", p.erros);
    }
 
    liberar_no(ast);
    if (arquivo_fonte != stdin) fclose(arquivo_fonte);
    return p.erros == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}