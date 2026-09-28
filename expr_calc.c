/*
 * expr_calc.c
 * Approximate reconstruction of the uploaded expr_calc.exe.
 *
 * Features:
 *   1. Evaluate an infix expression
 *   2. Show postfix conversion
 *   3. Show supported operators / rules
 *   4. Exit
 *
 * Operators: + - * / % ^ and parentheses
 * Unary + / - are supported.
 * ^ is right-associative; all other binary operators are left-associative.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define MAX_EXPR 1024
#define MAX_TOKENS 512

typedef enum {
    TOK_NUMBER,
    TOK_OPERATOR,
    TOK_LPAREN,
    TOK_RPAREN
} TokenType;

typedef struct {
    TokenType type;
    double value;
    char op;
} Token;

typedef struct {
    Token items[MAX_TOKENS];
    int top;
} TokenStack;

static void stack_init(TokenStack *s) {
    s->top = -1;
}

static int stack_empty(const TokenStack *s) {
    return s->top < 0;
}

static Token stack_peek(const TokenStack *s) {
    return s->items[s->top];
}

static void stack_push(TokenStack *s, Token t) {
    if (s->top < MAX_TOKENS - 1)
        s->items[++s->top] = t;
}

static Token stack_pop(TokenStack *s) {
    return s->items[s->top--];
}

static int is_binary_operator(char c) {
    return c == '+' || c == '-' || c == '*' ||
           c == '/' || c == '%' || c == '^';
}

static int precedence(char op) {
    if (op == '^')
        return 3;

    if (op == '*' || op == '/' || op == '%')
        return 2;

    if (op == '+' || op == '-')
        return 1;

    return 0;
}

static int is_right_associative(char op) {
    return op == '^';
}

static int tokenize(const char *expr, Token *tokens, int *count) {
    int n = 0;
    int expect_operand = 1;
    const char *p = expr;

    while (*p) {

        if (isspace((unsigned char)*p)) {
            ++p;
            continue;
        }

        if (isdigit((unsigned char)*p) || *p == '.') {

            char *end;
            double v = strtod(p, &end);

            if (end == p || n >= MAX_TOKENS)
                return 0;

            tokens[n++] = (Token){
                TOK_NUMBER,
                v,
                0
            };

            expect_operand = 0;
            p = end;
            continue;
        }

        if (*p == '(') {

            if (n >= MAX_TOKENS)
                return 0;

            tokens[n++] = (Token){
                TOK_LPAREN,
                0.0,
                '('
            };

            expect_operand = 1;
            ++p;
            continue;
        }

        if (*p == ')') {

            if (n >= MAX_TOKENS)
                return 0;

            tokens[n++] = (Token){
                TOK_RPAREN,
                0.0,
                ')'
            };

            expect_operand = 0;
            ++p;
            continue;
        }

        if (*p == '+' || *p == '-') {

            if (expect_operand) {

                /*
                 * Encode unary +/- as 0 +/- expression.
                 */

                if (n >= MAX_TOKENS - 1)
                    return 0;

                tokens[n++] = (Token){
                    TOK_NUMBER,
                    0.0,
                    0
                };

                tokens[n++] = (Token){
                    TOK_OPERATOR,
                    0.0,
                    *p
                };

                ++p;
                expect_operand = 1;
                continue;
            }
        }

        if (is_binary_operator(*p)) {

            if (expect_operand)
                return 0;

            if (n >= MAX_TOKENS)
                return 0;

            tokens[n++] = (Token){
                TOK_OPERATOR,
                0.0,
                *p
            };

            expect_operand = 1;
            ++p;
            continue;
        }

        return 0;
    }

    if (n == 0 || expect_operand)
        return 0;

    *count = n;

    return 1;
}

static int infix_to_postfix(
    const Token *in,
    int n,
    Token *out,
    int *out_n
) {
    TokenStack ops;
    int k = 0;

    stack_init(&ops);

    for (int i = 0; i < n; ++i) {

        Token t = in[i];

        if (t.type == TOK_NUMBER) {

            if (k >= MAX_TOKENS)
                return 0;

            out[k++] = t;
        }

        else if (t.type == TOK_LPAREN) {

            stack_push(&ops, t);
        }

        else if (t.type == TOK_RPAREN) {

            while (!stack_empty(&ops) &&
                   stack_peek(&ops).type != TOK_LPAREN) {

                out[k++] = stack_pop(&ops);
            }

            if (stack_empty(&ops))
                return 0;

            (void)stack_pop(&ops);
        }

        else if (t.type == TOK_OPERATOR) {

            while (!stack_empty(&ops) &&
                   stack_peek(&ops).type == TOK_OPERATOR) {

                char topop = stack_peek(&ops).op;

                int should_pop =
                    (!is_right_associative(t.op) &&
                     precedence(t.op) <= precedence(topop))
                    ||
                    (is_right_associative(t.op) &&
                     precedence(t.op) < precedence(topop));

                if (!should_pop)
                    break;

                out[k++] = stack_pop(&ops);
            }

            stack_push(&ops, t);
        }
    }

    while (!stack_empty(&ops)) {

        if (stack_peek(&ops).type == TOK_LPAREN)
            return 0;

        out[k++] = stack_pop(&ops);
    }

    *out_n = k;

    return 1;
}

static void print_number(double x) {
    printf("%.6g", x);
}

static void print_postfix(
    const Token *postfix,
    int n
) {
    for (int i = 0; i < n; ++i) {

        if (i)
            putchar(' ');

        if (postfix[i].type == TOK_NUMBER)
            print_number(postfix[i].value);
        else
            putchar(postfix[i].op);
    }

    putchar('\n');
}

static int evaluate_postfix(
    const Token *p,
    int n,
    double *result
) {
    double stack[MAX_TOKENS];
    int top = -1;

    for (int i = 0; i < n; ++i) {

        if (p[i].type == TOK_NUMBER) {

            if (top >= MAX_TOKENS - 1)
                return 0;

            stack[++top] = p[i].value;
            continue;
        }

        if (p[i].type != TOK_OPERATOR || top < 1)
            return 0;

        double b = stack[top--];
        double a = stack[top--];

        switch (p[i].op) {

            case '+':
                stack[++top] = a + b;
                break;

            case '-':
                stack[++top] = a - b;
                break;

            case '*':
                stack[++top] = a * b;
                break;

            case '/':

                if (b == 0.0) {
                    printf(
                        "[Error]