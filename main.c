/* =========================================================================
   EXPRESSION CALCULATOR  (DSA Mini Project)
   ---------------------------------------------------------------------
   Concepts used : Stack (array based), Infix -> Postfix conversion,
                   Postfix expression evaluation.
   Operators      : +  -  *  /  %  ^  ( )
   Author         : <Your Name Here>
   ========================================================================= */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define MAX 200

/* ---------------------------------------------------------------------
   Generic character stack  (used for infix -> postfix conversion)
   --------------------------------------------------------------------- */
typedef struct {
    char data[MAX];
    int  top;
} CharStack;

void csInit(CharStack *s)          { s->top = -1; }
int  csIsEmpty(CharStack *s)       { return s->top == -1; }
int  csIsFull(CharStack *s)        { return s->top == MAX - 1; }
void csPush(CharStack *s, char c)  { if (!csIsFull(s)) s->data[++(s->top)] = c; }
char csPop(CharStack *s)           { return csIsEmpty(s) ? '\0' : s->data[(s->top)--]; }
char csPeek(CharStack *s)          { return csIsEmpty(s) ? '\0' : s->data[s->top]; }

/* ---------------------------------------------------------------------
   Generic double stack  (used for postfix evaluation)
   --------------------------------------------------------------------- */
typedef struct {
    double data[MAX];
    int    top;
} NumStack;

void nsInit(NumStack *s)           { s->top = -1; }
int  nsIsEmpty(NumStack *s)        { return s->top == -1; }
int  nsIsFull(NumStack *s)         { return s->top == MAX - 1; }
void nsPush(NumStack *s, double v) { if (!nsIsFull(s)) s->data[++(s->top)] = v; }
double nsPop(NumStack *s)          { return nsIsEmpty(s) ? 0 : s->data[(s->top)--]; }

/* ---------------------------------------------------------------------
   Helper functions
   --------------------------------------------------------------------- */
int isOperator(char c) {
    return (c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c == '^');
}

/* Higher number = higher precedence */
int precedence(char op) {
    switch (op) {
        case '^': return 3;
        case '*': case '/': case '%': return 2;
        case '+': case '-': return 1;
        default:  return 0;
    }
}

/* '^' is right-associative, others are left-associative */
int isRightAssoc(char op) {
    return op == '^';
}

double applyOp(double a, double b, char op, int *err) {
    switch (op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;

        case '/':
            if (b == 0) {
                *err = 1;
                return 0;
            }
            return a / b;

        case '%':
            if ((long long)b == 0) {
                *err = 1;
                return 0;
            }
            return (double)((long long)a % (long long)b);

        case '^': return pow(a, b);

        default:
            *err = 1;
            return 0;
    }
}

/* Quick sanity check on the raw infix string before processing */
int isBalanced(const char *expr) {
    int bal = 0;

    for (int i = 0; expr[i]; i++) {
        if (expr[i] == '(')
            bal++;
        else if (expr[i] == ')')
            bal--;

        if (bal < 0)
            return 0;
    }

    return bal == 0;
}

/* ---------------------------------------------------------------------
   Infix -> Postfix conversion  (classic Shunting-Yard using a stack)
   Output is space separated so that multi-digit / decimal numbers and
   multi-letter variable-like tokens remain distinguishable.
   --------------------------------------------------------------------- */
int infixToPostfix(const char *infix, char *postfix) {
    CharStack s;
    csInit(&s);

    int j = 0;
    int n = strlen(infix);

    for (int i = 0; i < n; i++) {
        char c = infix[i];

        if (isspace(c))
            continue;

        if (isdigit(c) || c == '.') {

            while (i < n && (isdigit(infix[i]) || infix[i] == '.')) {
                postfix[j++] = infix[i++];
            }

            i--;
            postfix[j++] = ' ';
        }

        else if (c == '(') {
            csPush(&s, c);
        }

        else if (c == ')') {

            while (!csIsEmpty(&s) && csPeek(&s) != '(') {
                postfix[j++] = csPop(&s);
                postfix[j++] = ' ';
            }

            if (csIsEmpty(&s))
                return 0;

            csPop(&s);
        }

        else if (isOperator(c)) {

            /* handle unary minus/plus, e.g. -5 + 3 or 4 * -2 */
            int isUnary = (c == '-' || c == '+') &&
                          (i == 0 ||
                           infix[i-1] == '(' ||
                           isOperator(infix[i-1]));

            if (isUnary) {
                postfix[j++] = '0';
                postfix[j++] = ' ';
            }

            while (!csIsEmpty(&s) &&
                   isOperator(csPeek(&s)) &&
                   (precedence(csPeek(&s)) > precedence(c) ||
                   (precedence(csPeek(&s)) == precedence(c) &&
                    !isRightAssoc(c)))) {

                postfix[j++] = csPop(&s);
                postfix[j++] = ' ';
            }

            csPush(&s, c);
        }

        else {
            /* invalid character in expression */
            return 0;
        }
    }

    while (!csIsEmpty(&s)) {

        if (csPeek(&s) == '(')
            return 0;

        postfix[j++] = csPop(&s);
        postfix[j++] = ' ';
    }

    postfix[j] = '\0';

    return 1;
}

/* ---------------------------------------------------------------------
   Postfix evaluation using a numeric stack
   --------------------------------------------------------------------- */
int evaluatePostfix(const char *postfix, double *result) {
    NumStack s;
    nsInit(&s);

    char token[64];
    int ti = 0;
    int err = 0;
    int n = strlen(postfix);

    for (int i = 0; i <= n; i++) {

        char c = postfix[i];

        if (c == ' ' || c == '\0') {

            if (ti == 0)
                continue;

            token[ti] = '\0';
            ti = 0;

            if (isOperator(token[0]) && token[1] == '\0') {

                if (s.top < 1)
                    return 0;

                double b = nsPop(&s);
                double a = nsPop(&s);

                double r = applyOp(a, b, token[0], &err);

                if (err)
                    return -1;

                nsPush(&s, r);
            }

            else {
                nsPush(&s, atof(token));
            }
        }

        else {
            token[ti++] = c;
        }
    }

    if (s.top != 0)
        return 0;

    *result = nsPop(&s);

    return 1;
}

/* ---------------------------------------------------------------------
   UI helpers
   --------------------------------------------------------------------- */
void printLine(char ch, int len) {

    for (int i = 0; i < len; i++)
        putchar(ch);

    putchar('\n');
}

void printHeader(void) {

    printLine('=', 60);

    printf("||%-56s||\n",
           "            EXPRESSION CALCULATOR (DSA)");

    printf("||%-56s||\n",
           "   Infix -> Postfix Conversion & Stack Evaluation");

    printLine('=', 60);
}

void printMenu(void) {

    printLine('-', 60);

    printf(" 1. Evaluate an expression\n");
    printf(" 2. Show Postfix conversion only\n");
    printf(" 3. Show supported operators / rules\n");
    printf(" 4. Exit\n");

    printLine('-', 60);

    printf(" Enter your choice : ");
}

void printRules(void) {

    printLine('-', 60);

    printf(" Supported operators : + - * / %% ^  and parentheses ( )\n");
    printf(" Precedence (high to low) : ^  >  * / %%  >  + -\n");
    printf(" '^' is right associative, others are left associative\n");
    printf(" Unary +/- supported, e.g. -5 + 3 or 4 * -2\n");
    printf(" Example : 3 + 4 * 2 / (1 - 5) ^ 2\n");

    printLine('-', 60);
}

/* ---------------------------------------------------------------------
   Main driver
   --------------------------------------------------------------------- */
int main(void) {

    char infix[MAX];
    char postfix[MAX];
    int choice;

    printHeader();

    do {

        printMenu();

        if (scanf("%d", &choice) != 1) {

            /* clear bad input */
            int c;

            while ((c = getchar()) != '\n' && c != EOF);

            printf("\n Invalid input. Please enter a number (1-4).\n");

            continue;
        }

        getchar(); /* consume leftover newline */

        if (choice == 1 || choice == 2) {

            printf("\n Enter an infix expression:\n > ");

            if (!fgets(infix, MAX, stdin))
                break;

            infix[strcspn(infix, "\n")] = '\0';

            if (strlen(infix) == 0) {

                printf("\n [Error] Empty expression.\n");

                continue;
            }

            if (!isBalanced(infix)) {

                printf("\n [Error] Unbalanced parentheses in expression.\n");

                continue;
            }

            if (!infixToPostfix(infix, postfix)) {

                printf("\n [Error] Invalid expression / unsupported character.\n");

                continue;
            }

            printf("\n Infix   : %s\n", infix);
            printf(" Postfix : %s\n", postfix);

            if (choice == 1) {

                double result;

                int status = evaluatePostfix(postfix, &result);

                if (status == 1) {

                    printf(" Result  : %.6g\n", result);
                }

                else if (status == -1) {

                    printf(" [Error] Division / modulo by zero.\n");
                }

                else {

                    printf(" [Error] Malformed expression, cannot evaluate.\n");
                }
            }
        }

        else if (choice == 3) {

            printf("\n");

            printRules();
        }

        else if (choice == 4) {

            printf("\n Exiting... Thank you for using the Expression Calculator!\n");
        }

        else {

            printf("\n Invalid choice. Please select between 1 and 4.\n");
        }

        printf("\n");

    } while (choice != 4);

    return 0;
}