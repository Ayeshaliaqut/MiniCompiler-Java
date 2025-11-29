// File: MiniParser.java
// A small lexer + recursive descent parser producing an AST for a tiny language.
//
// Grammar (informal):
// program      := { statement }
// statement    := "var" IDENTIFIER "=" expr ";"     // variable declaration/assignment
//               | IDENTIFIER "=" expr ";"           // assignment
//               | "if" "(" expr ")" block [ "else" block ]
//               | "while" "(" expr ")" block
//               | "print" "(" expr ")" ";"
//               | block
// block        := "{" { statement } "}"
// expr         := equality
// equality     := comparison { ( "==" | "!=" ) comparison }
// comparison   := term { ( "<" | "<=" | ">" | ">=" ) term }
// term         := factor { ( "+" | "-" ) factor }
// factor       := unary { ( "*" | "/" ) unary }
// unary        := ( "-" | "!" ) unary | primary
// primary      := NUMBER | IDENTIFIER | "(" expr ")"
//
// This program is intentionally simple and focuses on parsing and AST structure.

import java.util.*;
import java.io.*;

public class MiniParser {
    // -------------------------
    // Entry: main + sample run
    // -------------------------
    public static void main(String[] args) {
        String sample =
            "var x = 10;\n" +
            "var y = 20;\n" +
            "x = x + 5 * (y - 2);\n" +
            "if (x > 30) {\n" +
            "  print(x);\n" +
            "} else {\n" +
            "  print(y);\n" +
            "}\n" +
            "while (x < 100) { x = x + 1; }\n";

        System.out.println("Input program:\n----------------\n" + sample);
        Lexer lexer = new Lexer(sample);
        List<Token> tokens = lexer.tokenize();

        Parser parser = new Parser(tokens);
        try {
            List<Stmt> program = parser.parseProgram();
            System.out.println("Parsed AST:\n-----------");
            AstPrinter printer = new AstPrinter();
            for (Stmt s : program) {
                System.out.println(printer.print(s));
            }
        } catch (ParseException ex) {
            System.err.println("Parse error: " + ex.getMessage());
        }
    }

    // -------------------------
    // TOKENS / LEXER
    // -------------------------
    enum TokenType {
        // single char tokens
        LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
        PLUS, MINUS, STAR, SLASH,
        BANG, EQUAL, LESS, GREATER, SEMICOLON, COMMA,

        // two-char tokens
        BANG_EQUAL, EQUAL_EQUAL, LESS_EQUAL, GREATER_EQUAL,

        // literals
        IDENTIFIER, NUMBER,

        // keywords
        VAR, IF, ELSE, WHILE, PRINT,

        EOF
    }

    static class Token {
        final TokenType type;
        final String lexeme;
        final Object literal;
        final int line;

        Token(TokenType type, String lexeme, Object literal, int line) {
            this.type = type;
            this.lexeme = lexeme;
            this.literal = literal;
            this.line = line;
        }

        public String toString() {
            return type + " '" + lexeme + "'" + (literal != null ? " (" + literal + ")" : "");
        }
    }

    static class Lexer {
        private final String source;
        private final List<Token> tokens = new ArrayList<>();
        private int start = 0;
        private int current = 0;
        private int line = 1;

        private static final Map<String, TokenType> keywords;
        static {
            keywords = new HashMap<>();
            keywords.put("var", TokenType.VAR);
            keywords.put("if", TokenType.IF);
            keywords.put("else", TokenType.ELSE);
            keywords.put("while", TokenType.WHILE);
            keywords.put("print", TokenType.PRINT);
        }

        Lexer(String source) {
            this.source = source;
        }

        List<Token> tokenize() {
            while (!isAtEnd()) {
                start = current;
                scanToken();
            }
            tokens.add(new Token(TokenType.EOF, "", null, line));
            return tokens;
        }

        private boolean isAtEnd() {
            return current >= source.length();
        }

        private void scanToken() {
            char c = advance();
            switch (c) {
                case '(' -> addToken(TokenType.LEFT_PAREN);
                case ')' -> addToken(TokenType.RIGHT_PAREN);
                case '{' -> addToken(TokenType.LEFT_BRACE);
                case '}' -> addToken(TokenType.RIGHT_BRACE);
                case ',' -> addToken(TokenType.COMMA);
                case ';' -> addToken(TokenType.SEMICOLON);
                case '+' -> addToken(TokenType.PLUS);
                case '-' -> addToken(TokenType.MINUS);
                case '*' -> addToken(TokenType.STAR);
                case '/' -> addToken(TokenType.SLASH);
                case '!' -> addToken(match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
                case '=' -> addToken(match('=') ? TokenType.EQUAL_EQUAL : TokenType.EQUAL);
                case '<' -> addToken(match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);
                case '>' -> addToken(match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);
                case ' ', '\r', '\t' -> { /* ignore whitespace */ }
                case '\n' -> line++;
                case '"' -> string(); // not used, but placeholder
                default -> {
                    if (isDigit(c)) {
                        number();
                    } else if (isAlpha(c)) {
                        identifier();
                    } else {
                        throw new RuntimeException("Unexpected character at line " + line + ": '" + c + "'");
                    }
                }
            }
        }

        private char advance() {
            return source.charAt(current++);
        }

        private boolean match(char expected) {
            if (isAtEnd()) return false;
            if (source.charAt(current) != expected) return false;
            current++;
            return true;
        }

        private void addToken(TokenType type) {
            addToken(type, null);
        }

        private void addToken(TokenType type, Object literal) {
            String lex = source.substring(start, current);
            tokens.add(new Token(type, lex, literal, line));
        }

        private void number() {
            while (!isAtEnd() && isDigit(peek())) advance();
            if (!isAtEnd() && peek() == '.' && isDigit(peekNext())) {
                advance(); // consume '.'
                while (!isAtEnd() && isDigit(peek())) advance();
            }
            String lex = source.substring(start, current);
            addToken(TokenType.NUMBER, Double.parseDouble(lex));
        }

        private void identifier() {
            while (!isAtEnd() && isAlphaNumeric(peek())) advance();
            String text = source.substring(start, current);
            TokenType type = keywords.getOrDefault(text, TokenType.IDENTIFIER);
            addToken(type);
        }

        private void string() {
            while (!isAtEnd() && peek() != '"') {
                if (peek() == '\n') line++;
                advance();
            }
            if (isAtEnd()) throw new RuntimeException("Unterminated string at line " + line);
            advance(); // closing "
            String value = source.substring(start + 1, current - 1);
            addToken(TokenType.IDENTIFIER, value);
        }

        private char peek() {
            if (isAtEnd()) return '\0';
            return source.charAt(current);
        }

        private char peekNext() {
            if (current + 1 >= source.length()) return '\0';
            return source.charAt(current + 1);
        }

        private boolean isDigit(char c) {
            return c >= '0' && c <= '9';
        }

        private boolean isAlpha(char c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
        }

        private boolean isAlphaNumeric(char c) {
            return isAlpha(c) || isDigit(c);
        }
    }

    // -------------------------
    // PARSER (recursive descent)
    // -------------------------
    static class ParseException extends RuntimeException {
        ParseException(String message) { super(message); }
    }

    static class Parser {
        private final List<Token> tokens;
        private int current = 0;

        Parser(List<Token> tokens) {
            this.tokens = tokens;
        }

        List<Stmt> parseProgram() {
            List<Stmt> statements = new ArrayList<>();
            while (!isAtEnd()) {
                statements.add(statement());
            }
            return statements;
        }

        private Stmt statement() {
            if (match(TokenType.VAR)) return varDeclaration();
            if (match(TokenType.IF)) return ifStatement();
            if (match(TokenType.WHILE)) return whileStatement();
            if (match(TokenType.PRINT)) return printStatement();
            if (match(TokenType.LEFT_BRACE)) return new Stmt.Block(blockStatements());
            // assignment or expression statement starting with identifier
            if (check(TokenType.IDENTIFIER) && checkNext(TokenType.EQUAL)) {
                Token name = consume(TokenType.IDENTIFIER, "Expect identifier.");
                consume(TokenType.EQUAL, "Expect '=' after identifier.");
                Expr value = expression();
                consume(TokenType.SEMICOLON, "Expect ';' after assignment.");
                return new Stmt.Var(name.lexeme, value); // reuse Var for assignment simplified
            }
            // fallback: expression statement
            Expr expr = expression();
            consume(TokenType.SEMICOLON, "Expect ';' after expression.");
            return new Stmt.Expression(expr);
        }

        private Stmt varDeclaration() {
            Token name = consume(TokenType.IDENTIFIER, "Expect variable name.");
            consume(TokenType.EQUAL, "Expect '=' after variable name.");
            Expr initializer = expression();
            consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
            return new Stmt.Var(name.lexeme, initializer);
        }

        private Stmt ifStatement() {
            consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
            Expr condition = expression();
            consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.");
            Stmt thenBranch = statement();
            Stmt elseBranch = null;
            if (match(TokenType.ELSE)) {
                elseBranch = statement();
            }
            return new Stmt.If(condition, thenBranch, elseBranch);
        }

        private Stmt whileStatement() {
            consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
            Expr cond = expression();
            consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");
            Stmt body = statement();
            return new Stmt.While(cond, body);
        }

        private Stmt printStatement() {
            consume(TokenType.LEFT_PAREN, "Expect '(' after 'print'.");
            Expr value = expression();
            consume(TokenType.RIGHT_PAREN, "Expect ')' after print argument.");
            consume(TokenType.SEMICOLON, "Expect ';' after print statement.");
            return new Stmt.Print(value);
        }

        private List<Stmt> blockStatements() {
            List<Stmt> statements = new ArrayList<>();
            while (!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
                statements.add(statement());
            }
            consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
            return statements;
        }

        private Expr expression() {
            return equality();
        }

        private Expr equality() {
            Expr expr = comparison();
            while (match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
                Token op = previous();
                Expr right = comparison();
                expr = new Expr.Binary(expr, op, right);
            }
            return expr;
        }

        private Expr comparison() {
            Expr expr = term();
            while (match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
                Token op = previous();
                Expr right = term();
                expr = new Expr.Binary(expr, op, right);
            }
            return expr;
        }

        private Expr term() {
            Expr expr = factor();
            while (match(TokenType.PLUS, TokenType.MINUS)) {
                Token op = previous();
                Expr right = factor();
                expr = new Expr.Binary(expr, op, right);
            }
            return expr;
        }

        private Expr factor() {
            Expr expr = unary();
            while (match(TokenType.STAR, TokenType.SLASH)) {
                Token op = previous();
                Expr right = unary();
                expr = new Expr.Binary(expr, op, right);
            }
            return expr;
        }

        private Expr unary() {
            if (match(TokenType.BANG, TokenType.MINUS)) {
                Token op = previous();
                Expr right = unary();
                return new Expr.Unary(op, right);
            }
            return primary();
        }

        private Expr primary() {
            if (match(TokenType.NUMBER)) {
                return new Expr.Literal(previous().literal);
            }
            if (match(TokenType.IDENTIFIER)) {
                return new Expr.Variable(previous().lexeme);
            }
            if (match(TokenType.LEFT_PAREN)) {
                Expr expr = expression();
                consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
                return new Expr.Grouping(expr);
            }
            throw error(peek(), "Expect expression.");
        }

        // utility parse helpers:
        private boolean match(TokenType... types) {
            for (TokenType t : types) {
                if (check(t)) {
                    advance();
                    return true;
                }
            }
            return false;
        }

        private Token consume(TokenType type, String message) {
            if (check(type)) return advance();
            throw error(peek(), message + " at line " + peek().line);
        }

        private ParseException error(Token token, String message) {
            return new ParseException("Line " + token.line + " - " + message + " (got " + token.type + ")");
        }

        private boolean check(TokenType type) {
            if (isAtEnd()) return false;
            return peek().type == type;
        }

        private boolean checkNext(TokenType type) {
            if (current + 1 >= tokens.size()) return false;
            return tokens.get(current + 1).type == type;
        }

        private Token advance() {
            if (!isAtEnd()) current++;
            return previous();
        }

        private boolean isAtEnd() {
            return peek().type == TokenType.EOF;
        }

        private Token peek() {
            return tokens.get(current);
        }

        private Token previous() {
            return tokens.get(current - 1);
        }
    }

    // -------------------------
    // AST Node types (Expr, Stmt)
    // -------------------------
    abstract static class Expr {
        interface Visitor<R> {
            R visitBinary(Binary e);
            R visitGrouping(Grouping e);
            R visitLiteral(Literal e);
            R visitUnary(Unary e);
            R visitVariable(Variable e);
        }
        static class Binary extends Expr {
            final Expr left; final Token operator; final Expr right;
            Binary(Expr left, Token operator, Expr right) { this.left = left; this.operator = operator; this.right = right; }
        }
        static class Grouping extends Expr {
            final Expr expression;
            Grouping(Expr expression) { this.expression = expression; }
        }
        static class Literal extends Expr {
            final Object value;
            Literal(Object value) { this.value = value; }
        }
        static class Unary extends Expr {
            final Token operator; final Expr right;
            Unary(Token operator, Expr right) { this.operator = operator; this.right = right; }
        }
        static class Variable extends Expr {
            final String name;
            Variable(String name) { this.name = name; }
        }
    }

    abstract static class Stmt {
        static class Expression extends Stmt {
            final Expr expression;
            Expression(Expr expression) { this.expression = expression; }
        }
        static class Print extends Stmt {
            final Expr expression;
            Print(Expr expression) { this.expression = expression; }
        }
        static class Var extends Stmt {
            final String name;
            final Expr initializer;
            Var(String name, Expr initializer) { this.name = name; this.initializer = initializer; }
        }
        static class Block extends Stmt {
            final List<Stmt> statements;
            Block(List<Stmt> statements) { this.statements = statements; }
        }
        static class If extends Stmt {
            final Expr condition; final Stmt thenBranch; final Stmt elseBranch;
            If(Expr cond, Stmt thenB, Stmt elseB) { this.condition = cond; this.thenBranch = thenB; this.elseBranch = elseB; }
        }
        static class While extends Stmt {
            final Expr condition; final Stmt body;
            While(Expr c, Stmt b) { this.condition = c; this.body = b; }
        }
    }

    // -------------------------
    // AST pretty-printer (for demo)
    // -------------------------
    static class AstPrinter implements Expr.Visitor<String> {
        String print(Stmt s) {
            if (s instanceof Stmt.Expression) {
                return "ExprStmt: " + printExpr(((Stmt.Expression) s).expression);
            } else if (s instanceof Stmt.Print) {
                return "Print: " + printExpr(((Stmt.Print) s).expression);
            } else if (s instanceof Stmt.Var) {
                Stmt.Var v = (Stmt.Var) s;
                return "Var " + v.name + " = " + printExpr(v.initializer);
            } else if (s instanceof Stmt.Block) {
                StringBuilder sb = new StringBuilder("Block {\n");
                for (Stmt st : ((Stmt.Block) s).statements) sb.append("  ").append(print(st)).append("\n");
                sb.append("}");
                return sb.toString();
            } else if (s instanceof Stmt.If) {
                Stmt.If i = (Stmt.If) s;
                return "If (" + printExpr(i.condition) + ") Then: [" + print(i.thenBranch) + "] Else: [" + (i.elseBranch != null ? print(i.elseBranch) : "null") + "]";
            } else if (s instanceof Stmt.While) {
                Stmt.While w = (Stmt.While) s;
                return "While (" + printExpr(w.condition) + ") Body: [" + print(w.body) + "]";
            } else {
                return "UnknownStmt";
            }
        }

        String printExpr(Expr e) {
            if (e instanceof Expr.Binary) {
                Expr.Binary b = (Expr.Binary) e;
                return "(" + printExpr(b.left) + " " + b.operator.lexeme + " " + printExpr(b.right) + ")";
            } else if (e instanceof Expr.Grouping) {
                return "(group " + printExpr(((Expr.Grouping) e).expression) + ")";
            } else if (e instanceof Expr.Literal) {
                Object v = ((Expr.Literal) e).value;
                return v == null ? "nil" : v.toString();
            } else if (e instanceof Expr.Unary) {
                Expr.Unary u = (Expr.Unary) e;
                return "(" + u.operator.lexeme + printExpr(u.right) + ")";
            } else if (e instanceof Expr.Variable) {
                return ((Expr.Variable) e).name;
            } else {
                return "unknownExpr";
            }
        }

        @Override
        public String visitBinary(Expr.Binary e) { return printExpr(e); }
        @Override
        public String visitGrouping(Expr.Grouping e) { return printExpr(e); }
        @Override
        public String visitLiteral(Expr.Literal e) { return printExpr(e); }
        @Override
        public String visitUnary(Expr.Unary e) { return printExpr(e); }
        @Override
        public String visitVariable(Expr.Variable e) { return printExpr(e); }
    }
}
