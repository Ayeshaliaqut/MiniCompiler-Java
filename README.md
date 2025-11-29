# Mini Syntax Analyzer (Java)

This is a simple **recursive-descent syntax analyzer** written in Java. It parses basic arithmetic expressions using the following grammar:

```
Expr  → Term Expr'
Expr' → + Term Expr' | ε
Term  → Factor Term'
Term' → * Factor Term' | ε
Factor → id
```

### ✔ What it Does

* Reads a tokenized input (like: `id + id * id`)
* Checks if it follows the grammar rules
* Prints **"Parsing successful!"** if the syntax is valid
* Throws a clear syntax error message if something is wrong

### ▶ How to Run

1. Save the Java code in a file named `SyntaxAnalyzer.java`.
2. Compile:

   ```bash
   javac SyntaxAnalyzer.java
   ```
3. Run:

   ```bash
   java SyntaxAnalyzer
   ```

### Customize

You can change the input expression inside the `main()` function:

```java
String input = "id + id * id";
```

Replace it with any expression you want to test.

###  Example Valid Inputs

* `id`
* `id + id`
* `id * id + id`
* `id + id * id * id`

###  Example Invalid Inputs

* `+ id id`
* `id *`
* `id + + id`

###  License

Free to use anywhere. No restrictions.
