# Kon Language Interpreter

**Kon** is a simple functional language.

Check out the [examples](examples/) for a quick overview.

## Syntax

### Values

Values can be one of the following types:
- unit (e.g. `()`)
- integer (signed 64-bit; e.g. `1776` and `-42`)
- string (e.g. `"Hello!"`)
- boolean (e.g. built-in `true` and `false`)
- lazy expression (e.g. `{123}`)
- function (e.g. built-in `print`)

Since the language does not have statements,
every syntactical construct is an expression that evaluates to a value.

### Comments

Comments start with a `#` and extend until the end of the line:
```
# This is a comment
```

### Calls

All operations are performed using function calls.
A call consists of two expressions side-by-side:
the function to call on the left and its argument on the right:
```
print 123
```

The return value of a call can be another function,
which is how multi-parameter functions are implemented:
```
add 2 3  # evaluated as (add 2) 3
```

### Chains

Multiple expressions can be evaluated sequentially by separating them with semicolons.
Every such expression *chain* is itself an expression that evaluates to its last value:
```
print "Chain evaluates to 5";
add 2 3
```

If the last expression also ends with a semicolon, the chain evaluates to `()` (unit):
```
print "Chain evaluates to ()";
add 2 3;
```

### Parentheses

Parentheses are used to pass the value of a call or a chain to a function:
```
print (not false);  # true
print (print "Inside chain"; 123; 456)  # 456
```

### Variables

Variable names can contain any of `[a-zA-Z0-9_-]`, but must start with one of `[a-zA-Z_]`.
They can be created by passing the desired variable identifier (as a string) and value to `bind`:
```
bind "my-variable-42" 42;
print my-variable-42  # 42
```

Variable bindings are only valid after they are created and last until the end of the chain:
```
# print myvar;  # error
bind "myvar" 999;
print myvar;  # 999
print (bind "abc" "cba"; abc);  # cba
# print abc  # error
```

### Lazy expressions

Lazy expressions are created by using curly braces instead of parentheses
around a chain. At the moment of their creation they capture
the surrounding scope (i.e. all currently bound variables) and
the inner expression (as an abstract syntax tree).
Lazy expressions can be evaluated with `eval`, and their captured
scope can be altered (out-of-place) with `bindl`:
```
bind "myvar" 88;
bind "mylazy" {myvar};
print (eval mylazy);  # 88
bind "myvar" 44;
print (eval mylazy);  # 88
bind "mylazy" (bindl "myvar" 44 mylazy);
print (eval mylazy)  # 44
```

### Functions

Functions are created by passing parameter names (as strings, at least one)
followed by the function body (as a lazy expression) to `func`.
This creates an anonymous function, which can be bound to a variable like any other value:
```
bind "add3" (func "a" "b" "c" {
    bind "sum2" (add a b);
    add sum2 c
})
```

Recursive functions must be bound with `bindr`, which, unlike `bind`,
only accepts functions as its second argument:
```
# bindr "myfunc" 123;  # error
bindr "myfunc" (func "x" {add x (myfunc (mul x 2))})
```

Since function bodies are just lazy expressions, all user-defined functions are also closures.

## Built-in functions and values

#### Interpreter intrinsics

Identifier | Usage example | Meaning
---|---|---
`bind` | `bind "var" 123` | Binds values to variable identifiers
`bindr` | `bindr "rec-func" (func "x" {rec-func x})` | Binds recursive functions to their names
`bindl` | `bindl "var" 123 {var}` | Binds values to variable identifiers inside a lazy expression
`eval` | `eval {123}` | Evaluates a lazy expression
`func` | `func "param0" "param1" {add param0 param1}` | Creates an anonymous function
`if` | `if condition {"condition is true"} {"condition is false"}` | Evaluates second or third argument depending on the value of the first
`print` | `print arg` | Prints a representation of the argument

#### Arithmetic

| Identifier | Usage example | Meaning
---|---|---
`add` | `add lhs rhs` | `lhs + rhs`
`sub` | `sub lhs rhs` | `lhs - rhs`
`mul` | `mul lhs rhs` | `lhs * rhs`
`div` | `div lhs rhs` | `lhs / rhs`
`mod` | `mod lhs rhs` | `lhs % rhs`

#### Comparisons

| Identifier | Usage example | Meaning
---|---|---
`eq` | `eq lhs rhs` | `lhs == rhs`
`lt` | `lt lhs rhs` | `lhs < rhs`
`le` | `le lhs rhs` | `lhs <= rhs`
`gt` | `gt lhs rhs` | `lhs > rhs`
`ge` | `ge lhs rhs` | `lhs >= rhs`

#### Logic

| Identifier | Usage example | Meaning
---|---|---
`or` | `or lhs rhs` | `lhs \|\| rhs`
`and` | `and lhs rhs` | `lhs && rhs`
`not` | `not arg` | `!arg`

#### Constants

Boolean `true` and `false`.

## Extras

Some features that are not available out-of-the-box can be implemented using existing language facilities.
Such implementations of [lists](examples/list.kon) and [records](examples/record.kon)
are included in the [examples](examples/).
