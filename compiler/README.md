# Haskell Lexer for Simple Assembly Language

This Haskell project implements a simple **lexer** for parsing a minimal assembly language. The lexer is designed to recognize a few basic tokens like instructions (`mov`, `hlt`), commas, and literal integers.

### Table of Contents

1. [Project Description](#project-description)
2. [Lexer Overview](#lexer-overview)
3. [How It Works](#how-it-works)
4. [Usage](#usage)
5. [Running the Program](#running-the-program)
6. [Building and Testing](#building-and-testing)
7. [License](#license)

---

### Project Description

This project demonstrates a basic lexer (or lexical analyzer) written in Haskell. It reads a string of assembly-like code and attempts to identify tokens such as:

- **Instructions**: Like `mov` and `hlt`
- **Literal Integers**
- **Comma** (`,`) for separating arguments

The lexer is built using a custom `Lexer` data type, and functions for parsing specific parts of the input.

---

### Lexer Overview

A lexer breaks down the input into meaningful tokens that can later be processed further in a parser.

The core data structure in this project is:

```haskell
data Lexer a = Lexer { run :: String -> Maybe (a, String) }


### How It Works

The `Lexer` type is a simple wrapper around a function that takes an input string and attempts to match patterns.  
- `char` matches a single character, and `string` matches a specific sequence of characters.  
- The lexer recognizes two instructions: `mov` and `hlt`.  
- The `run` function executes a lexer, applying it to a string input. If the lexer successfully matches a pattern, it returns the matched token and the remaining string; otherwise, it returns `Nothing`.

For example, the lexer will attempt to parse an input like `"mov ax, 5"` and identify the instruction `mov`, followed by a literal value and commas.

---

### Usage

To use the lexer, you can define a lexer for specific tokens and run it over your input string. Here's a simple example:

```haskell
movLexer :: Lexer Token
movLexer = mov

main :: IO ()
main = do
    let input = "mov ax, 5"
    print $ run movLexer input

