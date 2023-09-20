# MLscript Syntax Highlight

Syntax highlighting support for the [MLscript][mlscript] programming language.

## Features

- Highlight MLscript source files (ending with `.mls`).
- Highlight most keywords and identifiers.
- Highlight comments created by `DiffTests`.
- This is _not_ a language server.

## Extension Settings

This extension does not have settings currently.

## Known Issues

- Some token scopes might be wrong.
- Types, terms and declarations in comments are not highlighted.

## Release Notes

### 0.0.4

#### Additions

- Highlight modifiers `declare` and `virtual` for function declarations.
- Highlight modifiers `declare`, `private`, `abstract`, and `data` for class
  declarations.
- Highlight modifier `private` for trait declarations.
- Highlight modifiers `declare` and `private` for module declarations.
- Highlight `mixin` declarations.
- Highlight keyword `super`.
- Highlight modifiers `declare` and `virtual` for function declarations.
- Highlight `val` declarations with modifiers `declare`, `private`, and `lazy`.
- Highlight `in`/`out` variance modifiers and type variables starting with a 
  single quote in parameter lists.
- Highlight field selections. For example, the `size` in `this.size`.
  Note that the highlighting isn’t based on semantics, it simply matches identifiers after a period.

#### Changes

- Set the scope of `[ERROR]` and `[WARNING]` to `markup.bold` as I found many
  VSCode themes does not colorize scope `message.error`.
- Set `this`, `true`, `false`, and `null` to the correct `constant` scope.
- Do not automatically close single quotations becuase they are also used in
  variable names and type variable names. (The [`notIn` property of 
  `autoClosingPairs`][not-in] does not support user-defined scope names.)

[not-in]: https://code.visualstudio.com/api/language-extensions/language-configuration-guide#autoclosing

### 0.0.3

- Highlight functions with user-defined symbolic operators ([#177][pr-177]).

[pr-177]: https://github.com/hkust-taco/mlscript/pull/177

### 0.0.2

- Support folding error and warning blocks generated by DiffTests.
- Add two simple snippets. I'm just trying it out.
- Fix minor bugs.
  - Only `string` in `stringOf` will be highlighted.
- Add more string escape sequences.

### 0.0.1

Create the extension and add basic syntax support.

[mlscript]: https://github.com/hkust-taco/mlscript
