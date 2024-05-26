# dfa-regex

Create a DFA from a regular expression and use it for matching strings.

## Usage

```rust
use dfa_regex::regex;

regex!(Foo => "foo");

assert!(Foo::matches("foo"));
assert!(!Foo::matches("bar"));
```
