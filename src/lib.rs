use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
};

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use thiserror::Error;

#[derive(Debug)]
struct Ast(Union);

#[derive(Debug)]
enum Union {
    Union(Box<Union>, Box<Concat>),
    Concat(Box<Concat>),
}

#[derive(Debug)]
enum Concat {
    Concat(Box<Concat>, Box<Star>),
    Star(Box<Star>),
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
enum Star {
    Star(Box<Terminal>),
    Optional(Box<Terminal>),
    Terminal(Box<Terminal>),
}

#[derive(Debug)]
enum Terminal {
    AnyChar,
    Char(char),
    Group(Box<Ast>),
}

#[derive(Debug, Error)]
enum ParseError {
    #[error("unexpected character: {0}")]
    UnexpectedChar(char),
    #[error("unexpected end of input")]
    UnexpectedEnd,
}

struct Ctx<'a>(&'a str);

impl<'a> Ctx<'a> {
    fn new(s: &'a str) -> Self {
        Self(s)
    }

    fn peek_skip_whitespace(&self) -> Option<char> {
        self.0.chars().find(|&c| c != ' ')
    }

    fn next_skip_whitespace(&mut self) -> Option<char> {
        let chars = self.0.chars();
        for (i, c) in chars.enumerate() {
            if c != ' ' {
                self.0 = &self.0[(i + 1)..];
                return Some(c);
            }
        }
        None
    }

    fn next_with_whitespace(&mut self) -> Option<char> {
        let c = self.0.chars().next();
        if c.is_some() {
            self.0 = &self.0[1..];
        }
        c
    }
}

trait Parse {
    fn parse(chars: &mut Ctx) -> Result<Self, ParseError>
    where
        Self: Sized;
}

impl Parse for Ast {
    fn parse(chars: &mut Ctx) -> Result<Self, ParseError> {
        Union::parse(chars).map(Ast)
    }
}

impl Parse for Union {
    fn parse(chars: &mut Ctx) -> Result<Self, ParseError> {
        let mut left = Union::Concat(Box::new(Concat::parse(chars)?));
        while let Some('+') = chars.peek_skip_whitespace() {
            chars.next_skip_whitespace();
            let right = Concat::parse(chars)?;
            left = Union::Union(Box::new(left), Box::new(right));
        }
        Ok(left)
    }
}

impl Parse for Concat {
    fn parse(chars: &mut Ctx) -> Result<Self, ParseError> {
        let mut left = Concat::Star(Box::new(Star::parse(chars)?));
        while let Some(c) = chars.peek_skip_whitespace() {
            if c == '+' {
                break;
            }
            let right = Star::parse(chars)?;
            left = Concat::Concat(Box::new(left), Box::new(right));
        }
        Ok(left)
    }
}

impl Parse for Star {
    fn parse(chars: &mut Ctx) -> Result<Self, ParseError> {
        let left = Terminal::parse(chars)?;
        match chars.peek_skip_whitespace() {
            Some('*') => {
                chars.next_skip_whitespace();
                Ok(Star::Star(Box::new(left)))
            },
            Some('?') => {
                chars.next_skip_whitespace();
                Ok(Star::Optional(Box::new(left)))
            },
            _ => Ok(Star::Terminal(Box::new(left))),
        }
    }
}

impl Parse for Terminal {
    fn parse(chars: &mut Ctx) -> Result<Self, ParseError> {
        match chars.next_skip_whitespace() {
            Some('.') => Ok(Terminal::AnyChar),
            Some('(') => {
                let ast = Ast::parse(chars)?;
                match chars.next_skip_whitespace() {
                    Some(')') => Ok(Terminal::Group(Box::new(ast))),
                    Some(c) => Err(ParseError::UnexpectedChar(c)),
                    None => Err(ParseError::UnexpectedEnd),
                }
            },
            Some('\\') => {
                let c = match chars.next_with_whitespace() {
                    Some(c) => c,
                    None => return Err(ParseError::UnexpectedEnd),
                };
                Ok(Terminal::Char(c))
            },
            Some(c) => Ok(Terminal::Char(c)),
            None => Err(ParseError::UnexpectedEnd),
        }
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Union {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Union::Union(left, right) => write!(f, "({}+{})", left, right),
            Union::Concat(concat) => write!(f, "{}", concat),
        }
    }
}

impl Display for Concat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Concat::Concat(left, right) => write!(f, "({}{})", left, right),
            Concat::Star(star) => write!(f, "{}", star),
        }
    }
}

impl Display for Star {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Star::Star(optional) => write!(f, "({}*)", optional),
            Star::Optional(optional) => write!(f, "({}?)", optional),
            Star::Terminal(optional) => write!(f, "{}", optional),
        }
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Terminal::AnyChar => write!(f, "."),
            Terminal::Char(c) => write!(f, "{}", c),
            Terminal::Group(ast) => write!(f, "({})", ast),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NfaTransitions {
    Epsilon,
    AnyChar,
    Char(char),
}

impl NfaTransitions {
    fn to_dfa(self) -> DfaTransitions {
        match self {
            NfaTransitions::AnyChar => DfaTransitions::AnyChar,
            NfaTransitions::Char(c) => DfaTransitions::Char(c),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct Nfa {
    start: usize,
    accept: usize,
    transitions: Vec<HashSet<(NfaTransitions, usize)>>,
}

impl Nfa {
    fn new() -> Self {
        Self {
            start: 1,
            accept: 0,
            transitions: vec![HashSet::new(), HashSet::new()],
        }
    }

    fn new_state(&mut self) -> usize {
        let state = self.transitions.len();
        self.transitions.push(HashSet::new());
        state
    }

    fn add_transition(&mut self, from: usize, to: usize, epsilon: NfaTransitions) {
        self.transitions[from].insert((epsilon, to));
    }

    fn add_epsilon_transition(&mut self, from: usize, to: usize) {
        self.add_transition(from, to, NfaTransitions::Epsilon);
    }

    fn epsilon_closure(&self, state: usize) -> HashSet<usize> {
        let mut closure = HashSet::new();
        let mut stack = VecDeque::new();
        stack.push_back(state);
        while let Some(state) = stack.pop_front() {
            if closure.contains(&state) {
                continue;
            }
            closure.insert(state);
            for (transition, next) in &self.transitions[state] {
                if *transition == NfaTransitions::Epsilon {
                    stack.push_back(*next);
                }
            }
        }
        closure
    }

    fn to_dfa(&self) -> Dfa {
        Dfa::product_construction(self)
    }
}

trait ToNfa {
    fn add_to_nfa(&self, nfa: &mut Nfa, from: usize, to: usize);
    fn as_nfa(&self) -> Nfa {
        let mut nfa = Nfa::new();
        let start = nfa.start;
        let accept = nfa.accept;
        self.add_to_nfa(&mut nfa, start, accept);
        nfa
    }
}

impl ToNfa for Ast {
    fn add_to_nfa(&self, nfa: &mut Nfa, from: usize, to: usize) {
        self.0.add_to_nfa(nfa, from, to);
    }
}

impl ToNfa for Union {
    fn add_to_nfa(&self, nfa: &mut Nfa, from: usize, to: usize) {
        match self {
            Union::Union(left, right) => {
                left.add_to_nfa(nfa, from, to);
                right.add_to_nfa(nfa, from, to);
            },
            Union::Concat(concat) => concat.add_to_nfa(nfa, from, to),
        }
    }
}

impl ToNfa for Concat {
    fn add_to_nfa(&self, nfa: &mut Nfa, from: usize, to: usize) {
        match self {
            Concat::Concat(left, right) => {
                let mid = nfa.new_state();
                left.add_to_nfa(nfa, from, mid);
                right.add_to_nfa(nfa, mid, to);
            },
            Concat::Star(star) => star.add_to_nfa(nfa, from, to),
        }
    }
}

impl ToNfa for Star {
    fn add_to_nfa(&self, nfa: &mut Nfa, from: usize, to: usize) {
        match self {
            Star::Star(optional) => {
                let mid = nfa.new_state();
                nfa.add_epsilon_transition(from, mid);
                nfa.add_epsilon_transition(mid, to);
                optional.add_to_nfa(nfa, mid, mid);
            },
            Star::Optional(ast) => {
                ast.add_to_nfa(nfa, from, to);
                nfa.add_epsilon_transition(from, to);
            },
            Star::Terminal(optional) => optional.add_to_nfa(nfa, from, to),
        }
    }
}

impl ToNfa for Terminal {
    fn add_to_nfa(&self, nfa: &mut Nfa, from: usize, to: usize) {
        match self {
            Terminal::AnyChar => {
                nfa.add_transition(from, to, NfaTransitions::AnyChar);
            },
            Terminal::Char(c) => nfa.add_transition(from, to, NfaTransitions::Char(*c)),
            Terminal::Group(ast) => ast.add_to_nfa(nfa, from, to),
        }
    }
}

// what if there's a AnyChar transition and a Char transition from a state?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DfaTransitions {
    AnyChar,
    Char(char),
}

#[derive(Debug)]
struct Dfa {
    start: usize,
    accept: usize,
    accept_states: HashSet<usize>,
    transitions: Vec<HashMap<DfaTransitions, usize>>,
}

impl Dfa {
    fn new() -> Self {
        let mut accept_states = HashSet::new();
        accept_states.insert(0);
        Self {
            start: 1,
            accept: 0,
            accept_states,
            transitions: vec![HashMap::new(), HashMap::new()],
        }
    }

    fn new_state(&mut self) -> usize {
        let state = self.transitions.len();
        self.transitions.push(HashMap::new());
        state
    }

    fn add_transition(&mut self, from: usize, to: usize, transition: DfaTransitions) {
        self.transitions[from].insert(transition, to);
    }

    fn product_construction(nfa: &Nfa) -> Self {
        let mut dfa = Dfa::new();
        let initial_states = nfa.epsilon_closure(nfa.start);
        let mut states = HashMap::new();
        states.insert(dfa.start, initial_states);
        states.insert(dfa.accept, HashSet::from_iter([nfa.accept]));
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();
        queue.push_back(dfa.start);
        while let Some(state) = queue.pop_front() {
            if visited.contains(&state) {
                continue;
            }
            visited.insert(state);
            let mut transitions = HashMap::new();
            let s = &states[&state];
            if s.contains(&nfa.accept) {
                dfa.accept_states.insert(state);
            }
            for state in s {
                for transition in &nfa.transitions[*state] {
                    if transition.0 == NfaTransitions::Epsilon {
                        continue;
                    }
                    let next_states = transitions.entry(transition.0).or_insert_with(HashSet::new);
                    next_states.extend(nfa.epsilon_closure(transition.1));
                }
            }
            for (transition, next_states) in transitions {
                let next_state = 'a: {
                    for (state, set) in &states {
                        if set == &next_states {
                            break 'a *state;
                        }
                    }
                    let next_state = dfa.new_state();
                    states.insert(next_state, next_states);
                    next_state
                };
                dfa.add_transition(state, next_state, transition.to_dfa());
                queue.push_back(next_state);
            }
            // for each state, there are multiple possible transitions
            // we need to find the set of states that can be reached by each
            // transition for epsilon moves, we compute the epsilon
            // closure for non-epsilon moves, we compute the set of
            // states that can be reached by the move
            //
            // first set is {start}, we compute a -> {...} for each transition a
            // from {start, epsilon_closure(start)}
            // each set of states is then a state in the dfa
            // we then move to the next set of states
        }
        dfa
    }
}

impl ToTokens for Dfa {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let start = self.start;
        let char_transitions =
            self.transitions
                .iter()
                .enumerate()
                .flat_map(|(from, transitions)| {
                    transitions
                        .iter()
                        .filter(|i| *i.0 != DfaTransitions::AnyChar)
                        .map(move |(transition, to)| match transition {
                            DfaTransitions::Char(c) => {
                                quote! { (#from, #c) => #to, }
                            },
                            _ => unreachable!(),
                        })
                });
        let any_char_transitions =
            self.transitions
                .iter()
                .enumerate()
                .flat_map(|(from, transitions)| {
                    transitions
                        .iter()
                        .filter(|i| *i.0 == DfaTransitions::AnyChar)
                        .map(move |(transition, to)| match transition {
                            DfaTransitions::AnyChar => {
                                quote! { (#from, _) => #to, }
                            },
                            _ => unreachable!(),
                        })
                });
        let accept_states = self.accept_states.iter().collect::<Vec<_>>();
        let accept_states = quote! { #(state == #accept_states)||* };
        tokens.extend(quote! {
            let mut state = #start;
            while let Some(c) = chars.next() {
                state = match (state, c) {
                    #(#char_transitions)*
                    #(#any_char_transitions)*
                    _ => return false,
                };
            }
            #accept_states
        });
    }
}

struct Input {
    name: syn::Ident,
    value: syn::LitStr,
}

impl syn::parse::Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<syn::Token![=>]>()?;
        let value = input.parse()?;
        Ok(Self { name, value })
    }
}

#[proc_macro]
pub fn regex(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as Input);
    let lit = input.value.value();
    let mut chars = Ctx::new(&lit);
    let ast = Ast::parse(&mut chars).unwrap();
    let nfa = ast.as_nfa();
    let dfa = nfa.to_dfa();
    let name = input.name;
    quote! {
        struct #name;

        impl #name {
            fn matches(s: &str) -> bool {
                let mut chars = s.chars();
                #dfa
            }
        }
    }
    .into()
}
