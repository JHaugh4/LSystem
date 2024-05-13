module LSystem where

type Rule     = Char -> String
type Axiom    = String

lSystem :: Rule -> Axiom -> [String]
lSystem rule = iterate (concatMap rule)