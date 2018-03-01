module Libby

export
data Nat = Z | S Nat

export
inc : Nat -> Nat -> Nat
inc Z y = y
inc y Z = y
inc (S k) y = S (inc k y)

