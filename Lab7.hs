{-- 

  author: Luke Dercher
  class: EECS 665
  student id: l446d901

--}


module Lab7 where

import LL.Language
import Data.List
import Data.Maybe
import Lab6

{-------------------------------------------------------------------------------

LAB 7: Control flow and data flow, part 2.

DUE: Thursday, April 5, 11:59 PM

This lab continues the exploration of control flow and data flow for the
LLVMlite language.  You will build on your results from last week's lab to
compute liveness information for LLVM functions.

FILES:

  LL/Language.hs - the definition of LLVMlite.  You should familiarize yourself
    with the definitions in this file, and how they map to your understanding of
    LLVM from the slides and previous labs.

  LL/Parser.hs, LL/Lexer.hs - parsing support for LLVMlite.  These files are
    tool-generated, and so are not for human consumption.

  Lab6.hs - last week's assignment.  You should start with your version of last
    week's assignment.  However, once Lab 6 is due, we will post sample
    solutions and you may use those if you prefer.

  Lab7.hs - this assignment.

  Main.hs - driver code.

  samples/inclass.ll - an LLVMlite sample, based on the data flow lectures in
    class.

TESTING: The distribution includes a file "Main.hs".  You should be able to load
this file into GHCi (or compile it with GHC) and then use it to analyze the
sample LLVM file (or any others you might want to create).  Here is a sample
interaction:

    λ> :l Main.hs
    [1 of 6] Compiling LL.Language      ( LL\Language.hs, interpreted )
    [2 of 6] Compiling LL.Lexer         ( LL\Lexer.hs, interpreted )
    [3 of 6] Compiling LL.Parser        ( LL\Parser.hs, interpreted )
    [4 of 6] Compiling Lab6             ( Lab6.hs, interpreted )
    [5 of 6] Compiling Lab7             ( Lab7.hs, interpreted )
    [6 of 6] Compiling Main             ( Main.hs, interpreted )
    Ok, modules loaded: LL.Language, LL.Lexer, LL.Parser, Lab6, Lab7, Main.
    Collecting type info for 6 module(s) ...
    λ> :main "samples\\inclass.ll"
    === f ===
    --- ^ ---
        Predecessors:  ...
        Used: ...
        Defined: ...
        Live in: ...
        Live out: ...
    --- 6 ---
    ...

Note that the LLVM format supported by the tests code is our LLVMlite, and so
is significantly less than full LLVM.  In addition to limiting the range of
types that can be used, the parser also insists on explicit names for
everything, and does not support many of the additional options to various
commands (including the "align" option on loads, the "nsw" option on arithmetic,
and the various annotations that come with functions).

SUBMISSION INSTRUCTIONS: Submit *only* Lab7.hs.  Do not submit any of the other
distributed files, any of your test data, &c.

-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------

PROBLEM 1: liveness

For this problem, you will write a function `liveness` that takes a `Cfg`, as
well as predecessors and uses-definitions information as computed in Lab 6, and
returns two maps, the first containing the live variables in at each node, and
the second containing the live variables out at each node.  For example, in
function `f`, the first block (following the last lab still called "^") has no
variables live in, and has variables "2", "3", "4", and "5" live out, so the
result of `liveness` should include the pair ("^", []) in the first entry, and
the pair ("^", ["2", "3", "4", "5"]) in the second entry.  Block 6 has variables
2-5 live in and live out, so the results of liveness should include the pair
("6", ["2", "3", "4", "5"]) in both the first and second entires.

Your `liveness` function takes three arguments: a `Cfg` (see LL/Language.hs for
type definition), a map from block names to predecessors of those blocks, and a
map from block names to use and definition information for each block.  For
example, in function `f` in the sample, block 23 has predecessor 9, uses
variables 3 and 5 ,and defines variable 24.  So, the predecessors argument will
include the pair ("23", ["9"]), while the useDefs argument will include the pair
("23", (["3", "5"], ["24"])).  For this lab, you should assume the information
in these arguments is correct.  (This means that you can assume that looking up
valid block names in these lists will succeed.)

In your results, lists of variables will be interpreted as sets---so the lists
["3", "5"], ["5", "3"] and ["5", "3", "5"] are all equivalent.  For your own
sanity, you may wish to keep the lists limited to unique entries.  The `nub`
function, from Data.List, can help.

-------------------------------------------------------------------------------}


-- This type aliases are simply to make the intended behavior of the `liveness`
-- function more apparent from its type signature!
type Predecessors = [(String, [String])] -- str has [strings that call it] (m, [n])
type Successors = [(String, [String])] 
type UseDefs = [(String, ([String], [String]))] -- str, ([uses],[defs])
type Liveness = [(String, [String])]

--in[n](subset)use[n]
--in[n](subset)out[n]-def[n]
--out[n](subset)in[m] where m is successor of n

--livein n = Usedef [(n,[uses],[_])] ++ (out[n] - def[n])
-- in(pred[n]) goes to previous node live out


parseUseDef :: UseDefs -> [String]
parseUseDef [(n,([uses],[defs]))] = [uses]

findUseDefs :: UseDefs -> String -> UseDefs
findUseDefs [(n,([uses],[defs]))] ss = [(ss,fromJust (lookup ss [(n,([uses],[defs]))]))]

keys :: [(String,a)] -> [String]
keys xs = map fst xs

liveness :: Cfg -> Predecessors -> UseDefs -> (Liveness, Liveness)
liveness (first,rest) pred ud = liveness' empty empty (successors (first,rest)) ud
                                            where blocks = ("^",first) : rest
                                                  empty = map (\(n, _) -> (n, [])) blocks
                                                  

liveness' :: Liveness -> Liveness -> Successors ->  UseDefs -> (Liveness, Liveness)
liveness' li lo succ ud = if ((li == li') && (lo == lo'))
                            then (li,lo)
                            else liveness' li' lo' succ ud
                        where li' = map liF li 
                              liF (n, _) = (n,nub (uses ++ (fromJust (lookup n lo) \\ defs)))
                                where (uses, defs) = fromJust (lookup n ud)
                              lo' = map loF lo
                              loF (n,_) = (n, nub( concatMap (\n' -> fromJust (lookup n' li)) (fromJust (lookup n succ)))) 
 