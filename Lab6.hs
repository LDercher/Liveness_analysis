module Lab6 where

import LL.Language
import Data.List
import Data.Maybe

{-------------------------------------------------------------------------------

LAB 6: Control flow and data flow, part 1.

DUE: Thursday, March 29, 11:59 PM

This lab asks you to start exploring our LLVMlite language, and establishing the
foundation for doing control-flow and data-flow analyses of LLVM functions.  You
will write functions to compute the predecessors and uses/definitions for the
blocks of individual functions.

FILES:

  LL/Language.hs - the definition of LLVMlite.  You should familiarize yourself
    with the definitions in this file, and how they map to your understanding of
    LLVM from the slides and previous labs.

  LL/Parser.hs, LL/Lexer.hs - parsing support for LLVMlite.  These files are
    tool-generated, and so are not for human consumption.

  Lab6.hs - this assignment.

  Main.hs - driver code.

  samples/inclass.ll - an LLVMlite sample, based on the data flow lectures in
    class.

TESTING: The distribution includes a file "Main.hs".  You should be able to load
this file into GHCi (or compile it with GHC) and then use it to analyze the
sample LLVM file (or any others you might want to create).  Here is a sample
interaction:

    λ> :l Main.hs
    [1 of 5] Compiling LL.Language      ( LL\Language.hs, interpreted )
    [2 of 5] Compiling LL.Lexer         ( LL\Lexer.hs, interpreted )
    [3 of 5] Compiling LL.Parser        ( LL\Parser.hs, interpreted )
    [4 of 5] Compiling Lab6             ( Lab6.hs, interpreted )
    [5 of 5] Compiling Main             ( Main.hs, interpreted )
    Ok, modules loaded: LL.Language, LL.Lexer, LL.Parser, Lab6, Main.
    Collecting type info for 5 module(s) ...
    λ> :main "samples\\inclass.ll"
    === f ===
    --- ^ ---
        Predecessors:
    ...

Note that the LLVM format supported by the tests code is our LLVMlite, and so
is significantly less than full LLVM.  In addition to limiting the range of
types that can be used, the parser also insists on explicit names for
everything, and does not support many of the additional options to various
commands (including the "align" option on loads, the "nsw" option on arithmetic,
and the various annotations that come with functions).

SUBMISSION INSTRUCTIONS: Submit *only* Lab6.hs.  Do not submit any of the other
distributed files, any of your test data, &c.

-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------

PROBLEM 1: predecessors

For this problem, you will write a function `predecessors` that takes a Cfg
(control-flow graph, see definition in LL/Language.hs), and returns a map from
each block's name to a list of its predecessors.  For example, in function `f`
in the sample, block 25 has predecessors 21 and 23, so your predecessors
function should return a list that includes the pair ("25", [("21", "23")]).

NOTE: The first block in a Cfg is not named; LLVM does automatically name the
block for predecessor listing purposes, but does so inconsistently.  To be
consistent, while avoiding overlap with any names that could legally be used for
other blocks, you should name the first block "^".

-------------------------------------------------------------------------------}



predecessors :: Cfg -> [(String, [String])]
predecessors (first, rest) = map predator blocks
                where blocks = ("^", first) : rest
                      predator (s, _) = (s, catMaybes (map (findInBlock s) blocks))


--helper function to find occurance of string in string block pair block
findInBlock :: String -> (String,Block) -> Maybe String
findInBlock s (n,(_,Ret _ t)) = Nothing
findInBlock s (n,(_,Bra i)) = if (s == i) then Just n else Nothing
findInBlock s (n,(_,CBr o x i)) = if ((s == i) || (s == x) ) then Just n else Nothing

successors :: Cfg -> [(String,[String])]
successors (first, rest) = map alien blocks
                    where blocks = ("^",first) :rest
                          alien (s,_) = (s, concatMap findSuccInBlock blocks)

findSuccInBlock :: (String,Block) ->  [String]
findSuccInBlock (n,(_,Ret _ t)) = []
findSuccInBlock (n,(_,Bra i)) = [i]
findSuccInBlock (n,(_,CBr o x i)) = [x,i]                  

{-------------------------------------------------------------------------------

PROBLEM 2: useDefs

For this problem, you will write a function `useDefs` that takes a Cfg and
returns a map from block names to (the pair of) the uses and definitions within
that block.  For example, in function `f` in the sample, the first block uses
the argument named 0, and defines temporaries 2, 3, 4 and 5, so your `useDefs`
function would return a list that includes the pair ("^", (["0"], ["2", "3",
"4", "5"])).

You should *not* include globals in your uses or definitions.  You *should*
include arguments and temporaries.  (You do not need to worry about where the
arguments are defined.)

You may find it helpful to define a helper function that extracts the uses and
definitions from a single statement.

NOTE: The first block in a Cfg is still not named; keep calling it "^".

-------------------------------------------------------------------------------}

useDefs :: Cfg -> [(String, ([String], [String]))]
useDefs (first, rest) = map (useDefsHelper blocks) blocks --map helper onto Cfg
                          where blocks = ("^",first) : rest

useDefsHelper :: [(String, Block)] -> (String, Block) -> (String,([String],[String]))
useDefsHelper blocks (n, (insts, term)) = (n,(filtUses, defs)) -- for block n gather uses and defs list
                    where defs = concat (map filterDefs insts) --gather all defs
                          uses = concat (map filterUidStr (((filtUseList insts)) ++ filterTermUses term)) --gather all uses
                          filtUses = nub (filter (\n' -> notElem n' defs) uses) --remove uses already in defs

filtUseList::[Instruction] -> [Operand]
filtUseList insts = concatMap filtUses insts

-- pull definitions out of insts
filterDefs :: Instruction -> [String]
filterDefs (Bin def _ _ _ _ ) = [def]
filterDefs (Alloca def _ ) = [def]
filterDefs (Load def _ _ ) = [def]
filterDefs (Store _ _ _ ) = []
filterDefs (Icmp def _ _ _ _ ) = [def]
filterDefs (Call def _ _ [(_, _)]) = [def]
filterDefs (Bitcast def _ _ _ ) = [def]
filterDefs (Gep def _ _ [_]) = [def]


--pull uses out of insts
filtUses :: Instruction -> [Operand]
filtUses (Bin _ _ _ op1 op2) = [op1,op2]
filtUses (Alloca _ _ ) = []
filtUses (Load _ _ op) = [op]
filtUses (Store _ op1 op2) = [op1, op2]
filtUses (Icmp _ _ _ op1 op2) = [op1, op2]
filtUses (Call _ _ _ ops ) = []--map snd ops
filtUses (Bitcast _ _ op _ ) = [op]
filtUses (Gep _ _ op1 ops) = []--op1 : ops

--filter uses out of terminators
filterTermUses :: Terminator -> [Operand]
filterTermUses (Ret _ (Just op)) = [op]
filterTermUses (Ret _ (Nothing)) = []
filterTermUses (Bra _) = []
filterTermUses (CBr op _ _ ) = [op]

--take only Operands of type Uid String
filterUidStr :: Operand -> [String]
filterUidStr (Const _) = []
filterUidStr (Gid _) = []
filterUidStr (Uid s) = [s]