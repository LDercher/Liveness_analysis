module LL.Language where

import Data.Int

--------------------------------------------------------------------------------
-- Types                                                                      --
--------------------------------------------------------------------------------

data Type = Void | I1 | I8 | I32 | I64
          | Ptr Type | Struct [Type] | Array Int64 Type
          | Fun [Type] Type | Named String
  deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- Instructions                                                               --
--------------------------------------------------------------------------------

data Operator = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor
  deriving (Eq, Read, Show)

data Relation = Eq | Neq | Lt | Le | Gt | Ge
  deriving (Eq, Read, Show)

data Operand = Const Int64 | Gid String | Uid String
  deriving (Eq, Read, Show)

-- Note: the result of each instruction that has one (i.e., everything but
-- store) is the FIRST string stored in that constructor.  So, for example, the
-- LLVM instruction
--
--    %1 = mul i64 %0, %0
--
-- would be represented as
--
--    Bin "1" Mul I64 (Uid 0) (Uid 0)

data Instruction
    = Bin String Operator Type Operand Operand  -- %uid = binop t op, op
    | Alloca String Type                        -- %uid = alloca t
    | Load String Type Operand                  -- %uid = load t, t* op
    | Store Type Operand Operand                -- store t op1, t* op2
    | Icmp String Relation Type Operand Operand -- %uid = icmp rel t op1 op2
    | Call String Type String [(Type, Operand)] -- %uid = call ret_ty name(t1 op1, t2 op2, ...)
    | Bitcast String Type Operand Type          -- %uid = bitcast t1 op to t2
    | Gep String Type Operand [Operand]         -- %uid = getelementptr t op, i64 op1, i64 op2
                                                --    .. or i32, if accessing struct...
  deriving (Eq, Read, Show)

data Terminator
    = Ret Type (Maybe Operand)                  -- return argument *only* optional if type is void
    | Bra String
    | CBr Operand String String
  deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- Programs                                                                   --
--------------------------------------------------------------------------------

type Block = ([Instruction], Terminator)

type Cfg = (Block, [(String, Block)])   -- first (unnamed) block, remaining blocks (with names)

type Function = ([(Type, String)], Type, Cfg)  -- arguments, return type, body

data Initializer = INull | IGid String | IInt Int64 | IString String
                 | IArray [(Type, Initializer)] | IStruct [(Type, Initializer)]
  deriving (Eq, Read, Show)

data Prog = P { types     :: [(String, Type)]
              , globals   :: [(String, Type, Initializer)]
              , functions :: [(String, Function)]
              , externs   :: [(String, Type)] }
  deriving (Eq, Read, Show)
