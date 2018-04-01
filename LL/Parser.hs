{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module LL.Parser where

import LL.Lexer
import LL.Language
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Prog) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Prog)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Operator) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Operator)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Relation) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Relation)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Type) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Type)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([Type]) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([Type])
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Operand) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Operand)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Type) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Type)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([(Type, Operand)]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([(Type, Operand)])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([(Type, Operand)]) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([(Type, Operand)])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Instruction) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Instruction)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Terminator) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Terminator)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([Instruction]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([Instruction])
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (([Instruction], Terminator)) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (([Instruction], Terminator))
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ([(String, Block)]) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ([(String, Block)])
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (String) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (String)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([(String, Block)]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([(String, Block)])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ((Type, String)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ((Type, String))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([(Type, String)]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([(Type, String)])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Initializer) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Initializer)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([(Type, Initializer)]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([(Type, Initializer)])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Prog) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Prog)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ((String, Type)) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ((String, Type))
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ((String, Type, Initializer)) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ((String, Type, Initializer))
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ((String, Function)) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ((String, Function))
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ((String, Type)) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ((String, Type))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x17\x00\x00\x00\x40\x00\x00\x00\x00\x05\x2e\x00\x00\x00\x80\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x80\x02\x17\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x17\x00\x00\x00\x40\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xc0\x05\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x80\x02\x17\x00\x00\x00\x40\x00\x00\x00\x60\x05\x00\x00\x00\x00\x30\x01\x00\x00\x00\x0a\x5c\x00\x00\x00\x00\x01\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x70\x01\x00\x00\x00\x04\x00\x00\x00\x50\xe0\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x81\x0b\x00\x00\x00\x20\x00\x00\x00\x80\x02\x17\x00\x00\x00\x40\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x02\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\xb8\x00\x00\x00\x00\x02\x00\x00\x00\x28\x70\x01\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x02\x00\x00\x00\x00\x98\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xc0\x05\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\xe0\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x20\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x2e\x00\x00\x00\x80\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x03\x7b\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\xb8\x00\x00\x00\x00\x02\x00\x00\x00\x00\x20\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x60\x00\x00\x00\x00\x00\xa0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x04\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x5c\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x70\x01\x00\x00\x00\x04\x00\x00\x00\x50\xe0\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\xe0\x07\x00\x00\x00\x00\x00\x40\x81\x0b\x00\x00\x00\x20\x00\x00\x00\x80\x02\x17\x00\x00\x00\x40\x00\x00\x00\x00\x05\x2e\x00\x00\x00\x80\x00\x00\x00\xc0\x20\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x80\x06\x00\x00\x00\x03\x00\x00\x00\x00\x00\x08\x00\x00\x00\x50\xe0\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x34\x00\x00\x00\x40\x81\x0b\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x80\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x2e\x00\x00\x00\x80\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x40\x03\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x50\xe0\x02\x00\x00\x00\x08\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x34\x00\x00\x00\x40\x81\x0b\x00\x00\x00\x20\x00\x00\x00\x80\x02\x1f\x00\x00\x00\x40\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x40\x03\x00\x00\x00\x00\x00\x00\x00\x00\x80\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x01\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x3e\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x40\x81\x0f\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","prog","op","relation","ty","types","operand","argType","arguments","maybeArguments","instr","term","instrs","block","blocksRev","blockLabel","blocks","param","params","init","inits","decls","tdecl","gdecl","fdecl","edecl","'*'","'('","')'","'['","']'","'{'","'}'","','","'='","':'","X","TO","VOID","I1","I8","I32","I64","LABEL","ADD","SUB","MUL","SHL","LSHR","ASHR","AND","OR","XOR","EQ","NE","LT","LE","GT","GE","ALLOCA","LOAD","STORE","ICMP","CALL","BITCAST","GEP","RET","BR","TYPE","DEFINE","DECLARE","EXTERNAL","GLOBAL","NULL","NUMBER","ID","LID","GID","%eof"]
        bit_start = st * 81
        bit_end = (st + 1) * 81
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..80]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x44\x00\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x15\x00\x13\x00\x1f\x00\xe4\xff\x0e\x00\x09\x00\x4a\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x15\x00\x4e\x00\x2c\x00\x6b\x00\x79\x00\x96\x00\x15\x00\x65\x00\x15\x00\x01\x00\x15\x00\x63\x00\x15\x00\x15\x00\x00\x00\x15\x00\x15\x00\x99\x00\x00\x00\x0f\x00\x9c\x00\xaa\x00\x00\x00\x2e\x00\xab\x00\x63\x00\x00\x00\x15\x00\x15\x00\x00\x00\x00\x00\x00\x00\x01\x00\xa9\x00\xac\x00\x00\x00\x00\x00\xad\x00\x15\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\xa7\x00\x15\x00\x1c\x00\x75\x00\x6f\x00\x15\x00\xae\x00\x47\x00\x07\x00\x6f\x00\xaf\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x38\x00\x7c\x00\x00\x00\x00\x00\x1a\x00\x7f\x00\x07\x00\xfa\xff\x00\x00\x1c\x00\xb0\x00\xb3\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x46\x00\x61\x00\x46\x00\x46\x00\x46\x00\x72\x00\x07\x00\x0d\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\xa1\x00\x07\x00\x46\x00\x1c\x00\x00\x00\x00\x00\x00\x00\xb4\x00\xa3\x00\x00\x00\x07\x00\xb5\x00\x46\x00\x07\x00\xb2\x00\xb6\x00\x46\x00\x07\x00\x46\x00\x10\x00\xb7\x00\x07\x00\x1a\x00\x00\x00\x83\x00\xb8\x00\x00\x00\x00\x00\x1a\x00\xa1\x00\x1a\x00\x00\x00\xbb\x00\x00\x00\xa1\x00\xb9\x00\x10\x00\x00\x00\xbc\x00\x00\x00\xba\x00\x90\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x04\x00\xbd\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x00\xc3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\xc5\x00\xbe\x00\xc6\x00\x00\x00\xa6\x00\xc7\x00\x00\x00\xa8\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x14\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x17\x00\x92\x00\xc1\x00\x7e\x00\xca\x00\x00\x00\xcd\x00\xce\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\x00\x00\x00\x00\x00\x00\x00\xcf\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x00\xd6\x00\xd8\x00\xd9\x00\xda\x00\xdb\x00\x00\x00\xdc\x00\x00\x00\xdd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\xdf\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\xe1\x00\xe2\x00\x00\x00\x00\x00\xe3\x00\xe4\x00\xe5\x00\x60\x00\x00\x00\xe6\x00\xe7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\xe9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xb9\xff\x00\x00\xfe\xff\x00\x00\xb8\xff\xb7\xff\xb6\xff\xb5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\xed\xff\xec\xff\xeb\xff\xe6\xff\x00\x00\xea\xff\x00\x00\x00\x00\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\x00\x00\x00\x00\xe8\xff\x00\x00\xc4\xff\x00\x00\xe7\xff\x00\x00\xc3\xff\x00\x00\xe5\xff\x00\x00\x00\x00\xb0\xff\xb3\xff\xbc\xff\xbc\xff\xc1\xff\xbf\xff\xc0\xff\x00\x00\x00\x00\x00\x00\xb1\xff\xe9\xff\x00\x00\xc4\xff\xc5\xff\xc2\xff\xce\xff\xbe\xff\xbd\xff\xbb\xff\xbc\xff\xce\xff\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\x00\x00\xc7\xff\xc8\xff\xcc\xff\x00\x00\x00\x00\xcd\xff\xba\xff\x00\x00\x00\x00\x00\x00\xee\xff\xb2\xff\xce\xff\x00\x00\x00\x00\xe3\xff\xe2\xff\xe1\xff\x00\x00\xfd\xff\xfc\xff\xfb\xff\xfa\xff\xf9\xff\xf8\xff\xf7\xff\xf6\xff\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xf3\xff\xf2\xff\xf1\xff\xf0\xff\xef\xff\x00\x00\xd9\xff\x00\x00\x00\x00\xce\xff\xca\xff\xd1\xff\xd0\xff\x00\x00\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\x00\x00\xda\xff\xd8\xff\x00\x00\xdf\xff\x00\x00\xdb\xff\x00\x00\xe0\xff\xd4\xff\x00\x00\x00\x00\xd5\xff\xde\xff\xd6\xff\x00\x00\x00\x00\x00\x00\xd3\xff\xdd\xff\xcf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x07\x00\x01\x00\x02\x00\x00\x00\x04\x00\x35\x00\x06\x00\x01\x00\x02\x00\x01\x00\x02\x00\x01\x00\x02\x00\x01\x00\x02\x00\x01\x00\x02\x00\x2e\x00\x2f\x00\x04\x00\x03\x00\x06\x00\x03\x00\x14\x00\x04\x00\x03\x00\x06\x00\x09\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x0d\x00\x0e\x00\x0f\x00\x13\x00\x11\x00\x13\x00\x09\x00\x03\x00\x13\x00\x31\x00\x32\x00\x01\x00\x02\x00\x01\x00\x02\x00\x30\x00\x31\x00\x05\x00\x08\x00\x34\x00\x10\x00\x11\x00\x31\x00\x2b\x00\x33\x00\x34\x00\x04\x00\x34\x00\x06\x00\x34\x00\x24\x00\x34\x00\x33\x00\x33\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x33\x00\x11\x00\x04\x00\x31\x00\x06\x00\x33\x00\x34\x00\x33\x00\x02\x00\x10\x00\x11\x00\x0d\x00\x0e\x00\x0f\x00\x09\x00\x11\x00\x0b\x00\x0c\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x03\x00\x01\x00\x02\x00\x06\x00\x07\x00\x08\x00\x22\x00\x23\x00\x33\x00\x25\x00\x26\x00\x27\x00\x28\x00\x2c\x00\x2d\x00\x07\x00\x01\x00\x02\x00\x01\x00\x02\x00\x33\x00\x34\x00\x33\x00\x08\x00\x31\x00\x08\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x03\x00\x0b\x00\x03\x00\x06\x00\x07\x00\x06\x00\x07\x00\x0e\x00\x0d\x00\x0e\x00\x0f\x00\x12\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x2f\x00\x0b\x00\x0c\x00\x09\x00\x02\x00\x0b\x00\x0c\x00\x09\x00\x03\x00\x0b\x00\x29\x00\x2a\x00\x31\x00\x32\x00\x01\x00\x02\x00\x08\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x03\x00\x08\x00\x07\x00\x05\x00\x33\x00\x06\x00\x02\x00\x12\x00\x33\x00\x09\x00\x07\x00\x0a\x00\x0a\x00\x08\x00\x08\x00\x08\x00\x03\x00\x08\x00\x08\x00\x08\x00\x0c\x00\x33\x00\x08\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x0a\x00\x12\x00\x03\x00\x01\x00\x03\x00\x12\x00\x14\x00\x12\x00\x05\x00\x05\x00\x05\x00\x0e\x00\x03\x00\x03\x00\x03\x00\x02\x00\xff\xff\x03\x00\x03\x00\x03\x00\xff\xff\x03\x00\x05\x00\x03\x00\x05\x00\x03\x00\x05\x00\x03\x00\x05\x00\x03\x00\x05\x00\xff\xff\x05\x00\x05\x00\x05\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xd2\xff\x18\x00\x19\x00\x03\x00\x34\x00\xff\xff\x35\x00\x18\x00\x19\x00\x18\x00\x19\x00\x18\x00\x19\x00\x18\x00\x19\x00\x18\x00\x19\x00\x20\x00\x21\x00\x10\x00\x38\x00\x11\x00\x38\x00\x02\x00\x10\x00\x38\x00\x11\x00\x0e\x00\x12\x00\x13\x00\x14\x00\x9f\x00\x15\x00\x12\x00\x13\x00\x14\x00\x3a\x00\x15\x00\x39\x00\x0d\x00\x2b\x00\x56\x00\xd2\xff\xd2\xff\x18\x00\x19\x00\x18\x00\x19\x00\x36\x00\x37\x00\x3d\x00\x28\x00\x38\x00\x2c\x00\x2d\x00\x60\x00\x1f\x00\x61\x00\x62\x00\x10\x00\x1e\x00\x11\x00\x1a\x00\x4a\x00\x8c\x00\x40\x00\x16\x00\x2b\x00\x5b\x00\x13\x00\x14\x00\x16\x00\x15\x00\x10\x00\x60\x00\x11\x00\x61\x00\x62\x00\x4b\x00\x29\x00\x2c\x00\x40\x00\x12\x00\x13\x00\x14\x00\x46\x00\x15\x00\x47\x00\x48\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x9a\x00\x18\x00\x19\x00\x9b\x00\x9c\x00\x9d\x00\x6d\x00\x6e\x00\x16\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x09\x00\x0a\x00\x27\x00\x18\x00\x19\x00\x18\x00\x19\x00\x0b\x00\x0c\x00\x16\x00\x8e\x00\x1d\x00\x8a\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x7c\x00\x9a\x00\x26\x00\x9a\x00\x9b\x00\xa8\x00\x9b\x00\xa9\x00\x58\x00\x4d\x00\x4e\x00\x4f\x00\x59\x00\x04\x00\x05\x00\x06\x00\x07\x00\x46\x00\x23\x00\x47\x00\x81\x00\x46\x00\x25\x00\x47\x00\x86\x00\x46\x00\x2b\x00\x55\x00\x54\x00\x55\x00\x51\x00\x52\x00\x18\x00\x19\x00\x3f\x00\x1a\x00\x1b\x00\x1a\x00\x29\x00\x1a\x00\x30\x00\x1a\x00\x2e\x00\x3e\x00\x3c\x00\x46\x00\x44\x00\x43\x00\x84\x00\x42\x00\x91\x00\x96\x00\x97\x00\x4c\x00\x5c\x00\x5d\x00\x81\x00\x80\x00\x86\x00\x94\x00\xa3\x00\x9a\x00\xa6\x00\xa2\x00\x90\x00\xab\x00\xa8\x00\x16\x00\x0e\x00\x23\x00\x21\x00\x31\x00\x2f\x00\x52\x00\xa7\x00\x4c\x00\x62\x00\x59\x00\x32\x00\x02\x00\x44\x00\x5e\x00\x84\x00\x82\x00\x5d\x00\x7e\x00\x7d\x00\x7c\x00\x75\x00\x00\x00\x74\x00\x73\x00\x72\x00\x00\x00\x8a\x00\x8c\x00\x87\x00\x88\x00\x92\x00\x94\x00\x8e\x00\x91\x00\x9f\x00\xa0\x00\x00\x00\x98\x00\x97\x00\xa4\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 79) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79)
	]

happy_n_terms = 54 :: Int
happy_n_nonterms = 25 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (let p = happy_var_1 in P (reverse (types p)) (reverse (globals p)) (reverse (functions p)) (reverse (externs p))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  happyIn5
		 (Add
	)

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  happyIn5
		 (Sub
	)

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  happyIn5
		 (Mul
	)

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  happyIn5
		 (Shl
	)

happyReduce_6 = happySpecReduce_1  1# happyReduction_6
happyReduction_6 happy_x_1
	 =  happyIn5
		 (Lshr
	)

happyReduce_7 = happySpecReduce_1  1# happyReduction_7
happyReduction_7 happy_x_1
	 =  happyIn5
		 (Ashr
	)

happyReduce_8 = happySpecReduce_1  1# happyReduction_8
happyReduction_8 happy_x_1
	 =  happyIn5
		 (And
	)

happyReduce_9 = happySpecReduce_1  1# happyReduction_9
happyReduction_9 happy_x_1
	 =  happyIn5
		 (Or
	)

happyReduce_10 = happySpecReduce_1  1# happyReduction_10
happyReduction_10 happy_x_1
	 =  happyIn5
		 (Xor
	)

happyReduce_11 = happySpecReduce_1  2# happyReduction_11
happyReduction_11 happy_x_1
	 =  happyIn6
		 (Eq
	)

happyReduce_12 = happySpecReduce_1  2# happyReduction_12
happyReduction_12 happy_x_1
	 =  happyIn6
		 (Neq
	)

happyReduce_13 = happySpecReduce_1  2# happyReduction_13
happyReduction_13 happy_x_1
	 =  happyIn6
		 (Lt
	)

happyReduce_14 = happySpecReduce_1  2# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn6
		 (Le
	)

happyReduce_15 = happySpecReduce_1  2# happyReduction_15
happyReduction_15 happy_x_1
	 =  happyIn6
		 (Gt
	)

happyReduce_16 = happySpecReduce_1  2# happyReduction_16
happyReduction_16 happy_x_1
	 =  happyIn6
		 (Ge
	)

happyReduce_17 = happySpecReduce_1  3# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn7
		 (Void
	)

happyReduce_18 = happySpecReduce_1  3# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn7
		 (I1
	)

happyReduce_19 = happySpecReduce_1  3# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn7
		 (I8
	)

happyReduce_20 = happySpecReduce_1  3# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn7
		 (I64
	)

happyReduce_21 = happySpecReduce_2  3# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (Ptr happy_var_1
	)}

happyReduce_22 = happyReduce 5# 3# happyReduction_22
happyReduction_22 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TokLiteral happy_var_2) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn7
		 (Array happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_23 = happySpecReduce_3  3# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (Struct happy_var_2
	)}

happyReduce_24 = happyReduce 4# 3# happyReduction_24
happyReduction_24 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (Fun happy_var_3 happy_var_1
	) `HappyStk` happyRest}}

happyReduce_25 = happySpecReduce_1  3# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	happyIn7
		 (Named happy_var_1
	)}

happyReduce_26 = happySpecReduce_3  4# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_27 = happySpecReduce_1  4# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ([happy_var_1]
	)}

happyReduce_28 = happySpecReduce_1  5# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokLiteral happy_var_1) -> 
	happyIn9
		 (Const happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  5# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	happyIn9
		 (Uid happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  5# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokGID happy_var_1) -> 
	happyIn9
		 (Gid happy_var_1
	)}

happyReduce_31 = happySpecReduce_1  6# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn10
		 (I32
	)

happyReduce_32 = happySpecReduce_1  6# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_33 = happySpecReduce_2  7# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 ([(happy_var_1, happy_var_2)]
	)}}

happyReduce_34 = happyReduce 4# 7# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	happyIn11
		 ((happy_var_1, happy_var_2) : happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_35 = happySpecReduce_0  8# happyReduction_35
happyReduction_35  =  happyIn12
		 ([]
	)

happyReduce_36 = happySpecReduce_1  8# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (happy_var_1
	)}

happyReduce_37 = happyReduce 7# 9# happyReduction_37
happyReduction_37 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut9 happy_x_5 of { happy_var_5 -> 
	case happyOut9 happy_x_7 of { happy_var_7 -> 
	happyIn13
		 (Bin happy_var_1 happy_var_3 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}}

happyReduce_38 = happyReduce 4# 9# happyReduction_38
happyReduction_38 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (Alloca happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_39 = happyReduce 7# 9# happyReduction_39
happyReduction_39 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut9 happy_x_7 of { happy_var_7 -> 
	happyIn13
		 (Load happy_var_1 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_40 = happyReduce 6# 9# happyReduction_40
happyReduction_40 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_6 of { happy_var_6 -> 
	happyIn13
		 (Store happy_var_2 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_41 = happyReduce 8# 9# happyReduction_41
happyReduction_41 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut6 happy_x_4 of { happy_var_4 -> 
	case happyOut7 happy_x_5 of { happy_var_5 -> 
	case happyOut9 happy_x_6 of { happy_var_6 -> 
	case happyOut9 happy_x_8 of { happy_var_8 -> 
	happyIn13
		 (Icmp happy_var_1 happy_var_4 happy_var_5 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}}

happyReduce_42 = happyReduce 8# 9# happyReduction_42
happyReduction_42 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { (TokGID happy_var_5) -> 
	case happyOut12 happy_x_7 of { happy_var_7 -> 
	happyIn13
		 (Call happy_var_1 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_43 = happyReduce 7# 9# happyReduction_43
happyReduction_43 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut9 happy_x_5 of { happy_var_5 -> 
	case happyOut7 happy_x_7 of { happy_var_7 -> 
	happyIn13
		 (Bitcast happy_var_1 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_44 = happyReduce 9# 9# happyReduction_44
happyReduction_44 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut9 happy_x_7 of { happy_var_7 -> 
	case happyOut11 happy_x_9 of { happy_var_9 -> 
	happyIn13
		 (Gep happy_var_1 happy_var_4 happy_var_7 (map snd happy_var_9)
	) `HappyStk` happyRest}}}}

happyReduce_45 = happySpecReduce_2  10# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  happyIn14
		 (Ret Void Nothing
	)

happyReduce_46 = happySpecReduce_3  10# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (Ret happy_var_2 (Just happy_var_3)
	)}}

happyReduce_47 = happySpecReduce_3  10# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_3 of { (TokLID happy_var_3) -> 
	happyIn14
		 (Bra happy_var_3
	)}

happyReduce_48 = happyReduce 9# 10# happyReduction_48
happyReduction_48 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_6 of { (TokLID happy_var_6) -> 
	case happyOutTok happy_x_9 of { (TokLID happy_var_9) -> 
	happyIn14
		 (CBr happy_var_3 happy_var_6 happy_var_9
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_0  11# happyReduction_49
happyReduction_49  =  happyIn15
		 ([]
	)

happyReduce_50 = happySpecReduce_2  11# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_51 = happySpecReduce_2  12# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 ((happy_var_1, happy_var_2)
	)}}

happyReduce_52 = happySpecReduce_0  13# happyReduction_52
happyReduction_52  =  happyIn17
		 ([]
	)

happyReduce_53 = happySpecReduce_3  13# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 ([(happy_var_1, happy_var_3)]
	)}}

happyReduce_54 = happyReduce 4# 13# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 ((happy_var_2, happy_var_4) : happy_var_1
	) `HappyStk` happyRest}}}

happyReduce_55 = happySpecReduce_1  14# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokID happy_var_1) -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  14# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokLiteral happy_var_1) -> 
	happyIn18
		 (show happy_var_1
	)}

happyReduce_57 = happySpecReduce_1  15# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (reverse happy_var_1
	)}

happyReduce_58 = happySpecReduce_2  16# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (TokLID happy_var_2) -> 
	happyIn20
		 ((happy_var_1, happy_var_2)
	)}}

happyReduce_59 = happySpecReduce_0  17# happyReduction_59
happyReduction_59  =  happyIn21
		 ([]
	)

happyReduce_60 = happySpecReduce_1  17# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ([happy_var_1]
	)}

happyReduce_61 = happySpecReduce_3  17# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1  18# happyReduction_62
happyReduction_62 happy_x_1
	 =  happyIn22
		 (INull
	)

happyReduce_63 = happySpecReduce_1  18# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokGID happy_var_1) -> 
	happyIn22
		 (IGid happy_var_1
	)}

happyReduce_64 = happySpecReduce_1  18# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokLiteral happy_var_1) -> 
	happyIn22
		 (IInt happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  18# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (IArray happy_var_2
	)}

happyReduce_66 = happySpecReduce_3  18# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (IStruct happy_var_2
	)}

happyReduce_67 = happySpecReduce_0  19# happyReduction_67
happyReduction_67  =  happyIn23
		 ([]
	)

happyReduce_68 = happySpecReduce_2  19# happyReduction_68
happyReduction_68 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 ([(happy_var_1, happy_var_2)]
	)}}

happyReduce_69 = happyReduce 4# 19# happyReduction_69
happyReduction_69 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 ((happy_var_1, happy_var_2) : happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_70 = happySpecReduce_0  20# happyReduction_70
happyReduction_70  =  happyIn24
		 (P [] [] [] []
	)

happyReduce_71 = happySpecReduce_2  20# happyReduction_71
happyReduction_71 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (let p = happy_var_1 in p { types = happy_var_2 : types p }
	)}}

happyReduce_72 = happySpecReduce_2  20# happyReduction_72
happyReduction_72 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (let p = happy_var_1 in p { globals = happy_var_2 : globals p }
	)}}

happyReduce_73 = happySpecReduce_2  20# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (let p = happy_var_1 in p { functions = happy_var_2 : functions p }
	)}}

happyReduce_74 = happySpecReduce_2  20# happyReduction_74
happyReduction_74 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (let p = happy_var_1 in p { externs = happy_var_2 : externs p }
	)}}

happyReduce_75 = happyReduce 4# 21# happyReduction_75
happyReduction_75 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokLID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 ((happy_var_1, happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_76 = happyReduce 5# 22# happyReduction_76
happyReduction_76 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokGID happy_var_1) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut22 happy_x_5 of { happy_var_5 -> 
	happyIn26
		 ((happy_var_1, happy_var_4, happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_77 = happyReduce 10# 23# happyReduction_77
happyReduction_77 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (TokGID happy_var_3) -> 
	case happyOut21 happy_x_5 of { happy_var_5 -> 
	case happyOut16 happy_x_8 of { happy_var_8 -> 
	case happyOut19 happy_x_9 of { happy_var_9 -> 
	happyIn27
		 ((happy_var_3, (happy_var_5, happy_var_2, (happy_var_8, happy_var_9)))
	) `HappyStk` happyRest}}}}}

happyReduce_78 = happyReduce 6# 24# happyReduction_78
happyReduction_78 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (TokGID happy_var_3) -> 
	case happyOut8 happy_x_5 of { happy_var_5 -> 
	happyIn28
		 ((happy_var_3, Fun happy_var_5 happy_var_2)
	) `HappyStk` happyRest}}}

happyReduce_79 = happyReduce 5# 24# happyReduction_79
happyReduction_79 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokGID happy_var_1) -> 
	case happyOut7 happy_x_5 of { happy_var_5 -> 
	happyIn28
		 ((happy_var_1, happy_var_5)
	) `HappyStk` happyRest}}

happyNewToken action sts stk [] =
	happyDoAction 53# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokStar -> cont 1#;
	TokLParen -> cont 2#;
	TokRParen -> cont 3#;
	TokLBracket -> cont 4#;
	TokRBracket -> cont 5#;
	TokLBrace -> cont 6#;
	TokRBrace -> cont 7#;
	TokComma -> cont 8#;
	TokEquals -> cont 9#;
	TokColon -> cont 10#;
	TokTimes -> cont 11#;
	TokTo -> cont 12#;
	TokVoid -> cont 13#;
	TokI1 -> cont 14#;
	TokI8 -> cont 15#;
	TokI32 -> cont 16#;
	TokI64 -> cont 17#;
	TokLabel -> cont 18#;
	TokAdd -> cont 19#;
	TokSub -> cont 20#;
	TokMul -> cont 21#;
	TokShl -> cont 22#;
	TokLshr -> cont 23#;
	TokAshr -> cont 24#;
	TokAnd -> cont 25#;
	TokOr -> cont 26#;
	TokXor -> cont 27#;
	TokEq -> cont 28#;
	TokNeq -> cont 29#;
	TokLt -> cont 30#;
	TokLe -> cont 31#;
	TokGt -> cont 32#;
	TokGe -> cont 33#;
	TokAlloca -> cont 34#;
	TokLoad -> cont 35#;
	TokStore -> cont 36#;
	TokIcmp -> cont 37#;
	TokCall -> cont 38#;
	TokBitcast -> cont 39#;
	TokGep -> cont 40#;
	TokRet -> cont 41#;
	TokBr -> cont 42#;
	TokType -> cont 43#;
	TokDefine -> cont 44#;
	TokDeclare -> cont 45#;
	TokExternal -> cont 46#;
	TokGlobal -> cont 47#;
	TokNull -> cont 48#;
	TokLiteral happy_dollar_dollar -> cont 49#;
	TokID happy_dollar_dollar -> cont 50#;
	TokLID happy_dollar_dollar -> cont 51#;
	TokGID happy_dollar_dollar -> cont 52#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 53# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> (error . ("Parse error: " ++) . show) tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq



{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "C:\\Program Files\\ghc\\ghc-8.2.2\\lib/include\\ghcversion.h" #-}















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\Garrett\\AppData\\Local\\Temp\\ghc16056_0\\ghc_2.h" #-}










































































































































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
