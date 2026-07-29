{-# LANGUAGE LambdaCase #-}
module LLM where

import Control.Monad.Writer
import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Char (intToDigit) --for serialization to hex

--A simple module for generating LLM.evmc scripts; allows generation from
--an assembly-like monad a la Ecomp.

--Instructions may have an immediate argument, which is either a label or
--constant.
data Short = L Label | R Int
  deriving (Eq,Ord,Read,Show)
--fromInteger sugar
instance Num Short where
  fromInteger = R . fromInteger
  (+) = err
  (-) = err
  (*) = err
  abs = err
  signum = err
err :: a
err = error "All ops but fromInteger are undefined for Short!"
--for simplicity I only have anonymous labels
newtype Label = Label Int
  deriving (Eq,Ord,Read,Show) 
data LLM =
 --Load a 16b constant into r_accum
 Load_k Short
 --Calldataload off a fixed offset
 | CDL_k Short
 --Mload off a fixed offset
 | ML_k Short
 --Mstore to a fixed offset
 | MS_k Short
 --accum <-> r storage:
 | Load_r1
 | Load_r2
 | Load_r3
 | Store_r1
 | Store_r2
 | Store_r3
 | Swap_r1
 | Swap_r2
 | Swap_r3
 --Control flow:
 | Jump_k Short
 | Jumpi_k Short
 --Arithmetic:
 --accum *= r1
 | Mul_dyn
 --accum -= imm
 | Sub_k Short
 --RETURN (off: accum, len: r1) 
 | Return
  deriving (Eq,Ord,Read,Show)

--The LLM codegen monad. Label resolution is done in a second pass.
type Code a = WriterT [LLM] (State CodeS) a
data CodeS = CodeS {codeOffset :: Int,
                    labelCtr :: Int,
                    labelMap :: Map Label Int
                   }
--Actions:
--Emit an instruction and bump the code offset for label placement
instr :: LLM -> Code ()
instr llm = do
  tell [llm]
  modify (\s->s{codeOffset = codeOffset s + byteSize llm})
--Allocate a new label; can be used directly as an argument.
label :: Code Short
label = do
  s <- get
  let n = labelCtr s
  put s{labelCtr = n+1}
  return $ L $ Label n
--Place a label, mapping it to the current code offset
place :: Short -> Code ()
place short =
  case short of
    L lab -> modify $ \s->s{labelMap = M.insert lab (codeOffset s) $
                                       labelMap s}
    R k -> error "badarg in place: k"

--I could use a generic solution here but why be fancy?
byteSize :: LLM -> Int
byteSize llm =
  if head (words $ show llm) `elem`
     words "Load_k CDL_k ML_k MS_k Jump_k Jumpi_k Sub_k"
  then 3
  else 1

--Compile a Code monad to bytes.
--The only possible cause of failure is an unplaced label; the
--first such label encountered is reported.
--Note constants out of the range 0..65535 are silently modulo'd.
runCode :: Code () -> Either Label [Int]
runCode c = do
  let (((),llms),cs) = flip runState CodeS{codeOffset=0,
                                           labelCtr=0,
                                           labelMap=M.empty
                                          } $ runWriterT c
      lmap = labelMap cs
  --Serialize each LLM instruction using labelMap
  concat <$> mapM (serialize lmap) llms
--LLM's iset uses the N5 tag scheme
serialize :: Map Label Int -> LLM -> Either Label [Int]
serialize lmap = \case
  Load_k x -> f 0 x
  CDL_k x -> f 5 x
  ML_k x -> f 10 x
  MS_k x -> f 15 x
  Load_r1 -> g 20
  Load_r2 -> g 25
  Load_r3 -> g 30
  Store_r1 -> g 35
  Store_r2 -> g 40
  Store_r3 -> g 45
  Swap_r1 -> g 50
  Swap_r2 -> g 55
  Swap_r3 -> g 60
  Jump_k x -> f 65 x
  Jumpi_k x -> f 70 x
  Mul_dyn -> g 75
  Sub_k x -> f 80 x
  Return -> g 85
  where
    f op x = (op:) <$> ser x
    g op = return [op]
    ser = \case
      L lab ->
        case M.lookup lab lmap of
          Just k -> return $ serk k
          Nothing -> Left lab
      R k -> return $ serk k
    serk k = [posmod (k `div` 256), posmod k]
    --modulo 256 adjusted to always be positive
    posmod n =
      let m = n `mod` 256
      in if m < 0
         then m + 256
         else m

--Convenience functions for emitting instructions
i0 = instr
i1 f = instr . f
load_k = i1 Load_k
cdl_k = i1 CDL_k
ml_k = i1 ML_k
ms_k = i1 MS_k
data Reg = R1 | R2 | R3
  deriving (Eq,Ord,Read,Show)
load reg = i0 $
  case reg of
    R1 -> Load_r1
    R2 -> Load_r2
    R3 -> Load_r3
store reg = i0 $
  case reg of
    R1 -> Store_r1
    R2 -> Store_r2
    R3 -> Store_r3
swap reg = i0 $
  case reg of
    R1 -> Swap_r1
    R2 -> Swap_r2
    R3 -> Swap_r3
jump_k = i1 Jump_k
jumpi_k = i1 Jumpi_k
mul_dyn = i0 Mul_dyn
sub_k = i1 Sub_k
llm_return = i0 Return

--Note that if I add new instructions to LLM after this, I don't need to
--rewrite much code... unless I extend the max length of instrs.

--The factorial script used in the LLM.evmc demo
facscript = do
  --The argument is placed at the end of the script
  end <- label
  cdl_k end

  fac <- label
  --If !n then return 1 else dowhile
  jumpi_k fac
  --return 1
  load_k 1
  ms_k 0
  load_k 32
  swap R1 --r1's initial value happens to be 0
  llm_return

  place fac
  --n is in accum, rest are 0
  swap R1
  load_k 1
  swap R1
  loop <- label

  place loop
  swap R1
  mul_dyn
  swap R1
  sub_k 1
  jumpi_k loop

  --return accumulator
  swap R1 --now R1 is 0, letting me avoid a load!
  ms_k 0
  load_k 32
  swap R1
  llm_return
  
  place end

toHex :: [Int] -> String
toHex ns = "0x" ++ do
  n <- ns
  map intToDigit [n `div` 16, n `mod` 16]
