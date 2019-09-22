{-# Language GADTs #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
module Main where

import Data.Array as A
import Data.Array.MArray as AM
import Control.Monad (foldM)
import Control.Monad.ST (ST(..))
import Control.Monad.State.Strict (State, state, runState)
import Data.Array.ST as ST
import Data.Functor.Identity (Identity(..))
import Data.Foldable (foldl')

newtype Node a = Node [(a, Int)] deriving Show

type Tape a = ([Node a], Int)

initTape :: Tape a
initTape = ([], 0)

pushTape :: Tape a -> Node a -> Tape a
pushTape (xs, len) x = (x : xs, len + 1)

type TapeState s a = State (Tape s) a

getTape :: TapeState a (Tape a)
getTape = state $ \tape -> (tape, tape)

pushTape' :: Node a -> TapeState a ()
pushTape' x = state $
  \(xs, len) -> ((), (x : xs, len + 1))

data Var a =
  Var
    { value :: a 
    , index :: Int
    } deriving Show

pushNode :: Tape a -> Node a -> (Int, Tape a)
pushNode tape@(_, len) node = (len, pushTape tape node)

pushNode' :: Node a -> TapeState a Int
pushNode' node = state $
  \tape@(_, len) -> (len, pushTape tape node)

createVar :: Tape a -> a -> (Var a, Tape a)
createVar tape value =
  (Var value index, tape')
    where
  (index, tape') = pushNode tape (Node [])

createVar' :: a -> TapeState a (Var a)
createVar' value =
  Var value <$> pushNode' (Node [])

sin_ :: Floating a => Tape a -> Var a -> (Var a, Tape a)
sin_ tape (Var value index) =
  (Var (sin value) index', tape')
    where
  (index', tape') = pushNode tape (Node [(cos value, index)])

sin_' :: Floating a => Var a -> TapeState a (Var a)
sin_' (Var value index) =
  Var (sin value) <$> pushNode' (Node [(cos value, index)])

cos_ :: Floating a => Tape a -> Var a -> (Var a, Tape a)
cos_ tape (Var value index) =
  (Var (cos value) index', tape')
    where
  (index', tape') = pushNode tape (Node [(- sin value, index)])

cos_' :: Floating a => Var a -> TapeState a (Var a)
cos_' (Var value index) =
  Var (cos value) <$> pushNode' (Node [(- sin value, index)])

add_ :: Floating a => Tape a -> Var a -> Var a -> (Var a, Tape a)
add_ tape (Var value1 index1) (Var value2 index2) =
  (Var (value1 + value2) index', tape')
    where
  (index', tape') = pushNode tape (Node [(1, index1), (1, index2)])

add_' :: Num a => Var a -> Var a -> TapeState a (Var a)
add_' (Var value1 index1) (Var value2 index2) =
  Var (value1 + value2) <$> pushNode' (Node [(1, index1), (1, index2)])

mult_ :: Floating a => Tape a -> Var a -> Var a -> (Var a, Tape a)
mult_ tape (Var value1 index1) (Var value2 index2) =
  (Var (value1 * value2) index', tape')
    where
  (index', tape') = pushNode tape (Node [(value2, index1), (value1, index2)])

mult_' :: Num a => Var a -> Var a -> TapeState a (Var a)
mult_' (Var value1 index1) (Var value2 index2) =
  Var (value1 * value2) <$> pushNode' (Node [(value2, index1), (value1, index2)])

modifyArray :: (AM.MArray a e m, Ix i) => (e -> e) -> a i e -> i -> m (a i e)
modifyArray f arr i = do
  value <- f <$> readArray arr i
  writeArray arr i value
  return arr

grad :: forall a. Num a => Tape a -> Var a -> A.Array Int a
grad tape@(nodes, len) (Var value index) =
  ST.runSTArray $ foldl' go initGrads . zip nodes $ reverse [0..(len - 1)]
    where
  initGrads :: ST s (ST.STArray s Int a)
  initGrads = do 
    arr <- AM.newArray (0, len - 1) 0
    AM.writeArray arr index 1
    return arr
  go :: ST s (ST.STArray s Int a) -> (Node a, Int) -> ST s (ST.STArray s Int a)
  go sarr (Node weights, index) = do 
    arr <- sarr
    deriv <- AM.readArray arr index
    foldM (\arr (w, i) -> modifyArray (+ w * deriv) arr i) arr weights

grad' :: Num a => Var a -> TapeState a (A.Array Int a)
grad' var = state $ \tape -> (grad tape var, tape)

run :: IO ()
run =
  let (x, tape) = createVar initTape 0.5 in
  let (y, tape') = createVar tape 4.2 in 
  let (a, tape'') = mult_ tape' x y in
  let (b, tape''') = sin_ tape'' x in
  let (z, tape'''') = add_ tape''' a b in
  let grads = grad tape'''' z in
    do
  print (x, tape)
  print (y, tape')
  print (a, tape'')
  print (b, tape''')
  print (z, tape'''')
  print grads

run' :: TapeState Double (Var Double, Array Int Double)
run' = do
  x <- createVar' 0.5
  y <- createVar' 4.2
  a <- mult_' x y
  b <- sin_' x
  z <- add_' a b
  grads <- grad' z
  return (z, grads)

runResult :: IO ()
runResult = do
  let ((z, grads), tape) = runState run' initTape
  print z
  print grads
  print tape

main :: IO ()
main = runResult
