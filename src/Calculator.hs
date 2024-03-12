module Calculator where

import Control.Monad.State ( State, put, get, modify )

type MemoryState a = State a

-- Арифметические операции
add' :: Num a => a -> a -> a 
add' x y = x + y

sub' :: Num a => a -> a -> a
sub' x y = x - y

div' :: Fractional a => a -> a -> a
div' x y = x / y

mul' :: Num a => a -> a -> a
mul' x y = x * y

saveNum :: Num a => a -> MemoryState a ()
saveNum = put

getNum :: MemoryState a a
getNum = get

clearMem :: Num a => MemoryState a ()
clearMem = put 0

addToStored :: Num a => a -> MemoryState a ()
addToStored x = modify (+ x)