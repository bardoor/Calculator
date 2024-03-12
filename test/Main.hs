module Main where

import Calculator
import Test.Hspec
import Control.Monad (forM_)
import Control.Monad.State (evalState, execState)

describeDiv :: (Fractional a, RealFloat a, Show a, Eq a) => String -> (a -> a -> a) -> [(a, a)] -> [a] -> SpecWith ()
describeDiv suitName func input expected =
    describe suitName $ do
        it ("input: " ++ show input) $ do
            forM_ (zip input expected) $ \((x, y), expected') ->
                if y == 0.0 && isNaN expected'
                then func x y `shouldSatisfy` isNaN
                else func x y `shouldBe` expected'

describeArithmetics :: (Show a, Eq a) => String -> (a -> a -> a) -> [(a, a)] -> [a] -> SpecWith ()
describeArithmetics suitName func input expected =
    describe suitName $ do
        it ("input: " ++ show input) $ do
            let results = map (uncurry func) input
            results `shouldBe` expected

describeMemorySuits :: (Num a, Show a, Eq a) => String -> MemoryState a a -> a -> SpecWith ()
describeMemorySuits description actions expected = 
  describe description $ do
    it "performs actions correctly" $ do
      evalState actions 0 `shouldBe` expected

main :: IO ()
main = hspec $ do
    -- Тест-кейсы для арифметики
    let input = [(0, 0), (0, 1), (5, 10), (3, -7), (-5, -10)]
        addOutput = [0, 1, 15, -4, -15]
        subOutput = [0, -1, -5, 10, 5]
        mulOutput = [0, 0, 50, -21, 50]
        divOutput = [0/0, 0.0, 0.5,-3/7, 0.5]

    describeArithmetics "test add" add' input addOutput
    describeArithmetics "test sub" sub' input subOutput
    describeArithmetics "test mul" mul' input mulOutput
    describeDiv "test div" div' input divOutput

    -- Тест-кейсы для памяти
    describeMemorySuits "Get from empty mem" getNum 0
    describeMemorySuits "Save" (saveNum 42 >> getNum) 42
    describeMemorySuits "Save and add" (saveNum 42 >> addToStored 15 >> getNum) 57
    describeMemorySuits "Save and add negative" (saveNum 42 >> addToStored (-12) >> getNum) 30
    describeMemorySuits "Save and add zero" (saveNum 42 >> addToStored 0 >> getNum) 42
    describeMemorySuits "Save and clear" (saveNum 42 >> clearMem >> getNum) 0

