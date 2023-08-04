module Ultimatum where

import Text.Printf

main :: IO ()
main = do
    putStrLn "Enter the filename:"
    fileName <- getLine
    matrixA <- readDatFile fileName
    putStrLn "Enter the filename:"
    fileName <- getLine
    matrixB <- readDatFile fileName
    let expr = MatrixSum (MatrixVar "A") (MatrixVar "B")
    let result = evaluateMatrixExpr expr [("A", matrixA), ("B", matrixB)]
    print result

readDatFile :: FilePath -> IO [[Double]]
readDatFile filePath = do
    content <- fmap lines $ readFile filePath
    let matrix = map (map read . words) content :: [[Double]]
    return matrix

-- Define the AST for matrix expressions
data MatrixExpr = Scalar Double            -- Scalar value (constant)
                | MatrixVar String         -- Matrix variable ("A", "B")
                | MatrixSum MatrixExpr MatrixExpr  -- Sum of 2 matrix expressions
                | MatrixSubtract MatrixExpr MatrixExpr  -- Subtraction of 2 matrix expressions
                deriving (Show)

-- Function to evaluate the matrix expression
evaluateMatrixExpr :: MatrixExpr -> [(String, [[Double]])] -> Maybe [[Double]]
evaluateMatrixExpr (Scalar x) _ = Just [[x]]  -- Scalar value becomes a 1x1 matrix
evaluateMatrixExpr (MatrixVar name) vars = lookup name vars  -- Look up matrix variable by name in the provided list of matrices
evaluateMatrixExpr (MatrixSum expr1 expr2) vars =
  case (evaluateMatrixExpr expr1 vars, evaluateMatrixExpr expr2 vars) of
    (Just m1, Just m2) -> if dimensionsMatch m1 m2
                            then Just (matrixSum m1 m2)
                            else Nothing
    _ -> Nothing
evaluateMatrixExpr (MatrixSubtract expr1 expr2) vars =
  case (evaluateMatrixExpr expr1 vars, evaluateMatrixExpr expr2 vars) of
    (Just m1, Just m2) -> if dimensionsMatch m1 m2
                            then Just (matrixSubtract m1 m2)
                            else Nothing
    _ -> Nothing

-- Function that help to check if the dimensions of 2 matrices match
dimensionsMatch :: [[Double]] -> [[Double]] -> Bool
dimensionsMatch m1 m2 = length m1 == length m2 && all (\row -> length row == length (head m2)) m1

-- Function that help to compute the sum of 2 matrices
matrixSum :: [[Double]] -> [[Double]] -> [[Double]]
matrixSum = zipWith (zipWith (+))

-- Function that help to compute the subtraction of 2 matrices
matrixSubtract :: [[Double]] -> [[Double]] -> [[Double]]
matrixSubtract = zipWith (zipWith (-))

--matrixC = [[1,2,4], [3,2,5], [4,2,0]]
--matrixD = [[1,2,4], [3,2,5], [4,2,0]]
--expr = MatrixSubtract (MatrixVar "C") (MatrixVar "D")
--evaluateMatrixExpr expr [("C", matrixC), ("D", matrixD)]