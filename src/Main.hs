import System.Environment (getArgs)

import Poli.Format

main :: IO ()
main = getArgs >>= \case
    path : _ -> format path
    _ -> putStrLn "usage: ./poli-format <path>"
