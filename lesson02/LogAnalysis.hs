{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
--
parseMessage :: String -> LogMessage
parseMessage line = case (words line) of
        ("I":ts:message) -> LogMessage Info (read ts) (unwords message)
        ("W":ts:message) -> LogMessage Warning (read ts) (unwords message)
        ("E":errorLevel:ts:message) -> LogMessage (Error (read errorLevel)) (read ts) (unwords message)
        message -> Unknown (unwords message)

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

-- Exercise 2
--
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg@(LogMessage _ ts1 _) (Node left node@(LogMessage _ ts2 _) right)
        | ts1 > ts2 = Node left node (insert logMsg right)
        | otherwise = Node (insert logMsg left) node right
insert logMsg Leaf = Node Leaf logMsg Leaf
insert _ tree = tree

-- Exercise 3
--
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs
