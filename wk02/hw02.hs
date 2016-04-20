{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseText . words

parseText :: [String] -> LogMessage
parseText ("I":t:xs)   = LogMessage Info (read t) (unwords xs)
parseText ("W":t:xs)   = LogMessage Warning (read t) (unwords xs)
parseText ("E":e:t:xs) = LogMessage (Error $ read e) (read t) (unwords xs)
parseText xs           = Unknown $ unwords xs

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMess Leaf = Node Leaf logMess Leaf
insert logMess@(LogMessage _ t _) tree@(Node l x@(LogMessage _ tx _) r)
    | t < tx = Node (insert logMess l) x r
    | t > tx = Node l x (insert logMess r)
    | otherwise = tree
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf lg Leaf) = [lg]
inOrder (Node l lg r) 
  | l == Leaf = lg:inOrder r
  | r == Leaf = inOrder l ++ [lg]
  | otherwise = inOrder l ++ [lg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = [xs | (LogMessage (Error e) _ xs)  <- logs, e >= 50]
